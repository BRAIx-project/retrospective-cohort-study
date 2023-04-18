# A simulated reader that uses the ground truth as prediction
oracle_reader <- function(model_df, id) {
  required_cols <- c("episode_id", "episode_outcome")
  stopifnot(all(required_cols %in% colnames(model_df)))
  
  function(accession_df, ind, final_decision = FALSE) {
    if (missing(ind)) {
      ind <- which(accession_df$decided == FALSE)
    }
    
    # Threshold the model prediction
    pred_df <- model_df |> 
      mutate(episode_prediction = episode_outcome %in% c(1, 2),
             by = id, 
             decided = final_decision) |>
      select(episode_id, episode_prediction, by, decided)
    
    # Update the original data.frame
    accession_df[ind, ] <- accession_df[ind, ] |>
      select(episode_id, episode_outcome) |>
      left_join(pred_df, by = "episode_id")
    
    accession_df
  }
}


# The AI model using different thresholds for the manufacturers
AI_adaptive_reader <- function(model_df, named_threshold, id) {
  required_cols <- c("episode_id", "episode_prediction")
  stopifnot(all(required_cols %in% colnames(model_df)))
  
  threshold_prediction <- function(df0, threshold) {
    for (manufacturer in names(threshold)) {
      ind <- which(df0$image_manufacturer == manufacturer)
      thres <- threshold[[manufacturer]]
      df0$episode_prediction[ind] <- df0$episode_prediction[ind] > thres
    }
    df0$episode_prediction
  }
  
  function(accession_df, ind, final_decision = FALSE) {
    if (missing(ind)) {
      ind <- which(accession_df$decided == FALSE)
    }
    
    # Threshold the model prediction
    pred_df <- model_df |> 
      mutate(episode_prediction = threshold_prediction(model_df, named_threshold),
             by = id, 
             decided = final_decision) |>
      select(episode_id, episode_prediction, by, decided)
    
    # Update the original data.frame
    accession_df[ind, ] <- accession_df[ind, ] |>
      select(episode_id, episode_outcome) |>
      left_join(pred_df, by = "episode_id")
    
    accession_df
  }
}


# The AI model
AI_reader <- function(model_df, threshold, id) {
  required_cols <- c("episode_id", "episode_prediction")
  stopifnot(all(required_cols %in% colnames(model_df)))

  function(accession_df, ind, final_decision = FALSE) {
    if (missing(ind)) {
      ind <- which(accession_df$decided == FALSE)
    }
    
    # Threshold the model prediction
    pred_df <- model_df |> 
      mutate(episode_prediction = episode_prediction > threshold,
             by = id, 
             decided = final_decision) |>
      select(episode_id, episode_prediction, by, decided)
    
    # Update the original data.frame
    accession_df[ind, ] <- accession_df[ind, ] |>
      select(episode_id, episode_outcome) |>
      left_join(pred_df, by = "episode_id")
    
    accession_df
  }
}


# Readers from the actual database
admani_reader <- function(reader_df, reader_position, id) {
  reader_df <- reader_df |>
    filter(reader_number == reader_position)
  function(accession_df, ind, final_decision = FALSE) {
    if (missing(ind)) {
      ind <- which(accession_df$decided == FALSE)
    }
    
    # Use database entry as prediction
    pred_df <- reader_df |> 
      select(episode_id, individual_recall) |>
      rename(episode_prediction = individual_recall) |>
      mutate(by = id, decided = final_decision)
    
    accession_df[ind, ] <- accession_df[ind, ] |>
      select(episode_id, episode_outcome) |>
      left_join(pred_df, by = "episode_id")
    
    accession_df
  }
}


# A reader simulated based on empirical performance
simulated_reader <- function(reader_df, reader_position, id) {
  reader_emp_perf <- reader_df |>
    filter(reader_number == reader_position) |>
    select(episode_outcome, individual_recall) |>
    group_by(episode_outcome, individual_recall) |>
    summarise(count = n()) |>
    rename(episode_prediction = individual_recall)
  
  probs <- reader_emp_perf |> 
    group_by(episode_outcome) |> 
    mutate(prob = count / sum(count)) |>
    filter(episode_prediction == 1)
  
  probs <- setNames(probs$prob, probs$episode_outcome)
  
  # Make sure all outcomes have empirical probabilities attached to them
  for (outcome_label in as.character(0:4)) {
    if (is.na(probs[outcome_label])) {
      probs[outcome_label] <- 0
    }
  }

  probs['0'] <- 1 - 0.554
  probs['1'] <- 0.959
  probs['2'] <- 0.959
  probs['3'] <- 1 - 0.554
  probs['4'] <- 1 - 0.554
  message(
    switch(probs['2'], 
         `0.2` = "Simulated reader 3 - setting 1",
         `0.3` = "Simulated reader 3 - setting 2",
         `0.4` = "Simulated reader 3 - setting 3",
         `0.959` = "Simulated reader 3 - setting 4",
         "Simulated reader 3 - empirical performance")
  )

  predict_empirical <- Vectorize(function(outcome) {
    p <- probs[[as.character(outcome)]]
    sample(c(0, 1), size = 1, prob = c(1 - p, p))
  })
  
  function(accession_df, ind, final_decision = FALSE) {
    if (missing(ind)) {
      ind <- which(accession_df$decided == FALSE)
    }
    
    # Simulate outcome using the empirical distribution
    accession_df$episode_prediction[ind] <- predict_empirical(accession_df$episode_outcome[ind])
    accession_df$by[ind] <- id
    accession_df$decided[ind] <- final_decision
    accession_df
  }
}


# Merge the results from two readers
# When two readers agree with each other, set the `decided` flag to TRUE.
# Otherwise, set `decided` to FALSE and the episode prediction to be 1 or -1.
# where 1 refers to when reader 1 indicates cancer, when -1 refers to when 
# reader 2 indicates cancer.
merge_two_readers <- function(reader_1_df, reader_2_df, ind, id) {
  if (missing(ind)) {
    ind <- which(reader_1_df$decided == FALSE)
  }
  merged_reader <- reader_1_df
  
  agree <- reader_1_df$episode_prediction[ind] == reader_2_df$episode_prediction[ind]
  merged_reader$by[ind] <- id
  merged_reader[ind, ][which(agree), ]$decided <- TRUE
  
  reader_1_reads <- reader_1_df[ind, ][which(!agree), ]$episode_prediction
  reader_2_reads <- reader_2_df[ind, ][which(!agree), ]$episode_prediction
  merged_reader[ind, ][which(!agree), ]$episode_prediction <- reader_1_reads - reader_2_reads
  merged_reader[ind, ][which(!agree), ]$decided <- FALSE
  
  merged_reader
}
