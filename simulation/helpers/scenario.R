# Baseline
baseline <- function(accession_df) {
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  
  reader_1(accession_df) |>
    merge_two_readers(reader_2(accession_df), id = "reader-1-and-2") |>
    reader_3(final_decision = TRUE)
}

scenario_summary <- function(res) {
  # Recall / no recall summary
  summary <- res |> 
    group_by(episode_prediction, by) |> 
    summarise(count = n())
  
  # Recall / no recall summary by sub-categories
  summary_by_outcome <- res |> 
    group_by(episode_prediction, by, episode_outcome) |> 
    summarise(count = n())
  
  # System performance
  performance <- confusion_matrix(res)
  
  list(result = res,
       summary = summary,
       summary_by_outcome = summary_by_outcome,
       performance = performance,
       extra = attr(res, "extra"))  
}


# 2022-10-26 ===================================================================
AI_baseline_check <- function(accession_df, model_pred, threshold) {
  message("Threshold is not used in this scenario.")
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  reader_1(accession_df) |>
    merge_two_readers(reader_2(accession_df), id = "reader-1-and-2") |>
    reader_3(final_decision = TRUE)
}


# 2022-06-06 New scenario ======================================================
# Band-Pass independent = Band Pass + Reader 3
AI_band_pass_independent <- function(accession_df, model_pred, threshold) {
  stopifnot(length(threshold) == 2)
  
  low_band_reader <- AI_reader(model_pred, threshold[1], "AI-reader")
  high_band_reader <- AI_reader(model_pred, threshold[2], "AI-reader")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  first_pass <- low_band_reader(accession_df)
  first_pass_recall <- first_pass |> filter(episode_prediction == 1)
  first_pass_no_recall <- first_pass |> filter(episode_prediction == 0)
  first_pass_no_recall$decided <- TRUE
  
  second_pass <- high_band_reader(first_pass_recall)
  second_pass_recall <- second_pass |> filter(episode_prediction == 1)
  second_pass_no_recall <- second_pass |> filter(episode_prediction == 0)
  second_pass_recall$decided <- TRUE
  
  third_read <- reader_3(second_pass_no_recall, final_decision = TRUE)
  rbind(first_pass_no_recall, second_pass_recall, third_read)
}


# Band-Pass screening = Band Pass + Reader 1,2 + Reader 3
AI_band_pass_screening <- function(accession_df, model_pred, threshold) {
  stopifnot(length(threshold) == 2)
  
  low_band_reader <- AI_reader(model_pred, threshold[1], "AI-reader")
  high_band_reader <- AI_reader(model_pred, threshold[2], "AI-reader")
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  
  first_pass <- low_band_reader(accession_df)
  first_pass_recall <- first_pass |> filter(episode_prediction == 1)
  first_pass_no_recall <- first_pass |> filter(episode_prediction == 0)
  first_pass_no_recall$decided <- TRUE
  
  second_pass <- high_band_reader(first_pass_recall)
  second_pass_recall <- second_pass |> filter(episode_prediction == 1)
  second_pass_no_recall <- second_pass |> filter(episode_prediction == 0)
  second_pass_recall$decided <- TRUE
 
  pass_read <- reader_1(second_pass_no_recall) |>
    merge_two_readers(reader_2(second_pass_no_recall), id = "reader-1-and-2") |>
    reader_3(final_decision = TRUE) 
  
  rbind(first_pass_no_recall, second_pass_recall, pass_read)
}


# Band-pass screening manufacturer specific thresholds
AI_band_pass_screening_ms <- function(accession_df, model_pred, model_pred_2, threshold) {
  stopifnot(length(threshold) == 2)
  
  low_band_reader <- AI_reader(model_pred, threshold[1], "AI-reader")
  high_band_reader <- AI_reader(model_pred_2, threshold[2], "AI-reader")
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  
  first_pass <- low_band_reader(accession_df)
  first_pass_recall <- first_pass |> filter(episode_prediction == 1)
  first_pass_no_recall <- first_pass |> filter(episode_prediction == 0)
  first_pass_no_recall$decided <- TRUE
  
  second_pass <- high_band_reader(first_pass_recall)
  second_pass_recall <- second_pass |> filter(episode_prediction == 1)
  second_pass_no_recall <- second_pass |> filter(episode_prediction == 0)
  second_pass_recall$decided <- TRUE
  
  pass_read <- reader_1(second_pass_no_recall) |>
    merge_two_readers(reader_2(second_pass_no_recall), id = "reader-1-and-2") |>
    reader_3(final_decision = TRUE) 
  
  rbind(first_pass_no_recall, second_pass_recall, pass_read)
}


# AI replaces both reader 2 and 3
AI_replacement_2_and_3 <- function(accession_df, model_pred, threshold, threshold_2) {
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- AI_reader(model_pred, threshold, "AI-reader-2")
  reader_3 <- AI_reader(model_pred, threshold_2, "AI-reader-3")
  
  reader_1(accession_df) |>
    merge_two_readers(reader_2(accession_df), id = "reader-1-and-AI") |>
    reader_3(final_decision = TRUE)
}


#===============================================================================
# Scenario 1a
AI_autonomous_screening_ideal <- function(accession_df, model_pred, threshold = 0.05) {
  reader_0 <- AI_reader(model_pred, threshold, "AI-reader")
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  
  screened_accession <- reader_0(accession_df)
  accession_recall <- screened_accession |> filter(episode_prediction == 1)
  accession_no_recall <- screened_accession |> filter(episode_prediction == 0)
  accession_no_recall$decided <- TRUE
  
  first_read <- reader_1(accession_recall)
  second_read <- reader_2(accession_recall)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-2"),
    final_decision = TRUE
  )
  
  rbind(third_read, accession_no_recall)
}

# Scenario 1b
AI_autonomous_screening_practical <- function(accession_df, model_pred, threshold = 0.05) {
  reader_0 <- AI_reader(model_pred, threshold, "AI-reader")
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  reader_4 <- simulated_reader(reader_perf, 1, "reader-4")
  
  screened_accession <- reader_0(accession_df)
  accession_recall <- screened_accession |> filter(episode_prediction == 1)
  accession_no_recall <- screened_accession |> filter(episode_prediction == 0)
  
  first_read <- reader_1(accession_recall)
  second_read <- reader_2(accession_recall)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-2"),
    final_decision = TRUE
  )
  
  rbind(third_read, reader_4(accession_no_recall, final_decision = TRUE))
}

# Scenario 2a - replace reader 2
AI_independent <- function(accession_df, model_pred, threshold = 0.05) {
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- AI_reader(model_pred, threshold, "AI-reader-2")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  reader_1(accession_df) |>
    merge_two_readers(reader_2(accession_df), id = "reader-1-and-AI") |>
    reader_3(final_decision = TRUE)
}

# Scenario 2b - replace reader 3
AI_independent_replace_3 <- function(accession_df, model_pred, threshold = 0.05) {
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- AI_reader(model_pred, threshold, "AI-reader-3")
  
  reader_1(accession_df) |>
    merge_two_readers(reader_2(accession_df), id = "reader-1-and-2") |>
    reader_3(final_decision = TRUE)
}

# Scenario 2c - replace reader 1
AI_independent_replace_1 <- function(accession_df, model_pred, threshold = 0.05) {
  reader_1 <- AI_reader(model_pred, threshold, "AI-reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  reader_1(accession_df) |>
    merge_two_readers(reader_2(accession_df), id = "reader-2-and-AI") |>
    reader_3(final_decision = TRUE)
}

# Scenario 2d - randomly replace reader 1 or 2
AI_independent_replace_random <- function(accession_df, model_pred, threshold = 0.05) {
  result_1 <- AI_independent_replace_1(accession_df, model_pred, threshold)
  result_2 <- AI_independent(accession_df, model_pred, threshold)
  from_reader_1 <- sample(c(T, F), nrow(result_1), replace = TRUE)
  stopifnot(all(result_1$episode_id == result_2$episode_id))
  rbind(
    result_1[which(from_reader_1), ],
    result_2[which(!from_reader_1), ]
  ) |>
    arrange(episode_id)
}

# Scenario 2e - replace weak readers
AI_independent_replace_random <- function(accession_df, model_pred, threshold = 0.05) {
  result_1 <- AI_independent_replace_1(accession_df, model_pred, threshold)
  result_2 <- AI_independent(accession_df, model_pred, threshold)
  from_reader_1 <- sample(c(T, F), nrow(result_1), replace = TRUE)
  stopifnot(all(result_1$episode_id == result_2$episode_id))
  rbind(
    result_1[which(from_reader_1), ],
    result_2[which(!from_reader_1), ]
  ) |>
    arrange(episode_id)
}


# Scenario 5
AI_final_pass <- function(accession_df, model_pred, threshold = 0.8) {
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  reader_4 <- AI_reader(model_pred, threshold, "AI-reader")
  
  first_read <- reader_1(accession_df)
  second_read <- reader_2(accession_df)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-2")
  )
  
  accession_recall <- third_read |> filter(episode_prediction == 1)
  accession_recall$decided <- TRUE
  accession_no_recall <- third_read |> filter(episode_prediction == 0)
  accession_no_recall$decided <- FALSE
  
  res <- rbind(accession_recall, reader_4(accession_no_recall, final_decision = TRUE))
  attr(res, "extra") <- list(
    n_reader_3 = third_read |> filter(by == "reader-3") |> nrow()
  )
  res
}

# Scenario 6 - AI autonomous screening (ideal) + AI independent ----------------
autonomous_independent <- function(accession_df, model_pred, threshold = c(0.05, 0.05)) {
  stopifnot(length(threshold) == 2)
  reader_0 <- AI_reader(model_pred, threshold[1], "AI-reader-screening")
  
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- AI_reader(model_pred, threshold[2], "AI-reader-independent")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  screened_accession <- reader_0(accession_df)
  accession_recall <- screened_accession |> filter(episode_prediction == 1)
  accession_no_recall <- screened_accession |> filter(episode_prediction == 0)
  accession_no_recall$decided <- TRUE
  
  first_read <- reader_1(accession_recall)
  second_read <- reader_2(accession_recall)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-2"),
    final_decision = TRUE
  )
  
  rbind(third_read, accession_no_recall)
}


# Scenario 7 - AI autonomous screening (ideal) + final pass --------------------
autonomous_final_pass <- function(accession_df, model_pred, threshold = c(0.05, 0.8)) {
  stopifnot(length(threshold) == 2)
  reader_0 <- AI_reader(model_pred, threshold[1], "AI-reader-screening")
  
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- admani_reader(reader_perf, 2, "reader-2")
  reader_3 <- admani_reader(reader_perf, 3, "reader-3")
  
  reader_4 <- AI_reader(model_pred, threshold[2], "AI-reader-final-pass")
  
  screened_accession <- reader_0(accession_df)
  accession_recall <- screened_accession |> filter(episode_prediction == 1)
  accession_no_recall <- screened_accession |> filter(episode_prediction == 0)
  
  first_read <- reader_1(accession_recall)
  second_read <- reader_2(accession_recall)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-2")
  )
  
  autonomous <- rbind(third_read, accession_no_recall)
  accession_recall <- autonomous |> filter(episode_prediction == 1)
  accession_recall$decided <- TRUE
  accession_no_recall <- autonomous |> filter(episode_prediction == 0)
  accession_no_recall$decided <- FALSE
  
  res <- rbind(accession_recall, reader_4(accession_no_recall, final_decision = TRUE))
  
  # Add extra information from the intermediate stages that cannot be recovered 
  # at the end of the simulation.
  attr(res, "extra") <- list(
    screening_recall = screened_accession |> 
      filter(by == "AI-reader-screening", episode_prediction == 1) |> 
      nrow(),
    screening_no_recall = screened_accession |> 
      filter(by == "AI-reader-screening", episode_prediction == 0) |> 
      nrow(),
    n_reader_3 = third_read |> filter(by == "reader-3") |> nrow()
  )
  res
}


# Scenario 8 - AI independent reader 2 + final pass ----------------------------
independent_final_pass <- function(accession_df, model_pred, threshold = c(0.05, 0.8)) {
  stopifnot(length(threshold) == 2)
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- AI_reader(model_pred, threshold[1], "AI-reader-independent")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  reader_4 <- AI_reader(model_pred, threshold[2], "AI-reader-final-pass")
  
  first_read <- reader_1(accession_df)
  second_read <- reader_2(accession_df)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-AI"),
    final_decision = FALSE
  )
  
  accession_recall <- third_read |> filter(episode_prediction == 1)
  accession_recall$decided <- TRUE
  accession_no_recall <- third_read |> filter(episode_prediction == 0)
  accession_no_recall$decided <- FALSE
  
  res <- rbind(accession_recall, reader_4(accession_no_recall, final_decision = TRUE))
  attr(res, "extra") <- list(
    n_reader_3 = third_read |> filter(by == "sim-reader-3") |> nrow()
  )
  res
}


# Scenario 9 - AI autonomous screening (ideal) + AI independent + final pass ----
autonomous_independent_final_pass <- function(accession_df, model_pred, 
                                              threshold = c(0.05, 0.05, 0.8)) {
  stopifnot(length(threshold) == 3)
  reader_0 <- AI_reader(model_pred, threshold[1], "AI-reader-screening")
  
  reader_1 <- admani_reader(reader_perf, 1, "reader-1")
  reader_2 <- AI_reader(model_pred, threshold[2], "AI-reader-independent")
  reader_3 <- simulated_reader(reader_perf, 3, "sim-reader-3")
  
  reader_4 <- AI_reader(model_pred, threshold[3], "AI-reader-final-pass")
  
  screened_accession <- reader_0(accession_df)
  accession_recall <- screened_accession |> filter(episode_prediction == 1)
  accession_no_recall <- screened_accession |> filter(episode_prediction == 0)
  
  first_read <- reader_1(accession_recall)
  second_read <- reader_2(accession_recall)
  third_read <- reader_3(
    merge_two_readers(first_read, second_read, id = "reader-1-and-AI")
  )
  
  autonomous <- rbind(third_read, accession_no_recall)
  accession_recall <- autonomous |> filter(episode_prediction == 1)
  accession_recall$decided <- TRUE
  accession_no_recall <- autonomous |> filter(episode_prediction == 0)
  accession_no_recall$decided <- FALSE
  
  res <- rbind(accession_recall, reader_4(accession_no_recall, final_decision = TRUE))
  
  # Add extra information from the intermediate stages that cannot be recovered 
  # at the end of the simulation.
  attr(res, "extra") <- list(
    screening_recall = screened_accession |> 
      filter(by == "AI-reader-screening", episode_prediction == 1) |> 
      nrow(),
    screening_no_recall = screened_accession |> 
      filter(by == "AI-reader-screening", episode_prediction == 0) |> 
      nrow(),
    n_reader_3 = third_read |> filter(by == "sim-reader-3") |> nrow()
  )
  res
}


# Scenario 9b - AI autonomous screening (ideal) + single AI independent + final pass ----
autonomous_single_independent_final_pass <- function(accession_df, model_pred, 
                                                     threshold = c(0.05, 0.05, 0.8)) {
  stopifnot(length(threshold) == 3)
  reader_0 <- AI_reader(model_pred, threshold[1], "AI-reader-screening")
  reader_2 <- AI_reader(model_pred, threshold[2], "AI-reader-independent")
  reader_4 <- AI_reader(model_pred, threshold[3], "AI-reader-final-pass")
  
  screened_accession <- reader_0(accession_df)
  accession_recall <- screened_accession |> filter(episode_prediction == 1)
  accession_no_recall <- screened_accession |> filter(episode_prediction == 0)
  
  middle_read <- reader_2(accession_recall)
  
  autonomous <- rbind(middle_read, accession_no_recall)
  accession_recall <- autonomous |> filter(episode_prediction == 1)
  accession_recall$decided <- TRUE
  accession_no_recall <- autonomous |> filter(episode_prediction == 0)
  accession_no_recall$decided <- FALSE
  
  res <- rbind(accession_recall, 
               reader_4(accession_no_recall, final_decision = TRUE))
  
  # Add extra information from the intermediate stages that cannot be recovered 
  # at the end of the simulation.
  attr(res, "extra") <- list(
    screening_recall = screened_accession |> 
      filter(by == "AI-reader-screening", episode_prediction == 1) |> 
      nrow(),
    screening_no_recall = screened_accession |> 
      filter(by == "AI-reader-screening", episode_prediction == 0) |> 
      nrow(),
    n_reader = middle_read |> filter(by == "AI-reader-independent") |> nrow()
  )
  res
}
