source("load.R")
dev_set <- TRUE
if (dev_set) {
  load("output/reader_perf-dev")
  load("output/model_pred-dev")
  suffix <- "dev_"
} else {
  message("Internal testing")
  load("output/reader_perf")
  load("output/model_pred")
  suffix <- ""
}


# Helper functions specific to this file =====================================
`%+%` <- paste0

# Compute the reader performance in the development set
reader_aggregation <- function(reader_perf) {
  # Check reader file has the correct unit, i.e. episodes --------------------
  local({
    num_eps <- reader_perf |> filter(reader_number == 1) |> nrow()
    message("Checking reader file has the correct unit (episodes): ",
            num_eps == count(reader_perf$episode_id))
    stopifnot(num_eps == count(reader_perf$episode_id))
  })

  # Compute the reader performance in the development set
  dev_episode_id <- model_pred$episode_id

  dev_reader <- reader_perf |>
    filter(reader_number %in% c(1, 2)) |>
    filter(episode_id %in% dev_episode_id)


  # Check the development set dates are correct ------------------------------
  local({
    date_1 <- dev_reader$screening_date |> sort() |> head(1)
    date_2 <- dev_reader$screening_date |> sort() |> tail(1)
    message("Checking the development set dates are correct: ",
            "from ", date_1, " to ", date_2)
  })


  message("Computing the weighted mean of the readers performance (TPR, FPR)")

  dev_reader_perf <- dev_reader |>
    get_reader_performance()

  dev_reader_perf_with_weights <- dev_reader_perf |>
    mutate(weight = n_reads / sum(n_reads)) |>
    select(reader_id, weight, TPR, FPR)

  dev_reader_weighted_mean <- local({
    weight <- dev_reader_perf_with_weights$weight
    tpr <- dev_reader_perf_with_weights$TPR
    fpr <- dev_reader_perf_with_weights$FPR
    list(weighted_TPR = sum(weight * tpr),
         weighted_FPR = sum(weight * fpr),
         weight = weight,
         tpr = tpr,
         fpr = fpr)
  })

  save_R_object(
    object = dev_reader_weighted_mean,
    path = "output/log",
    filename = suffix %+% "reader_weighted_mean",
    msg = "Saving the weighted mean of the readers performance"
  )

  RDS_to_JSON("output/log/" %+% suffix %+% "reader_weighted_mean.rds")


  # Compute the reader performance by position -----------------------------------
  # Readers averaged by positions
  message("Computing the average readers performance by positions")

  reader_perf_by_position <- do.call(
    rbind,
    lapply(1:3, function(i) {
      reader_perf |>
        filter(reader_number == i) |>
        get_reader_performance() |>
        mutate(weights = n_reads / sum(n_reads)) |>
        summarise(wTPR = sum(weights * TPR),
                  wFPR = sum(weights * FPR))
    })
  )

  save_R_object(
    object = reader_perf_by_position,
    path = "output/log",
    filename = suffix %+% "reader_perf_by_position",
    msg = "Save the weight mean of the readers performance by position"
  )

  RDS_to_JSON("output/log/" %+% suffix %+% "reader_perf_by_position.rds")

  message("Complete")
}


# Main =========================================================================
message_heading("Compute the weighted mean of readers")
reader_aggregation(reader_perf)
