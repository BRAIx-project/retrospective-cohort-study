# Get the performance of a single reader
reader_performance <- function(df0) {
  df0 |>
    select(episode_id, episode_outcome, individual_recall) |>
    rename(episode_prediction = individual_recall) |>
    confusion_matrix()
}


AI_performance <- function(df0, threshold) {
  df0 |>
    apply_threshold(threshold) |>
    select(episode_id, episode_outcome, episode_prediction) |>
    confusion_matrix()
}


# Find the threshold that matches the reader's performance ---------------------
match_reader <- function(df0, target = "TNR", target_value = 0.95) {
  f <- function(threshold) {
    df2 <- df0 |> apply_threshold(threshold) |> confusion_matrix()
    df2[[target]] - target_value
  }
  uniroot(f, c(0, 1))
}

apply_threshold <- function(df0, threshold) {
  df0$episode_prediction <- (df0$episode_prediction > threshold) * 1.0
  df0
}
