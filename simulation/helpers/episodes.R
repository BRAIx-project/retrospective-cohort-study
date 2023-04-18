#' Convert the image-unit prediction into episode-unit prediction
#' 
#' @param df0 A data frame; the image predictions.
#' @param by "mean", "max", or "mean-max"; "mean" refers to taking the mean of 
#' all the image predictions, "max" refers to taking the maximum of all the 
#' image predictions, and "mean-max" refers to taking the mean over the image 
#' laterality, then taking the max over the laterality predictions. 
as_episode_df <- function(df0, by = "mean-max") {
  if (by == "mean") {
    df1 <- df0 |>
      group_by(episode_id) |>
      mutate(episode_prediction = mean(image_prediction)) |>
      slice_sample(n = 1) |>
      ungroup()  
  } else if (by == "max") {
    df1 <- df0 |>
      group_by(episode_id) |>
      mutate(episode_prediction = max(image_prediction)) |>
      slice_sample(n = 1) |>
      ungroup()
  } else {
    df1 <- df0 |>
      # mean over images
      group_by(episode_id, image_laterality) |>
      mutate(laterality_prediction = mean(image_prediction)) |>
      slice_sample(n = 1) |>
      ungroup() |>
      # max over laterality
      group_by(episode_id) |>
      mutate(episode_prediction = max(laterality_prediction)) |>
      slice_sample(n = 1) |>
      ungroup()
  }
  df1
}

episode_roc <- function(df0) {
  pROC::roc(1.0 * is_cancer(df0$episode_outcome), 
            df0$episode_prediction,
            direction = "<")  
}

is_cancer <- function(xs) {
  xs %in% c(1, 2)
}
