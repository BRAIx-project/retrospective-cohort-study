# Find threshold (match weighted mean reader) ----------------------------------
matching_individual_threshold <- function(model_pred, target, target_value) {
  roc <- episode_roc(model_pred)
  roc_df <- data.frame(sensitivities = roc$sensitivities,
                       specificities = roc$specificities,
                       thresholds = roc$thresholds)
  roc_df$is_better <- roc_df[[target]] > target_value
  roc_df |>
    filter(is_better) |>
    arrange(desc(!!sym(target))) |>
    tail(1)
}


# Find threshold (match weighted mean reader per manufacturer) -----------------
matching_manufacturer_threshold <- function(model_pred, target, target_value) {
  default_threshold <- matching_individual_threshold(model_pred, target, 
                                                     target_value)
  
  model_pred_roc_by_algorithm <- model_pred |>
    # mutate_algorithm() |>
    rename(algorithm = manufacturer) |>  # switch to the source
    group_by(algorithm) |>
    nest() |>
    mutate(num_data = map_dbl(data, nrow),
           roc = map(data, function(df0) {
      tryCatch({ episode_roc(df0) }, 
      error = \(e) NA)
    }))
  
  roc_to_threshold <- function(roc) {
    roc_df <- data.frame(sensitivities = roc$sensitivities,
                         specificities = roc$specificities,
                         thresholds = roc$thresholds)
    roc_df$is_better <- roc_df[[target]] > target_value
    roc_df |>
      filter(is_better) |>
      arrange(desc(!!sym(target))) |>
      tail(1)  
  }
  
  result <- model_pred_roc_by_algorithm$roc |>
    lapply(function(roc) {
      if (class(roc) == "roc") roc_to_threshold(roc) else NA 
    }) |>
    setNames(model_pred_roc_by_algorithm$algorithm)
  
  result[["default"]] <- default_threshold
  result
}


# Apply manufacturer specific threshold 
apply_manufacturer_threshold <- function(model_pred, thresholds) {
  model_pred_group_by_algor <- model_pred |>
    # mutate_algorithm() |>
    rename(algorithm = manufacturer) |>  # switch to the source
    group_by(algorithm) |>
    nest()
  
  thresholded_episode_prediction <- Map(
    function(df0, algorithm) {
      threshold_list <- thresholds[[algorithm]]
      # For algorithm that does not exist in the developing test or exists 
      # but have too few samples to match the ROC for matching, we use the
      # default threshold (computed using all data points)
      if ((length(threshold_list) == 1 && is.na(threshold_list)) || 
          is.null(threshold_list)) {
        threshold <- thresholds[["default"]]$thresholds
      } else {
        threshold <- threshold_list$thresholds
      }
      data.frame(episode_id = df0$episode_id,
                 episode_prediction = 1.0 * (df0$episode_prediction > threshold))
    },
    model_pred_group_by_algor$data,
    model_pred_group_by_algor$algorithm)
    
  id_with_thresholded_prediction <- do.call(rbind, thresholded_episode_prediction)
  result_pred <- model_pred
  result_pred$episode_prediction <- NULL
  left_join(result_pred, id_with_thresholded_prediction,
            by = "episode_id")
}
