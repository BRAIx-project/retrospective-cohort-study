#' Load all the csv that contains the readers records
#' 
#' @param folder_path A character string; the path to the reader view folder.
#' @param years A numeric vector; the years to investigate.
#' @return A data frame.
#' 
#' @examples 
#' source("load.R")
#' load_readers(database("readers_v1.1.1"))  # all years
#' load_readers(database("readers_v1.1.1"), c(2016, 2017))
load_readers <- function(folder_path, years) {
  readers_filename <- list.files(folder_path, pattern = "reader-view",
                                 full.names = T)
  if (missing(years)) {
    # keep all years
    result <- readers_filename
  } else {
    # keep only the provided years
    result <- c()
    for (year in years) {
      keep <- grepl(x = readers_filename, pattern = year, fixed = T)
      result <- c(result, readers_filename[keep])
    }  
  }
  
  message("Number of files to be loaded: ", length(result))
  do.call(rbind, lapply(result, \(x) data.table::fread(x, data.table = F)))
}


#' Load all the model predictions
#' 
#' @param folder_path A character string; the path to the reader view folder.
#' @return A list of data frames.
#' 
#' @examples 
#' source("load.R")
#' load_models(database("models_ensemble_9"))
load_models <- function(folder_path, pattern = "predictions") {
  models_filename <- list.files(folder_path, pattern = pattern, 
                                full.names = T)  
  models_names <- models_filename |> 
    gsub(pattern = "predictions_", replacement = "", fixed = T) |>
    gsub(pattern = ".csv", replacement = "", fixed = T)
  
  message("Number of files to be loaded: ", length(models_filename))
  setNames(
    lapply(models_filename, \(x) data.table::fread(x, data.table = F)),
    models_names
  )
}


#' Create an ensemble from a list of models
#'
#' @param models A list of models; the output from `load_models`.
#' @return A data frame.
create_ensemble <- function(models) {
  models_names <- names(models)
  
  ensemble <- models |>
    map(\(df0) df0 |> select(image_data_sha256, image_prediction)) |>
    reduce(\(x, y) left_join(x, y, by = "image_data_sha256")) |>
    set_colnames(c("image_data_sha256", models_names))
  
  mean_image_predictions <- ensemble |> 
    select(-image_data_sha256) |>
    rowMeans()
  
  result <- models[[1]]
  result$image_prediction <- mean_image_predictions
  result
}
