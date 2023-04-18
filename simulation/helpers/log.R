#' Save RDS with logging
save_R_object <- function(..., path, filename, msg) {
  # Could extend arguments to handle "prefix", "suffix" and "csv" (bool)
  if (!missing(msg)) message(msg)
  fpath <- file.path(path, paste0(filename, ".rds"))
  message("File written to: ", fpath)
  saveRDS(..., file = fpath)
}


#' Convert an RDS file to a JSON file
#' 
#' @examples 
#' RDS_to_JSON("output/log/dev_reader_weighted_mean.rds")
RDS_to_JSON <- function(path) {
  new_path <- gsub("[.]rds$", ".json", path, ignore.case = TRUE)
  path |>
    readRDS() |>
    jsonlite::write_json(new_path, pretty = TRUE, auto_unbox = TRUE)
  message("File written to: ", new_path)
}


#' Print a message with a separator line
message_heading <- function(msg, symbol = "=") {
  message(paste(rep(symbol, 80), collapse = ""))
  message(msg)
}
