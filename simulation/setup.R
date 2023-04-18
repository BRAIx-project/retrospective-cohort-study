#' This file prepares three data frames for the simulation. They are the 
#' ensemble model predictions, the reader performance and the placeholder to be 
#' filled by the simulation system.


# Helpers ======================================================================
handle_command_line_input <- function(default, args) {
  if (length(args) == 0) return(default)
  if (gsub("\\s", "", args[1]) == "--TEST=TRUE") return(TRUE)
  if (gsub("\\s", "", args[1]) == "--TEST=FALSE") return(FALSE)
  stop("Invalid argument. Use --TEST=TRUE or --TEST=FALSE")
}


# Configuration ================================================================
if (interactive()) {
  TEST <- TRUE # Use FALSE for developing set
} else {
  args <- commandArgs(trailingOnly = TRUE)
  TEST <- handle_command_line_input(default = TRUE, args)
}
SUFFIX <- ifelse(TEST, "", "-dev")

`%+%` <- paste0
OUTPUT_ROOT <- "output"
OUTPUT_LOG <- file.path(OUTPUT_ROOT, "log")
OUTPUT_FIGURES <- file.path(OUTPUT_ROOT, "figures")

if (!dir.exists(OUTPUT_ROOT)) {
  message("Directory created: ", OUTPUT_ROOT)
  dir.create(OUTPUT_ROOT)
}
if (!dir.exists(OUTPUT_LOG)) {
  message("Directory created: ", OUTPUT_LOG)
  dir.create(OUTPUT_LOG)
}
if (!dir.exists(OUTPUT_FIGURES)) {
  message("Directory created: ", OUTPUT_FIGURES)
  dir.create(OUTPUT_FIGURES)
}

if (TEST) {
  READER_YEARS <- c(2017, 2018)
  PRED_LABELS <- "test_prediction"
} else {
  # Development set
  READER_YEARS <- c(2016)
  PRED_LABELS <- "dev_prediction"
}


#===============================================================================
# Set up paths to the database on the server
source("load.R")
models_dir <- "/retro/models/outputs"
readers_dir <- "/retro/models/outputs"


# Load the reader_view files
message("=============================================")
message("Setting up for model operating point analysis")
message("=============================================")
message(glue::glue("Running the analysis using the {ifelse(TEST, 'testing', 'development')} set."))
message("Loading the reader files")
readers_file <- ifelse(TEST, "retro-test-reader.csv", "retro-dev-reader.csv")
admani2_reader <- read.csv(file.path(readers_dir, readers_file))

message("Reporting the number of reads")
message("  Total number of reads: ", num_episodes(admani2_reader))
message("  Number of reader 1 reads: ",
        admani2_reader |> filter(reader_number == 1) |> num_episodes())
message("  Number of reader 2 reads: ",
        admani2_reader |> filter(reader_number == 2) |> num_episodes())
message("  Number of reader 3 reads: ",
        admani2_reader |> filter(reader_number == 3) |> num_episodes())


# Load the models prediction
models_file <- ifelse(TEST,
                     "test_predictions.csv",
                     "dev_predictions.csv")
admani2_ensemble <- read.csv(file.path(models_dir, models_file))


#===============================================================================
# Data validation and cleaning
message("Performing data validation and cleaning")


# Match episodes between the model prediction and the reader files (for the simulation study)
message("Ensuring only episodes that appear in both the image-view and the reader-view are inspected.")
message("  Number of episodes in model predictions: ", num_episodes(admani2_ensemble))
message("  Number of episodes in reader records: ", num_episodes(admani2_reader))

common_eid <- intersect(admani2_reader$episode_id, admani2_ensemble$episode_id)
message("  Number of common episodes: ", count(common_eid))

# Keep track of excluded episodes
excluded_eid <- c(setdiff(admani2_reader$episode_id, admani2_ensemble$episode_id),
                  setdiff(admani2_ensemble$episode_id, admani2_reader$episode_id))
if (length(excluded_eid) > 0) {
  excluded_df <- data.frame(episode_id = unique(excluded_eid),
                            reasons = "unmatched_records_between_model_predictions_and_reader_reads")
  log_path <- file.path(OUTPUT_LOG, "excluded_episodes.csv")
  message("Logging excluded episodes to: ", log_path)
  write.csv(excluded_df, file = log_path, row.names = F)
}

message("  Performing filtering")
admani2_reader_matched <- admani2_reader |> filter(episode_id %in% common_eid)
admani2_ensemble_matched <- admani2_ensemble |> filter(episode_id %in% common_eid)
rm(common_eid)

message("  Number of episodes in model predictions: ", num_episodes(admani2_ensemble_matched))
message("  Number of episodes in reader records: ", num_episodes(admani2_reader_matched))
message("  Excluded episodes: ", count(excluded_eid))
stopifnot(num_episodes(admani2_ensemble_matched) == num_episodes(admani2_reader_matched))


# Tidying up the abnormal / ambiguous outcome
message("Episodes with outcome '20' are considered '0' for modelling purposes.")
message("Model files:")
model_pred <- admani2_ensemble_matched
print(table(model_pred$episode_outcome))
model_pred$episode_outcome[model_pred$episode_outcome == 20] <- 0
print(table(model_pred$episode_outcome))

message("Reader files:")
reader_perf <- admani2_reader_matched
print(table(reader_perf$episode_outcome))
reader_perf$episode_outcome[reader_perf$episode_outcome == 20] <- 0
print(table(reader_perf$episode_outcome))


# Converting records into unit of episodes
message("  Number of episodes in model predictions: ", num_episodes(model_pred))
message("  Number of episodes in reader records: ", num_episodes(reader_perf))
stopifnot(num_episodes(model_pred) == num_episodes(reader_perf))
message("Converting the unit in the model file from image to episode")
message("  Number of rows in model file: ", nrow(model_pred))
message("  Number of episodes in model file: ", num_episodes(model_pred))
message("  Performing conversion")
model_pred <- model_pred |> as_episode_df(by = "mean-max")
message("  Number of rows in model file: ", nrow(model_pred))
message("  Number of episodes in model file: ", num_episodes(model_pred))
stopifnot(nrow(model_pred) == num_episodes(model_pred))


# Need to be careful the unit in the reader file is not 'episode' but lesion assessments
message("Converting the unit in the reader file from lesion to episode")
message("  Number of rows in reader records: ", nrow(reader_perf))
message("  Number of records by reader 1: ", reader_perf |> filter(reader_number == 1) |> nrow())
message("  Number of records by reader 2: ", reader_perf |> filter(reader_number == 2) |> nrow())
message("  Number of episodes in reader records: ", num_episodes(reader_perf))

message("  Performing conversion")
reader_perf <- reader_perf |>
  group_by(episode_id, reader_number) |>
  slice_sample(n = 1) |>
  ungroup()
message("  Number of rows in reader records: ", nrow(reader_perf))
message("  Number of records by reader 1: ", reader_perf |> filter(reader_number == 1) |> nrow())
message("  Number of records by reader 2: ", reader_perf |> filter(reader_number == 2) |> nrow())
message("  Number of episodes in reader records: ", num_episodes(reader_perf))
stopifnot(reader_perf |> filter(reader_number == 1) |> nrow() == num_episodes(reader_perf))
stopifnot(reader_perf |> filter(reader_number == 2) |> nrow() == num_episodes(reader_perf))


message("Creating the placeholder data frame for the simulation")
accession_df <- model_pred |>
  select(episode_id, episode_outcome) |>
  mutate(episode_prediction = NA_real_, by = NA_character_, decided = FALSE)


# Save the clean data into files
message("Creating the 'model_pred', 'reader_perf' and 'accession_df' variables")
log_path <- file.path(OUTPUT_ROOT, "model_pred" %+% SUFFIX)
save(model_pred, file = log_path)
message("File written to: ", log_path)

log_path <- file.path(OUTPUT_ROOT, "reader_perf" %+% SUFFIX)
save(reader_perf, file = log_path)
message("File written to: ", log_path)

log_path <- file.path(OUTPUT_ROOT, "accession_df" %+% SUFFIX)
save(accession_df, file = log_path)
message("File written to: ", log_path)


message("Keeping a log of the clean records")
log_path <- file.path(OUTPUT_LOG, "clean_records" %+% SUFFIX %+% ".csv")
write.csv(model_pred, log_path, row.names = F)
message("File written to: ", log_path)

log_path <- file.path(OUTPUT_LOG, "clean_readers" %+% SUFFIX %+% ".csv")
write.csv(reader_perf, log_path, row.names = F)
message("File written to: ", log_path)


message("Setup is complete.")
