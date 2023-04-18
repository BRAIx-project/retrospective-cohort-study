source("load.R")
load("output/model_pred-dev", verbose = TRUE)
load("output/accession_df-dev", verbose = TRUE)
load("output/reader_perf-dev", verbose = TRUE)


# Helper functions specific to this file ======================================
# Find the model threshold / operating point using the development set
find_threshold <- function(model_pred, specificities) {
  select_row_by_max <- \(x, attr) x[which.max(x[[attr]]),]

  # Generate the corresponding thresholds for a set of specificities
  s <- specificities
  ms_thresholds <- s |>
    lapply(\(x) matching_manufacturer_threshold(model_pred, "specificities", x)) |>
    set_attr("target_specificity", s)

  # Apply the thresholds to the model predictions
  perf_df <- ms_thresholds |>
    lapply(\(thr) apply_manufacturer_threshold(model_pred, thr)) |>
    lapply(compose(data.frame, confusion_matrix)) |>
    Reduce(f = rbind)

  # Find the threshold that matches the reader performance from the development set
  mean_reader_perf <- readRDS("output/log/dev_reader_weighted_mean.rds")
  matched_perf_df <- data.frame(target = s, perf_df) |>
    filter(TPR >= mean_reader_perf$weighted_TPR,
           TNR >= 1 - mean_reader_perf$weighted_FPR) |>
    mutate(youden = TPR - FPR,
           DOR = TPR / (1 - TPR) * TNR / (1 - TNR))

  # If more than one threshold matches the reader performance, select the one with
  # the highest DOR or Youden's J statistics
  opt_single_dor <- select_row_by_max(matched_perf_df, "DOR")
  opt_single_youden <- select_row_by_max(matched_perf_df, "youden")

  print("Best threshold by Diagnostic Odd Ratios")
  print(opt_single_dor)

  print("Best threshold by Youden's J statistics")
  print(opt_single_youden)

  opt_single_dor_threshold <- ms_thresholds[[which(s == opt_single_dor$target)]]
  opt_single_youden_threshold <- ms_thresholds[[which(s == opt_single_youden$target)]]

  # If no threshold matches the reader performance, return the full tables
  unmatched_perf_df <- data.frame(target = s, perf_df) |>
    mutate(youden = TPR - FPR,
           DOR = TPR / (1 - TPR) * TNR / (1 - TNR))

  list(
    dor_optimal_performance = opt_single_dor,
    dor_optimal_threshold = opt_single_dor_threshold,
    youden_optimal_performance = opt_single_youden,
    youden_optimal_threshold = opt_single_youden_threshold,
    evaluation_log = list(
      constrained = matched_perf_df,
      unconstrained = unmatched_perf_df
    )
  )
}


# Main ========================================================================
message("Find the model threshold / operating point using the development set")
specificities <- c(seq(0, 0.9, 0.02), seq(0.9, 0.99, 0.005)) # O(n)
matched_threshold <- find_threshold(model_pred, specificities)


# Check matched readers are correct in the development set (sanity check)
local({
  png("output/figures/check_matched_readers_on_dev_set.png")
  par(las = 1, family = "DejaVu Sans")
  plot(episode_roc(model_pred), xlim = c(1, 0.5), ylim = c(0.5, 1))
  mean_reader_perf <- readRDS("output/log/dev_reader_weighted_mean.rds")
  points(1 - mean_reader_perf$weighted_FPR,
         mean_reader_perf$weighted_TPR, pch = 4)
  points(matched_threshold$youden_optimal_performance$TNR,
         matched_threshold$youden_optimal_performance$TPR, pch = 19)
  points(matched_threshold$dor_optimal_performance$TNR,
         matched_threshold$dor_optimal_performance$TPR, cex = 2)
  dev.off()
})


# Write to files
save_R_object(
  matched_threshold,
  path = "output/log",
  filename = "replacement_threshold",
  msg = "Threshold matching the mean reader"
)
