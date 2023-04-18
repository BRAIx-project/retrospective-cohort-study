source("load.R")
dev_set <- TRUE
seed <- 1234
if (dev_set) {
  load("output/model_pred-dev", verbose = TRUE)
  load("output/accession_df-dev", verbose = TRUE)
  load("output/reader_perf-dev", verbose = TRUE)
  suffix <- "-dev"
} else {
  message("Internal testing")
  load("output/model_pred", verbose = TRUE)
  load("output/accession_df", verbose = TRUE)
  load("output/reader_perf", verbose = TRUE)
  suffix <- ""
}


# Helper functions specific to this file =====================================
do_call_rbind <- \(x) do.call(rbind, x)
quietly2 <- \(f) \(...) quietly(f)(...)$result
select_row_by_max <- \(x, attr) x[which.max(x[[attr]]),]
select_row_by_min <- \(x, attr) x[which.min(x[[attr]]),]


# Find the two thresholds for the band-pass scenario
find_two_thresholds <- function(model_pred, specificities) {
  s <- specificities
  ms_thresholds <- s |>
    lapply(\(x) matching_manufacturer_threshold(model_pred, "specificities", x)) |>
    set_attr("target_specificity", s)

  # Compute the consensus performance
  res_0 <- baseline(accession_df)
  res_0_summary <- scenario_summary(res_0)
  baseline_econ <- baseline_economics(res_0_summary)

  # Generate the performance table
  perf_df <- ms_thresholds |>
    lapply(\(thr) apply_manufacturer_threshold(model_pred, thr)) |>
    AI_band_pass_pipe(param = list(res_0_summary = res_0_summary,
                                   baseline_econ = baseline_econ),
                      specificities = s)

  # Match the consensus performance
  matched_perf_df <- perf_df |>
    mutate(youden = TPR - FPR,
           DOR = TPR / (1 - TPR) * TNR / (1 - TNR)) |>
    filter(TPR >= res_0_summary$performance$TPR,
           TNR >= res_0_summary$performance$TNR)
  if (nrow(matched_perf_df) == 0) {
    warning("There is no setting where the TPR and TNR are both better than the baseline.")
    matched_perf_df <- perf_df |>
      mutate(youden = TPR - FPR,
             DOR = TPR / (1 - TPR) * TNR / (1 - TNR))
  }

  # Extract the optimal performance
  youden_opt_performance <- select_row_by_max(matched_perf_df, "youden")
  dor_opt_performance <- select_row_by_max(matched_perf_df, "DOR")
  econ_opt_performance <- select_row_by_min(matched_perf_df, "total_cost")
  spec_opt_performance <- select_row_by_max(matched_perf_df, "TNR")

  list(youden_opt_performance = youden_opt_performance,
       dor_opt_performance = dor_opt_performance,
       econ_opt_performance = econ_opt_performance,
       spec_opt_performance = spec_opt_performance,
       full_log = perf_df,
       evaluation_log = matched_perf_df)
}

AI_band_pass_pipe <- function(model_preds, param, specificities) {
  s <- specificities

  l <- length(model_preds)
  stopifnot(l > 1)
  AI_bp_perf <- quietly2(function(accession_df, mj, mi) {
    system_sim <- AI_band_pass_screening_ms(accession_df, mj, mi, rep(0.5, 2))
    system_summary <- scenario_summary(system_sim)
    system_performance <- data.frame(system_summary$performance)
    list(summary = system_summary,
         performance = system_performance)
  })

  result <- vector("list", l * (l - 1) / 2)
  m <- 1
  for (i in 1:(l - 1)) {
    tictoc::tic()
    for (j in (i + 1):l) {
      if (s[j] < 0.9) {
        next
      }
      mi <- model_preds[[i]]
      mj <- model_preds[[j]]
      set.seed(seed)
      # sensitivity order is mj, mi
      # specificity order is mi, mj
      res <- AI_bp_perf(accession_df, mi, mj)
      system_econ <- band_pass_screening_economics(res$summary)

      result[[m]] <- data.frame(i = i, j = j, si = s[i], sj = s[j],
                                res$performance,
                                third_reads = system_econ$cost_drivers$human_readings$reader_3_reads,
                                total_cost = system_econ$cost$total_cost$total)
      m <- m + 1
    }
    pb <- tictoc::toc()
    message(
      "Remaining iteration: ", (l - i),
      ". Estimated remaining time: ", (l - i) * (pb$toc - pb$tic) / 60, " minutes."
    )
  }
  do_call_rbind(result)
}

extract_thresholds <- function(result) {
  perf_labels <- paste0(c("youden", "dor", "econ", "spec"), "_opt_performance")
  thr_labels <- paste0(c("youden", "dor", "econ", "spec"), "_opt_threshold")

  result[thr_labels] <- result[perf_labels] |>
    lapply(function(perf) {
      thr1 <- matching_manufacturer_threshold(model_pred, "specificities", perf$si)
      thr2 <- matching_manufacturer_threshold(model_pred, "specificities", perf$sj)
      list(threshold_1 = thr1, threshold_2 = thr2)
    })
  result
}


# Main ========================================================================
tictoc::tic("Find the threshold for the AI band-pass scenario")
specificities <- c(seq(0, 0.9, 0.1), seq(0.95, 0.99, 0.02)) # O(n^2)
full_result <- find_two_thresholds(model_pred, specificities)
bandpass_system_threshold <- extract_thresholds(full_result)
print("Best threshold for AI band-pass screening")
print(bandpass_system_threshold)
tictoc::toc()


# Write to files
save_R_object(
  bandpass_system_threshold,
  path = "output/log",
  filename = "bandpass_threshold",
  msg = "Threshold matching the consensus performance"
)
