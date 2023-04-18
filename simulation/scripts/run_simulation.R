source("load.R")
load(file = "output/model_pred", verbose = TRUE)
load(file = "output/reader_perf", verbose = TRUE)
load(file = "output/accession_df", verbose = TRUE)
set_lapply <- function(parallel = TRUE, mc.cores = 2, ...) {
  if (!parallel) return(lapply)
  return(purrr::partial(parallel::mclapply, mc.cores = mc.cores, ...))
}
lapply2 <- set_lapply(TRUE, mc.cores = 10)


# Helper functions specific to this file ======================================
main <- function(model_pred, reader_perf, accession_df, seed = 1234, ...) {
  # Baseline scenario
  res_0 <- baseline(accession_df)
  res_0_summary <- scenario_summary(res_0)
  res_0_econ <- baseline_econ <- baseline_economics(res_0_summary)
  device <- animate$new(1000, 800, virtual = TRUE)
  baseline_flowchart(device, baseline_flowchart_data(res_0_summary))
  res_0_flowchart <- rmd_animate(device, options = click_to_loop())
  baseline_result <- list(data = res_0,
                          summary = res_0_summary,
                          econ = res_0_econ,
                          flowchart = res_0_flowchart)

  # AI replacement scenario
  replacement_threshold <- readRDS("output/log/replacement_threshold.rds")
  replacement_model_pred <- model_pred |>
    apply_manufacturer_threshold(replacement_threshold[["youden_optimal_threshold"]])

  set.seed(seed)
  AI_replacement_opt <- AI_independent_analysis(accession_df, replacement_model_pred, baseline_result$econ)

  # AI band-pass scenario
  bandpass_threshold <- readRDS("output/log/bandpass_threshold.rds")
  bandpass_model_pred_youden_1 <- model_pred |>
    apply_manufacturer_threshold(bandpass_threshold[["youden_opt_threshold"]]$threshold_1)

  bandpass_model_pred_youden_2 <- model_pred |>
    apply_manufacturer_threshold(bandpass_threshold[["youden_opt_threshold"]]$threshold_2)

  set.seed(seed)
  # order is (2,1) for sensitivity and (1,2) for specificity
  AI_bandpass_opt_youden <- AI_band_pass_analysis(accession_df,
                                                  bandpass_model_pred_youden_1,
                                                  bandpass_model_pred_youden_2,
                                                  baseline_result$econ)

  list(
    baseline_result = baseline_result,
    AI_individual = replacement_model_pred,
    AI_replacement_opt = AI_replacement_opt,
    AI_bandpass_opt = list(youden = AI_bandpass_opt_youden)
  )
}

get_flowchart_reads <- function(df0) {
  result <- t(df0$count)
  colnames(result) <- paste(df0$episode_prediction, df0$by, sep = "_")
  result
}

get_outcome_reads <- function(df0) {
  result <- t(df0$count)
  colnames(result) <- paste(df0$episode_prediction,
                            df0$episode_outcome,
                            df0$by,
                            sep = "_")
  result
}


# Main =========================================================================
result <- main(model_pred, reader_perf, accession_df)
saveRDS(result, file = "output/log/scenario_simulation.RDS")


# Run multiple times to generate the bootstrap distribution and CIs
set.seed(1234)
n <- 1000
multiple_runs_seed <- c(1234, sample(10000000, n - 1))
run <- 1
system.time({
  multiple_runs <- multiple_runs_seed |>
  lapply2(function(seed) {
    print(glue::glue("Run: {run} / {n}"))
    run <<- run + 1
    result <- main(model_pred, reader_perf, accession_df, seed = seed)
    list(AI_replacement_opt_system_performance = result$AI_replacement_opt$system_performance,
          AI_replacement_opt_system_econ = result$AI_replacement_opt$system_econ |> unlist(),
          AI_replacement_opt_system_summary = result$AI_replacement_opt$system_summary$summary |>
            get_flowchart_reads(),
          AI_replacement_opt_system_summary_by_outcome = result$AI_replacement_opt$system_summary$summary_by_outcome |>
            get_outcome_reads(),

          AI_bandpass_opt_system_performance = result$AI_bandpass_opt$youden$system_performance,
          AI_bandpass_opt_system_econ = result$AI_bandpass_opt$youden$system_econ |> unlist(),
          AI_bandpass_opt_system_summary = result$AI_bandpass_opt$youden$system_summary$summary |>
            get_flowchart_reads(),
          AI_bandpass_opt_system_summary_by_outcome = result$AI_bandpass_opt$youden$system_summary$summary_by_outcome |>
            get_outcome_reads())
  })
})

# Create and save summary
multiple_runs_summary <- names(multiple_runs[[1]]) |>
  lapply(function(key) {
    multiple_runs |>
      lapply(\(x) x[[key]]) %>%
      do.call(rbind, .)
  }) |>
  set_names(names(multiple_runs[[1]]))
saveRDS(multiple_runs_summary, file = "output/log/scenario_simulation_multiple.RDS")
