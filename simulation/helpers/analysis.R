AI_independent_analysis <- function(accession_df, thresholded_model_pred,
                                    baseline_econ) {
  dummy_threshold <- 0.5
  
  message("Replacing reader randomly")
  scenario_fun <- AI_independent_replace_random
  scenario_econ <- AI_independent_random_reader_economics 
  scenario_flowchart_data <- independent_random_flowchart_data
  labels <- c("Reader 1", "AI Reader", "Reader 3")
  
  # Individual performance
  individual_perf <- data.frame(confusion_matrix(thresholded_model_pred))
  
  # System wise performance
  system_sim <- scenario_fun(accession_df, thresholded_model_pred, dummy_threshold)
  system_summary <- scenario_summary(system_sim)
  system_performance <- data.frame(system_summary$performance)
  
  # Flowchart
  device <- animate$new(1000, 800, virtual = TRUE)
  independent_flowchart(device, scenario_flowchart_data(system_summary), labels)
  system_flowchart <- rmd_animate(device, options = click_to_loop())
  
  # Economics table
  system_econ <- scenario_econ(system_summary)
  system_econ_table <- diff_baseline(baseline_econ, system_econ)|>
    diff_table("AI reader replacement")
  
  list(
    individual_perf = individual_perf,
    system_summary = system_summary,
    system_performance = system_performance,
    system_flowchart = system_flowchart,
    system_econ = system_econ,
    system_econ_table = system_econ_table
  )
}


AI_band_pass_analysis <- function(accession_df, thr_model_pred, thr_model_pred_2,
                                  baseline_econ) {
  dummy_threshold2 <- rep(0.5, 2)
  
  # System wise performance
  system_sim <- AI_band_pass_screening_ms(accession_df, thr_model_pred, thr_model_pred_2, dummy_threshold2)
  system_summary <- scenario_summary(system_sim)
  system_performance <- data.frame(system_summary$performance)
  
  # Flowchart
  device <- animate$new(1000, 800, virtual = TRUE)
  band_pass_screening_flowchart(device, band_pass_screening_flowchart_data(system_summary))
  system_flowchart <- rmd_animate(device, options = click_to_loop())
  
  # Economics table
  system_econ <- band_pass_screening_economics(system_summary)
  system_econ_table <- diff_baseline(baseline_econ, system_econ)|>
    diff_table("AI band pass screening")
  
  list(
    # individual_perf = individual_perf,
    system_summary = system_summary,
    system_performance = system_performance,
    system_flowchart = system_flowchart,
    system_econ = system_econ,
    system_econ_table = system_econ_table
  )
}
