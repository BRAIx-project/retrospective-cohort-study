source("renv/activate.R")
library(BRAIxMOP)
library(dplyr)
source("helpers.R")

# Folder containing
input_path <- "./data/"
output_path <- "./output/"
stopifnot(dir.exists(input_path))
stopifnot(dir.exists(output_path))

output <- \(x = "") file.path(output_path, x)
message("Input path: ", input_path)
message("Output path: ", output_path)




# Setup ========================================================================
# These lines are needed only for the original data. The example data have been
# processed and no further set-up is required.
setup(input_path, output_path, is_test = TRUE)  # set up for test set
setup(input_path, output_path, is_test = FALSE) # set up for dev set


# Match threshold
# - compute reader performance
get_reader_performance_for_matching(
  reader_file = output("reader_df-dev.RDS"),
  output_directory = output_path,
  is_test = FALSE
)

# - needed for the ROC figure for comparison
get_reader_performance_for_matching(
  reader_file = output("reader_df.RDS"),
  output_directory = output_path,
  is_test = TRUE
)

# - compute ai threshold
# Get thresholds for AI reader replacement
get_ai_threshold_and_performance(
  readRDS(output("model_df-dev.RDS")),
  readRDS(output("reader_performance-dev.RDS")),
  output_path, F, sort(c(seq(0, 0.9, 0.02), seq(0.9, 0.99, 0.001)))
)

readRDS(output("AI_performance.RDS"))$threshold |>
  write.csv(output("rr_matched_optimal_youden.csv"),
            row.names = FALSE)

# Get thresholds for AI band-pass
consensus_dev <- readRDS(output("reader_df-dev.RDS")) |>
  filter(reader_number == 1) |>
  get_consensus_performance()

get_ai_threshold_and_performance_bandpass(
  readRDS(output("model_df-dev.RDS")),
  consensus_dev,
  output_path,
  readRDS(output("accession_df-dev.RDS")),
  readRDS(output("reader_df-dev.RDS")),
  seq(0, 0.99, 0.01)
)




#===============================================================================
# Generate reader ROC
tictoc::tic()
reader_roc(
  reader_perf = readRDS(output("reader_df.RDS")),
  model_pred = readRDS(output("model_df.RDS"))
)
tictoc::toc()




#===============================================================================
# Scenario analysis ============================================================
library(BRAIxMOP)

# Main simulation
accession_df <- readRDS(output("accession_df.RDS"))
model_df <- readRDS(output("model_df.RDS"))
reader_df <- readRDS(output("reader_df.RDS"))

ai_performance <- readRDS(output("AI_performance.RDS"))
ai_performance_bandpass <- readRDS(output("AI_performance_bandpass.RDS"))


# AI band-pass scenario --------------------------------------------------------
band_pass_result <- run_simulation(
  run_fun = run_AI_bandpass,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold_1 = ai_performance_bandpass$low_band_threshold,
  threshold_2 = ai_performance_bandpass$high_band_threshold,
  seed = 1234
)
save_RDS(band_pass_result,
         output("simulation_band_pass.RDS"),
         "Simulation result of the band-pass scenario")


# AI replacement scenario ------------------------------------------------------
emp_perf <- get_reader_summary(reader_df)$mean_reader_by_position  # Test performance
emp_tpr_3rd <- emp_perf$weighted_tpr[3]
emp_fpr_3rd <- emp_perf$weighted_fpr[3]
result <- run_simulation(
  run_AI_replacement,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold = ai_performance$threshold,
  seed = 1234,
  custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd)
)

result_n <- run_simulation_bootstrap(
  run_AI_replacement,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold = ai_performance$threshold,
  custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd),
  run_parallel = FALSE,
  mc.cores = 6,
  seed = 1234,
  bootstrap_n = 1000
)
saveRDS(result_n, output("simulation_replacement_original.RDS"))


# MASAI ------------------------------------------------------------------------
masai_threshold <- quantile(model_df$episode_prediction, 0.9)
res_masai <- AI_masai(accession_df, reader_df, model_df, masai_threshold)
saveRDS(res_masai, output("simulation_masai.RDS"))


# Extension - correct x% of Reader 1's decision
# Main
res_masai <- readRDS(output("simulation_masai.RDS"))
corr_prop <- seq(0, 1, 0.01)
masai_correct_df <- masai_correct_tbl(res_masai, corr_prop, seed = 1234)
masai_correct_df


# Matching the baseline
res_baseline <- baseline(accession_df, reader_df)
par <- solve_equivalent(res_masai, res_baseline, seed = 1234)
set.seed(1234)
masai_eq <- masai_correct_perf(res_masai, par$root)
set.seed(1234)
masai_eq_econ <- masai_correct_econ(res_masai, par$root)


# Summary table 1 (performance analysis)
tmp <- rbind(masai_correct_df,
             cbind(corr_prop = "*",
                   data.frame(scenario_summary(res_baseline)$performance)),
             cbind(corr_prop = round(par$root, 3), masai_eq))
tmp[, 8:11] <- round(tmp[, 8:11] * 100 , 1)

masai_tbl_1 <- tmp[, c(1, 8, 10, 2:7)]
masai_tbl_1
write.csv(masai_tbl_1, output("masai_table_1.csv"), row.names = F)


# Summary table 2 (economics analysis)
masai_correct_df2 <- masai_correct_tbl(res_masai, corr_prop, masai_correct_econ)
masai_correct_df2
write.csv(masai_correct_df2, output("masai_table_2.csv"), row.names = F)




#===============================================================================
# Tables and Figures ===========================================================
# Table: Prediction by decile scores =========================================
model_df <- readRDS(output("model_df.RDS"))
qs <- quantile(model_df$episode_prediction, seq(0.1, 1, 0.1))

binned_model_pred <- model_df |>
  mutate(binned_score = case_when(
    episode_prediction <= qs[1] ~ 1,
    episode_prediction <= qs[2] ~ 2,
    episode_prediction <= qs[3] ~ 3,
    episode_prediction <= qs[4] ~ 4,
    episode_prediction <= qs[5] ~ 5,
    episode_prediction <= qs[6] ~ 6,
    episode_prediction <= qs[7] ~ 7,
    episode_prediction <= qs[8] ~ 8,
    episode_prediction <= qs[9] ~ 9,
    episode_prediction <= qs[10] ~ 10,
  ))


# Quantile
qs


# Binned scores
binned_model_pred |>
  select(episode_id, episode_outcome, episode_prediction, binned_score)


binned_counts <- binned_model_pred |>
  group_by(binned_score, episode_outcome) |>
  summarise(count = n()) |>
  group_by(episode_outcome) |>
  mutate(total = sum(count),
         weight = count / total,
         weighted_score = weight * binned_score) |>
  arrange(episode_outcome)


binned_counts |>
  print(n = 100)


table_1_df <- binned_counts |>
  tidyr::pivot_wider(id_cols = binned_score,
                     values_from = count,
                     names_from = episode_outcome)
table_1_df[is.na(table_1_df)] <- 0
table_1_df

for (i in 2:6) {
  col_i <- table_1_df[, i][[1]]
  percent <- round(col_i / sum(col_i) * 100 , 1)
  table_1_df[, i] <- paste(col_i, " (", percent, ")", sep = "")
}
table_1_df
table_1_df <- table_1_df[, c("binned_score", "1", "2", "0", "3", "4")]
colnames(table_1_df) <- c(
  "Binned AI reader scores",
  "Screen-detected cancer",
  "Interval cancer",
  "Normal",
  "Benign",
  "NSA")
table_1_df


table_1_LaTeX <- xtable::xtable(table_1_df)
table_1_LaTeX




# Table: Scenario summary table ================================================
# Baseline, Reader replacement, bandpass and MASAI
res_baseline <- baseline(accession_df, reader_df)
res_baseline_econ <- BRAIxMOP:::baseline_economics(scenario_summary(res_baseline))

res_repl <- readRDS(output("simulation_replacement_original.RDS"))

res_bp <- readRDS(output("simulation_band_pass.RDS"))

res_masai <- readRDS(output("simulation_masai.RDS"))
res_masai_econ <- BRAIxMOP:::masai_economics(scenario_summary(res_masai))

message("Generating LaTeX table")
table_2(
  cbind(baseline_summary_col(res_baseline_econ),
        repl_summary_col(res_repl, res_baseline_econ),
        bp_summary_col(res_bp$system_econ_table),
        masai_summary_col(res_masai_econ, res_baseline_econ))
) |>
  cat()




# Figure: Individual ROC curve =================================================
model_df <- readRDS(output("model_df.RDS"))
reader_df <- readRDS(output("reader_df.RDS"))
accession_df <- readRDS(output("accession_df.RDS"))

# Base plot
par(las = 1)
plot(roc(model_df), type = 'n')

# Readers points
res_reader <- get_reader_performance(reader_df)
point_size <- 100   # use 10 for example data
with(res_reader,
     points(TNR, TPR, cex = point_size * (1 + P + N) / sum(P + N),
            col = 'lightgray'))

# ROC curve
lines(roc(model_df))

# Consensus point
res_baseline <- baseline(accession_df, reader_df)
with(scenario_summary(res_baseline)$performance,
     points(TNR, TPR, pch = 15, col = "red"))

# Weighted reader point
res_weighted <- readRDS(output("reader_performance.RDS"))
with(res_weighted$mean_reader,
     points(1 - weighted_fpr, weighted_tpr, pch = 19))




# Figure 2: Flowcharts =========================================================
cat(flowchart_A(res_baseline))
cat(flowchart_B(res_repl))
cat(flowchart_C(res_bp))
cat(flowchart_D(res_masai))




# Figure 3: Scenario ROC curve =================================================
temp <- list()

pt_size <- 2
plot(roc(model_df), lty = 3, xlim = c(1, 0.6), ylim = c(0.6, 1))
with(res_weighted$mean_reader,
     points(1 - weighted_fpr, weighted_tpr, pch = 19, cex = pt_size))

repl_op <- readRDS(output("AI_performance.RDS"))
with(
  temp$plot_repl_op <- model_df |>
    apply_manufacturer_threshold(repl_op$threshold) |>
    confusion_matrix() |>
    data.frame(),
  points(TNR, TPR, pch = 19, col = "green", cex = pt_size)
)

bp_op <- readRDS(output("AI_performance_bandpass.RDS"))
with(
  temp$plot_bp_op_low <- model_df |>
    apply_manufacturer_threshold(bp_op$low_band_threshold) |>
    confusion_matrix() |>
    data.frame(),
  points(TNR, TPR, pch = 19, col = "purple", cex = pt_size)
)
with(
  temp$plot_bp_op_high <- model_df |>
    apply_manufacturer_threshold(bp_op$high_band_threshold) |>
    confusion_matrix() |>
    data.frame(),
  points(TNR, TPR, pch = 19, col = "purple", cex = pt_size)
)

# Add scenario points
with(temp$plot_repl_perf <- data.frame(t(colMeans(res_repl$performance))),
     points(TNR, TPR, pch = 15, col = "green", cex = pt_size))

with(temp$plot_bp <- res_bp$system_performance,
     points(TNR, TPR, pch = 15, col = "purple", cex = pt_size))

with(temp$plot_consensus <- scenario_summary(res_baseline)$performance,
     points(TNR, TPR, pch = 15, col = "red", cex = pt_size))

with(temp$plot_masai <- scenario_summary(res_masai)$performance,
     points(TNR, TPR, pch = 15, col = "orange", cex = pt_size))

legend("bottomright",
       c("Weighted mean reader", "AI replacement operating point", "AI bandpass operating points",
         "Consensus", "AI replacement scenario", "AI band-pass scenario", "MASAI"),
       pch = c(19, 19, 19, 15, 15, 15, 15),
       col = c("black", "green", "purple",
               "red", "green", "purple", "orange"))


result_df <- data.frame(rbind(
  c("Weighted Mean Reader",
    1 - res_weighted$mean_reader$weighted_fpr,
    res_weighted$mean_reader$weighted_tpr),
  c("Replacement OP",
    temp$plot_repl_op$TNR,
    temp$plot_repl_op$TPR),
  c("Band-pass OP low",
    temp$plot_bp_op_low$TNR,
    temp$plot_bp_op_low$TPR),
  c("Band-pass OP high",
    temp$plot_bp_op_high$TNR,
    temp$plot_bp_op_high$TPR),
  c("Censensus",
    temp$plot_consensus$TNR,
    temp$plot_consensus$TPR),
  c("Replacement",
    temp$plot_repl_perf$TNR,
    temp$plot_repl_perf$TPR),
  c("Band-pass",
    temp$plot_bp$TNR,
    temp$plot_bp$TPR),
  c("MASAI",
    temp$plot_masai$TNR,
    temp$plot_masai$TPR)
))
colnames(result_df) <- c("desc", "TNR", "TPR")
result_df

write.csv(result_df, file = output("scenario_roc.csv"), row.names = FALSE)


# System curve - Replacement, band-pass, MASAI
# MASAI
res_masai_curve <- masai_curve(seq(0, 1, 0.05))
lines(res_masai_curve$benefit_rate.TN, res_masai_curve$benefit_rate.TP,
      col = "orange")
write.csv(res_masai_curve, file = output("curve_masai.csv"), row.names = FALSE)


# Band-pass scenario
res_bp_thr <- get_ai_threshold_and_performance_bandpass(
  readRDS(output("model_df.RDS")),
  consensus_dev,
  NULL,
  readRDS(output("accession_df.RDS")),
  readRDS(output("reader_df.RDS")),
  seq(0, 0.99, 0.02)
)

# Compute efficient boundary
res_bp_curve <- eff(res_bp_thr$search_log)
lines(res_bp_curve$TNR, res_bp_curve$TPR, col = "purple")
lines(c(1, max(res_bp_curve$TNR)), c(0, min(res_bp_curve$TPR)), col = "purple")
write.csv(res_bp_curve, file = output("curve_bandpass.csv"), row.names = FALSE)


# Replacement scenario
specs <- c(seq(0, 0.99, 0.01), seq(0.991, 0.995, 0.001))  # use `specs <- seq(0, 0.99, 0.04)` for example data
res_repl_curve <- data.frame(do.call(rbind, lapply(
  specs,
  function(spec) {
     result <- run_simulation(
       run_AI_replacement,
       accession_df = accession_df,
       reader_df = reader_df,
       model_df = model_df,
       threshold = find_manufacturer_threshold_matching(model_df, "specificities", spec),
       seed = 1234,
       custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd)
     )
     unlist(result$system_econ)
  }
)))

local({
  with(res_repl_curve, {
    TNR <- benefit_rate.TN
    TPR <- benefit_rate.TP
    lines(TNR, TPR, col = "green")
    lines(c(1, max(TNR)), c(0, min(TPR)), col = "green")
    lines(c(0, min(TNR)), c(1, max(TPR)), col = "green")
  })
})
write.csv(res_repl_curve, file = output("curve_replacement.csv"), row.names = FALSE)



# Supplementary Table: scenario details ========================================
supp_table_repl(colMeans(res_repl$econ), res_baseline_econ) |> cat()
supp_table_bp(res_bp$system_econ, res_baseline_econ) |> cat()
supp_table_masai(res_masai_econ, res_baseline_econ) |> cat()


# Supplementary Table: sensitivity analysis ====================================
# Second reader arbiter
second_reader_result <- run_simulation(
  BRAIxMOP:::run_AI_replacement_second_reader_arbiter,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold = ai_performance$threshold,
  seed = 1234,
  custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd)
)

second_reader_result_n <- run_simulation_bootstrap(
  BRAIxMOP:::run_AI_replacement_second_reader_arbiter,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold = ai_performance$threshold,
  custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd),
  run_parallel = FALSE,
  mc.cores = 4,
  seed = 1234,
  bootstrap_n = 1000
)

save_RDS(second_reader_result_n,
         output("simulation_replacement_second_reader_arbiter.RDS"),
         "Simulation result of the replacement scenario with second reader arbiter")


# Mixed third reader
mixed_third_result <- run_simulation(
  BRAIxMOP:::run_AI_replacement_mixed_third_reader,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold = ai_performance$threshold,
  seed = 1234,
  custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd)
)

mixed_third_result_n <- run_simulation_bootstrap(
  BRAIxMOP:::run_AI_replacement_mixed_third_reader,
  accession_df = accession_df,
  reader_df = reader_df,
  model_df = model_df,
  threshold = ai_performance$threshold,
  custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd),
  run_parallel = FALSE,
  mc.cores = 3,
  seed = 1234,
  bootstrap_n = 1000
)

save_RDS(mixed_third_result_n,
         output("simulation_replacement_mixed_third_reader.RDS"),
         "Simulation result of the replacement scenario with mixed simulated third reader")

cat(supp_table_sensitivity_test(res_baseline_econ))


# Numbers/Tables: Hypothesis testing ===========================================
# Testing superiority / non-inferiority of the AI integrative scenario w.r.t. the AI baseline ----
accession_df <- readRDS(output("accession_df.RDS"))
reader_df <- readRDS(output("reader_df.RDS"))
model_df <- readRDS(output("model_df.RDS"))
result_baseline <- baseline(accession_df, reader_df)
result_bp <- readRDS(output("simulation_band_pass.RDS"))
result_rr <- readRDS(output("simulation_replacement_original.RDS"))
result_tr <- readRDS(output("simulation_masai.RDS"))


# Match the median (Youden) case for Reader Replacement
result_rr_median <- local({
  # Find the median (Youden) case
  result_rr_df0 <- result_rr$performance |>
    mutate(idx = 1:1000,
           youden = TPR - FPR,
           youden_check = TPR + TNR - 1)
  yd <- result_rr_df0$youden
  stopifnot(any(yd == quantile(yd, 0.5, type = 1)))

  median_run <- result_rr_df0 |>
    filter(youden == quantile(yd, 0.5, type = 1))
  print(median_run)
  median_run$idx

  # Run the simulation
  set.seed(1234)
  n <- 1000
  multiple_runs_seed <- c(1234, sample(10000000, n - 1))
  median_seed <- multiple_runs_seed[median_run$idx]

  emp_perf <- get_reader_summary(reader_df)$mean_reader_by_position  # Test performance
  emp_tpr_3rd <- emp_perf$weighted_tpr[3]
  emp_fpr_3rd <- emp_perf$weighted_fpr[3]

  result <- run_simulation(
    run_AI_replacement,
    accession_df = accession_df,
    reader_df = reader_df,
    model_df = model_df,
    threshold = readRDS(output("AI_performance.RDS"))$threshold,
    seed = median_seed,
    custom = c(emp_fpr_3rd, emp_tpr_3rd, emp_tpr_3rd, emp_fpr_3rd, emp_fpr_3rd)
  )
  result
})
rr_pair <- pair_scenarios(result_baseline, result_rr_median$system_summary$result)
bp_pair <- pair_scenarios(result_baseline, result_bp$system_summary$result)
tr_pair <- pair_scenarios(result_baseline, result_tr)


# McNemar test
run_test(bp_pair)
run_test(tr_pair)
run_test(rr_pair)




# Testing consistency between client split and prospective splits (both w.r.t. radiologists) ----
# Client split -----------------------------------------------------------------
IS_TEST <- TRUE
model_df <- readRDS(output(ifelse(IS_TEST, "model_df.RDS", "model_df-dev.RDS")))
reader_df <- readRDS(output(ifelse(IS_TEST, "reader_df.RDS", "reader_df-dev.RDS")))
nrow(model_df)
nrow(reader_df)

# This test is for screen-detected cancers only (for comparison with the prospective study) ----
test_client_split_SDC <- test_client_split_against_radiologists_SDC(model_df, reader_df)
with(test_client_split_SDC, {
  message("Testing sensitivity ================================================")
  print(sens_tbl)  # Contingency table
  print(mcnemar.test(sens_tbl, correct = TRUE))  # Null hypothesis: No effect
  # Use entry (2,1) to check AI is superior (and (1,2) to check inferior)
  binom.test(sens_tbl[2,1],
             n = sens_tbl[1,2] + sens_tbl[2,1],
             p = 0.5,
             alternative = "greater") |>
    print()

  message("Testing specificity ================================================")
  print(spec_tbl)  # Contingency table
  print(mcnemar.test(spec_tbl, correct = TRUE))  # Null hypothesis: No effect
  # Use entry (2,1) to check AI is superior (and (1,2) to check inferior)
  binom.test(spec_tbl[2,1],
             n = spec_tbl[1,2] + spec_tbl[2,1],
             p = 0.5,
             alternative = "greater") |>
    print()
})


# Prospective split ------------------------------------------------------------
# (The following only applies to the real data, not example data since there is no "prospective" example data)
prefix <- "retro"  # options: "live", "retro"
prosp_path <- output(sprintf("test-%s-prospective", prefix))
dir.create(prosp_path)
prosp_output <- \(x = "") file.path(prosp_path, x)


# Set-up script
prospective_setup <- function(output_dir, is_test) {
  reader_file <- file.path(input_path, "all-reader-view-1.0.0.csv")
  model_file <- file.path(input_path, "prospective_image_mean_ensemble.csv")
  prepare_data_for_simulation(model_file, reader_file, is_test, output_dir)
}

prospective_setup(prosp_path, TRUE)


IS_TEST <- TRUE
model_df <- readRDS(prosp_output(ifelse(IS_TEST, "model_df.RDS", "model_df-dev.RDS")))
reader_df <- readRDS(prosp_output(ifelse(IS_TEST, "reader_df.RDS", "reader_df-dev.RDS")))
nrow(model_df)
nrow(reader_df)


# McNemar and Binomial tests
test_prosp_result <- test_prospective_split_against_radiologists(model_df, reader_df)
with(test_prosp_result, {
  sens <- sens_tbl
  spec <- spec_tbl

  # Null hypothesis: No effect
  message("Testing sensitivity ================================================")
  print(sens)
  print(mcnemar.test(sens, correct = TRUE))
  # Use entry (2,1) to check AI is superior (and (1,2) to check inferior)
  binom.test(sens[2,1],
             n = sens[1,2] + sens[2,1],
             p = 0.5,
             alternative = "greater") |>
    print()

  # Null hypothesis: No effect
  message("Testing specificity ================================================")
  print(spec)
  print(mcnemar.test(spec, correct = TRUE))
  # Use entry (2,1) to check AI is superior (and (1,2) to check inferior)
  binom.test(spec[2,1],
             n = spec[1,2] + spec[2,1],
             p = 0.5,
             alternative = "greater") |>
    print()
})
