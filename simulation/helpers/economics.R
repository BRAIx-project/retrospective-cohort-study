# Helpers
economics <- function(res_sum, n_reads) {
  none <- n_reads$none
  single <- n_reads$single
  double <- n_reads$double
  triple <- n_reads$triple
  reader_3 <- n_reads$reader_3
  TP <- res_sum$performance$TP
  FP <- res_sum$performance$FP
  
  human_read_unit_cost <- 9.68
  reader_3_additional_cost <- human_read_unit_cost
  assessment_unit_cost <- 1333
  
  number_reads <- (0 * none + 1 * single + 2 * double + 3 * triple)
  reading_cost <- human_read_unit_cost * number_reads + reader_3 * reader_3_additional_cost
  assessment_cost <- assessment_unit_cost * (TP + FP)
  
  list(
    episodes = sum(res_sum$summary$count),
    cancers = res_sum$performance$P,
    normals = res_sum$performance$N,
    
    benefit = list(TN = res_sum$performance$TN,
                   TP = res_sum$performance$TP),
    harms = list(FN = res_sum$performance$FN, 
                 FP = res_sum$performance$FP),
    
    benefit_rate = list(TN = res_sum$performance$TNR,
                        TP = res_sum$performance$TPR),
    harms_rate = list(FN = res_sum$performance$FNR, 
                      FP = res_sum$performance$FPR),
    
    cost_drivers = list(
      human_readings = list(
        none = none, 
        single = single,
        double = double,
        triple = triple,
        total_reads = number_reads,
        reader_3_reads = reader_3
      ),
      assessments = list(
        necessary = TP,
        unnecessary = FP,
        total = TP + FP
      )
    ),
    cost = list(
      unit_cost = list(
        human_read = human_read_unit_cost,
        reader_3 = human_read_unit_cost + reader_3_additional_cost,
        assessment = assessment_unit_cost
      ),
      total_cost = list(
        reading = reading_cost,
        assessment = assessment_cost,
        total = reading_cost + assessment_cost
      )
    )
  )
}


# Baseline
baseline_economics <- function(res_sum) {
  economics(res_sum, list(
    none = 0,
    single = 0,
    double = res_sum$summary_by_outcome |> 
      count_total(by == "reader-1-and-2"),
    triple = res_sum$summary_by_outcome |> 
      count_total(by == "reader-3"),
    reader_3 = res_sum$summary_by_outcome |> 
      count_total(by == "reader-3")
  ))
}
# # Usage
# res_0 |>
#   retain_episode() |>
#   scenario_summary() |>
#   baseline_economics()

# Scenario band pass screening
band_pass_screening_economics <- function(res_sum) {
  economics(res_sum, list(
    none = res_sum$summary_by_outcome |>
      count_total(by == "AI-reader"),
    single = res_sum$summary_by_outcome |>
      count_total(by == 0),
    double = res_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-2"),
    triple = res_sum$summary_by_outcome |>
      count_total(by == "reader-3"),
    reader_3 = res_sum$summary_by_outcome |>
      count_total(by == "reader-3")
  ))
}

# Scenario 1a
autonomous_ideal_economics <- function(res_sum) {
  economics(res_sum, list(
    none = res_sum$summary_by_outcome |>
      count_total(by == "AI-reader"),
    single = 0,
    double = res_sum$summary_by_outcome |> 
      count_total(by == "reader-1-and-2"),
    triple = res_sum$summary_by_outcome |> 
      count_total(by == "reader-3"),
    reader_3 = res_sum$summary_by_outcome |> 
      count_total(by == "reader-3")
  ))
}

# Scenario 1b
autonomous_practical_economics <- function(res_sum) {
  economics(res_sum, list(
    none = 0, 
    single = res_sum$summary_by_outcome |>
      count_total(by == "reader-4"),
    double = res_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-2"),
    triple = res_sum$summary_by_outcome |>
      count_total(by == "reader-3"),
    reader_3 = res_sum$summary_by_outcome |> 
      count_total(by == "reader-3")
  ))
}

# Scenario 2a
AI_independent_reader_2_economics <- function(res_sum) {
  economics(res_sum, list(
    none = 0, 
    single = res_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-AI"),
    double = res_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3"),
    triple = 0,
    reader_3 = res_sum$summary_by_outcome |> 
      count_total(by == "sim-reader-3")
  ))
}

# Scenario 2b
AI_independent_reader_3_economics <- function(res_sum) {
  economics(res_sum, list(
    none = 0, 
    single = 0,
    double = count_total(res_sum$summary_by_outcome, by == "reader-1-and-2") + 
      count_total(res_sum$summary_by_outcome, by == "AI-reader-3"),
    triple = 0,
    reader_3 = 0
  ))
}

# Scenario 2c
AI_independent_reader_1_economics <- function(res_sum) {
  economics(res_sum, list(
    none = 0, 
    single = res_sum$summary_by_outcome |>
      count_total(by == "reader-2-and-AI"),
    double = res_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3"),
    triple = 0,
    reader_3 = res_sum$summary_by_outcome |> 
      count_total(by == "sim-reader-3")
  ))
}

# Scenario 2d
AI_independent_random_reader_economics <- function(res_sum) {
  economics(res_sum, list(
    none = 0, 
    single = res_sum$summary_by_outcome |>
      count_total(by == "reader-2-and-AI" | by == "reader-1-and-AI"),
    double = res_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3"),
    triple = 0,
    reader_3 = res_sum$summary_by_outcome |> 
      count_total(by == "sim-reader-3")
  ))
}

# Scenario 5
final_pass_economics <- function(res_sum) {
  beginning_total <- sum(res_sum$summary$count)
  third_read <- res_sum$extra$n_reader_3
  economics(res_sum, list(
    none = 0, 
    single = 0,
    double = beginning_total - third_read,
    triple = third_read,
    reader_3 = third_read
  ))
}

# Scenario 6
autonomous_independent_economics <- function(res_sum) {
  summary_outcome <- res_sum$summary_by_outcome
  economics(res_sum, list(
    none = count_total(summary_outcome, by == "AI-reader-screening"), 
    single = count_total(summary_outcome, by == "reader-1-and-2"),
    double = count_total(summary_outcome, by == "sim-reader-3"),
    triple = 0,
    reader_3 = count_total(summary_outcome, by == "sim-reader-3")
  ))
}

# Scenario 7
autonomous_final_pass_economics <- function(res_sum) {
  AI_no_recall <- res_sum$extra$screening_no_recall
  AI_recall <- res_sum$extra$screening_recall
  third_read <- res_sum$extra$n_reader_3
  economics(res_sum, list(
    none = AI_no_recall, 
    single = 0,
    double = AI_recall - third_read,
    triple = third_read,
    reader_3 = third_read
  ))
}

# Scenario 8
independent_final_pass_economics <- function(res_sum) {
  beginning_total <- sum(res_sum$summary$count)
  third_read <- res_sum$extra$n_reader_3
  economics(res_sum, list(
    none = 0, 
    single = beginning_total - third_read,
    double = third_read,
    triple = 0,
    reader_3 = third_read
  ))
}

# Scenario 9
autonomous_independent_final_pass_economics <- function(res_sum) {
  beginning_total <- sum(res_sum$summary$count)
  AI_no_recall <- res_sum$extra$screening_no_recall
  AI_recall <- res_sum$extra$screening_recall
  third_read <- res_sum$extra$n_reader_3
  economics(res_sum, list(
    none = AI_no_recall, 
    single = AI_recall - third_read,
    double = third_read,
    triple = 0,
    reader_3 = third_read
  ))
}
