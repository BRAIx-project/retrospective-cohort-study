# Diff the economics summary of a scenario against the baseline
diff_baseline <- function(baseline, scenario) {
  base <- unlist(baseline)
  target <- unlist(scenario)
  res <- data.frame(
    "Baseline" = base, 
    "Scenario" = target, 
    "delta_number" = target - base, 
    "delta_percentage" = (target - base) / base
  )
  no_diff <- c("episodes", "cancers", "normals",
               "cost_drivers.human_readings.none",
               "cost_drivers.human_readings.single",
               "cost_drivers.human_readings.double",
               "cost_drivers.human_readings.triple",
               "cost_drivers.assessments.necessary",
               "cost_drivers.assessments.unnecessary",
               "cost.unit_cost.human_read",
               "cost.unit_cost.assessment",
               "cost.unit_cost.reader_3")
  res["delta_number"][no_diff, ] <- NA
  res["delta_percentage"][no_diff, ] <- NA
  # res["delta_percentage"][
  #   c("benefit_rate.TP", "benefit_rate.TN",
  #     "harms_rate.FP", "harms_rate.FN"), 
  # ] <- NA
  res
}


diff_table <- function(diff_table, scenario_text) {
  diff_table <- diff_table |>
    t() |>
    as.data.frame() %>%
    cbind(data.frame("row_names" = c(
      "Baseline", "Scenario", "delta #", "delta %"
    )), .)
  
  diff_table |>
    gt(rowname_col = "row_names") |>
    tab_row_group(
      label = scenario_text,
      rows = c("Scenario", "delta #", "delta %")
    ) |>
    tab_spanner_delim(".", columns = everything()) |>
    fmt_percent(
      columns = contains("rate"),
      decimals = 1
    ) |>
    fmt_percent(
      rows = c("delta %"),
      columns = everything(),
      decimals = 1
    ) |>
    fmt_currency(
      columns = c(contains("unit_cost"))
    ) |>
    fmt_currency(
      columns = c(contains("total_cost")),
      decimals = 0
    ) |>
    sub_missing(
      missing_text = "",
      columns = everything()
    ) |>
    row_group_order(groups = c(NA, scenario_text)) |>
    tab_style(
      style = cell_borders(
        sides = c("right"),
        color = "#BBBBBB",
        weight = px(1.5),
        style = "solid"
      ),
      locations = list(
        cells_body(
          columns = c(
            all_of(c("normals", "harms.FP", "harms_rate.FP", 
                     "cost_drivers.human_readings.total_reads", 
                     "cost_drivers.human_readings.reader_3_reads",
                     "cost_drivers.assessments.total",
                     "cost.unit_cost.assessment")),
          ),
          rows = everything()
        ),
        cells_column_spanners(
          spanners = all_of(
            c("harms.FN", "harms_rate.FN",
              "cost_drivers.human_readings.none", 
              "assessments.necessary",
              "cost.unit_cost.human_read")
          )
        ),
        cells_column_labels(
          columns = all_of(
            c("normals", "harms.FP", "harms_rate.FP", 
              "cost_drivers.human_readings.total_reads", 
              "cost_drivers.human_readings.reader_3_reads",
              "cost_drivers.assessments.total",
              "cost.unit_cost.assessment")
          )
        )
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$benefit.TP[3] > 0),
      locations = cells_body(
        columns = benefit.TP,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$benefit.TN[3] > 0),
      locations = cells_body(
        columns = benefit.TN,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$harms.FP[3] < 0),
      locations = cells_body(
        columns = harms.FP,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$harms.FN[3] < 0),
      locations = cells_body(
        columns = harms.FN,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$cost_drivers.human_readings.total_reads[3] < 0),
      locations = cells_body(
        columns = cost_drivers.human_readings.total_reads,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$cost_drivers.assessments.total[3] < 0),
      locations = cells_body(
        columns = cost_drivers.assessments.total,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$cost.total_cost.reading[3] < 0),
      locations = cells_body(
        columns = cost.total_cost.reading,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$cost.total_cost.assessment[3] < 0),
      locations = cells_body(
        columns = cost.total_cost.assessment,
        rows = contains("delta")
      )
    ) |>
    tab_style(
      style = color_by_benefit(diff_table$cost.total_cost.total[3] < 0),
      locations = cells_body(
        columns = cost.total_cost.total,
        rows = contains("delta")
      )
    )
}


color_by_benefit <- function(x) {
  if (x) cell_fill("#00FF00", 0.3) else cell_fill("#FF0000", 0.3)
}


# # Usage
# diff_res <- diff_baseline(res_0_economics, res_1_economics)
# diff_table(diff_res, "Autonomous screening, threshold @ 0.2")


# Generate a png of the economics table
economics_table_png <- function(accession_df, 
                                baseline_economics, 
                                VAR_THRESHOLD, 
                                VAR_FUNCTION,
                                VAR_FUNCTION_ECON, 
                                VAR_FUNCTION_LABEL, 
                                VAR_LABEL, 
                                seed) {
  for (threshold in VAR_THRESHOLD) {
    if (!missing(seed)) {
      set.seed(seed)  
    }
    
    print(threshold)
    
    res_economics <- VAR_FUNCTION(accession_df, threshold) |>
      retain_episode() |>
      scenario_summary() |>
      VAR_FUNCTION_ECON()
    
    temp_table <- diff_table(
      diff_baseline(baseline_economics, res_economics),
      VAR_FUNCTION_LABEL %+% ", threshold @ " %+% paste(threshold, collapse = " - ") 
    ) 
    
    temp_table |>
      gtsave(
        sprintf(
          "LR_analysis/output/tables/%s.png",
          VAR_LABEL %+% "_" %+% gsub("[.]", "p", paste(threshold, collapse = "-"))
        ), 
        vwidth = 3000
      )
  }
}


# Generate a png of the economics table (can refactor with the last function)
economics_table <- function(accession_df, 
                            baseline_economics, 
                            VAR_THRESHOLD, 
                            VAR_FUNCTION,
                            VAR_FUNCTION_ECON, 
                            VAR_FUNCTION_LABEL, 
                            VAR_LABEL, 
                            seed) {
  missing_seed <- missing(seed)
  purrr::map(VAR_THRESHOLD, function(threshold) {
    if (!missing_seed) {
      set.seed(seed)  
    }
    
    print(threshold)
    
    res_economics <- VAR_FUNCTION(accession_df, threshold) |>
      retain_episode() |>
      scenario_summary() |>
      VAR_FUNCTION_ECON()
    
    temp_table <- diff_table(
      diff_baseline(baseline_economics, res_economics),
      VAR_FUNCTION_LABEL %+% ", threshold @ " %+% paste(threshold, collapse = " - ") 
    ) 
  })
}


latex_table <- function(x) {  
  x |>
    cols_hide(columns = all_of(c("episodes", "cancers", "normals"))) |>
    cols_hide(columns = all_of(c(contains("benefit_rate"), 
                                 contains("harms_rate")))) |>
    cols_hide(columns = all_of(c(contains("none"), 
                                 contains("single"),
                                 contains("double"),
                                 contains("triple"),
                                 contains("necessary"),
                                 contains("unit_cost")))) |>
    as_latex() |>
    cat()
}

