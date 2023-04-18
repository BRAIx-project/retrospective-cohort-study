baseline_flowchart_data <- function(res_0_sum) {
  list(
    n_preread = sum(res_0_sum$summary$count),
    n_reader_1_and_2 = sum(res_0_sum$summary$count),
    n_reader_1_and_2_no_recall = res_0_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-2", episode_prediction == 0),
    n_reader_1_and_2_recall = res_0_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-2", episode_prediction == 1),
    n_reader_3 = res_0_sum$summary_by_outcome |>
      count_total(by == "reader-3"),
    n_reader_3_recall = res_0_sum$summary_by_outcome |>
      count_total(by == "reader-3", episode_prediction == 1),
    n_reader_3_no_recall = res_0_sum$summary_by_outcome |>
      count_total(by == "reader-3", episode_prediction == 0)
  )
}

independent_flowchart_data <- function(res_2a_sum) {
  list(
    n_preread = sum(res_2a_sum$summary$count),
    n_reader_1_and_2 = sum(res_2a_sum$summary$count),
    n_reader_1_and_2_no_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-AI", episode_prediction == 0),
    n_reader_1_and_2_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-AI", episode_prediction == 1),
    n_reader_3 = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3"),
    n_reader_3_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3", episode_prediction == 1),
    n_reader_3_no_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3", episode_prediction == 0)
  )
}


independent_r1_flowchart_data <- function(res_2a_sum) {
  list(
    n_preread = sum(res_2a_sum$summary$count),
    n_reader_1_and_2 = sum(res_2a_sum$summary$count),
    n_reader_1_and_2_no_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "reader-2-and-AI", episode_prediction == 0),
    n_reader_1_and_2_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "reader-2-and-AI", episode_prediction == 1),
    n_reader_3 = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3"),
    n_reader_3_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3", episode_prediction == 1),
    n_reader_3_no_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3", episode_prediction == 0)
  )
}


independent_random_flowchart_data <- function(res_2a_sum) {
  list(
    n_preread = sum(res_2a_sum$summary$count),
    n_reader_1_and_2 = sum(res_2a_sum$summary$count),
    n_reader_1_and_2_no_recall = res_2a_sum$summary_by_outcome |>
      count_total(by %in% c("reader-1-and-AI", "reader-2-and-AI"), episode_prediction == 0),
    n_reader_1_and_2_recall = res_2a_sum$summary_by_outcome |>
      count_total(by %in% c("reader-1-and-AI", "reader-2-and-AI"), episode_prediction == 1),
    n_reader_3 = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3"),
    n_reader_3_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3", episode_prediction == 1),
    n_reader_3_no_recall = res_2a_sum$summary_by_outcome |>
      count_total(by == "sim-reader-3", episode_prediction == 0)
  )
}


band_pass_screening_flowchart_data <- function(res_9_sum) {
  list(
    n_preread = sum(res_9_sum$summary$count),
    
    screening_recall = res_9_sum$summary_by_outcome |>
      count_total(by == "AI-reader", episode_prediction == 1),
    screening_no_recall = res_9_sum$summary_by_outcome |>
      count_total(by == "AI-reader", episode_prediction == 0),
      
    screening_pass = sum(res_9_sum$summary$count) -  
      count_total(res_9_sum$summary_by_outcome, by == "AI-reader"),
    n_reader_1_and_2_recall = res_9_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-2", episode_prediction == 1),
    n_reader_1_and_2_no_recall = res_9_sum$summary_by_outcome |>
      count_total(by == "reader-1-and-2", episode_prediction == 0),
    
    n_reader_3 = res_9_sum$summary |> 
      count_total(by == "reader-3"),
    n_reader_3_recall = res_9_sum$summary_by_outcome |>
      count_total(by == "reader-3", episode_prediction == 1),
    n_reader_3_no_recall = res_9_sum$summary_by_outcome |>
      count_total(by == "reader-3", episode_prediction == 0)
  )
}

