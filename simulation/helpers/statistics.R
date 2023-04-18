count <- \(x) length(unique(x))
num_episodes <- \(x) count(x$episode_id)


count_total <- function(x, ...) {
  x |> filter(...) |> extract2("count") |> sum()
}


number_of_readers_below_curve <- function(roc, reader_perf_summary,
                                          min_required_reads = 2500) {
  is_under_curve <- function(pairs, curve) {
    pairs <- as.numeric(pairs)
    any(curve[,1] <= pairs[1] & curve[,2] >= pairs[2])
  }
  
  num_datapoints <- length(roc$sensitivities)
  
  xy_curve <- data.frame(x = 1 - roc$specificities, 
                         y = roc$sensitivities) |>
    tail(round(num_datapoints / 4)) |>
    extract(seq(1, round(num_datapoints / 4), length.out = 1000), )
  
  reader_xy <- reader_perf_summary |> 
    filter(n_reads >= min_required_reads) |>
    extract(, c("FPR", "TPR"))
  
  1:nrow(reader_xy) |>
    map_lgl(~is_under_curve(reader_xy[.x, ], xy_curve)) |>
    sum()
}


# Get the performance of all readers
get_reader_performance <- function(reader_perf) {
  readers_df <- reader_perf |> 
    group_by(reader_id) |>
    nest() |>
    mutate(n_reads = sapply(data, nrow))
  
  confusion_table <- readers_df$data |>
    map(~.x |> 
          rename(episode_prediction = individual_recall) |>
          confusion_matrix())  # Note 1
  # Note 1: Human readers only make prediction 0 or 1, so we can use any 
  # threshold in-between. 
  
  confusion_df <- confusion_table |>
    map(as.data.frame) %>%
    do.call(rbind, .)
  
  cbind(readers_df, confusion_df) |> 
    select(-data) |>
    ungroup()
}


get_consensus_performance <- function(reader_perf) {
  reader_perf |>
    rename(episode_prediction = consensus_recall) |>
    confusion_matrix() |>
    as.data.frame()
}


confusion_matrix <- function(episode_df0) {
  stable <- episode_df0 |> 
    mutate(episode_outcome = 1.0 * (episode_outcome %in% c(1,2))) |>
    mutate(episode_prediction = episode_prediction) |>
    group_by(episode_prediction, episode_outcome) |> 
    summarise(count = n(), .groups = "drop")
  
  get_count <- function(pred, outcome) {
    stable |> 
      filter(episode_prediction == pred, episode_outcome == outcome) |>
      extract2("count") |>
      zero_if_empty()
  }
  
  zero_if_empty <- function(x) if (length(x) == 0) 0 else x
  
  res <- list(
    TP = get_count(1, 1), FP = get_count(1, 0),
    FN = get_count(0, 1), TN = get_count(0, 0),
    P = get_count(1, 1) + get_count(0, 1),
    N = get_count(1, 0) + get_count(0, 0)
  )
  res$TPR <- res$TP / max(res$P, 1e-10)
  res$FNR <- 1 - res$TPR
  res$TNR <- res$TN / max(res$N, 1e-10)
  res$FPR <- 1 - res$TNR
  res
}


get_scenario_performance <- function(scenario_fun, accession_df, model_pred,
                                  threshold = seq(0, 1, 0.05)) {
  threshold %>%
    map(~scenario_fun(accession_df, model_pred, .x) |> 
          confusion_matrix() |> 
          as.data.frame()) %>%
    do.call(rbind, .)
}


efficient_boundary <- function(df0) {
  stopifnot(all(c("TPR", "FPR") %in% colnames(df0)))
  
  is_better_than <- function(pt, pt2) {
    (pt$TPR >= pt2$TPR) && (pt$FPR <= pt2$FPR)
  }
  
  some_point_is_better_than <- function(pt, ref_group) {
    1:nrow(ref_group) |>
      map_lgl(~is_better_than(ref_group[.x, ], pt)) |>
      any()
  }
  
  df1 <- df0 |> mutate(is_boundary = FALSE)
  for (i in 1:nrow(df1)) {
    # If there is a better point, then the current point is not at the efficient boundary
    df1$is_boundary[i] <- !some_point_is_better_than(df1[i, ], df1[-i, ])
  }
  df1
}


# Scenario analysis threshold setting and optimisation =========================
top_left_distance <- function(summary) {
  tpr <- summary$performance$TPR
  fpr <- summary$performance$FPR
  sqrt(fpr^2 + (1 - tpr)^2)  # minimise
}

youden <- function(summary) {
  tpr <- summary$performance$TPR
  fpr <- summary$performance$FPR
  tpr + (1 - fpr) - 1 # maximise
}

negate <- \(f) \(...) -f(...) 

negate_youden <- negate(youden)  # minimise

