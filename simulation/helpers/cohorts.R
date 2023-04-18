nest_by <- function(df0, groups) {
  df0 |> 
    group_by(across(all_of(groups))) |>
    nest()
}


merge_minority <- function(df0, by) {
  df0_group_counts <- df0 |>
    group_by(across(all_of(by))) |>
    summarise(count = n())
  
  k <- auto_grouping(df0_group_counts$count)
  sorted_by_samples <- df0_group_counts |> arrange(desc(count))
  top_k_groups <- head(sorted_by_samples, k)[[by]]
  # print(top_k_groups)
  
  # Group the groups that are not in the top k into the "Others" group
  new_col <- paste0(by, "_minority_merged")
  df0[[new_col]] <- df0[[by]]
  
  in_top_k <- df0[[new_col]] %in% top_k_groups
  df0[[new_col]][!in_top_k] <- "Others"
  
  df0
}

# Returns the number of groups when the minority groups are combined
auto_grouping <- function(xs) {
  n <- sort(xs)
  cn <- cumsum(n)
  len <- length(cn) - 1
  ind <- which(diff(cn[1:len] <= n[1 + 1:len]) == -1)
  m <- ifelse(length(ind) == 0, 0, min(ind))
  length(cn) - m
}

# Testing 
# auto_grouping(1:10) == 8
# auto_grouping(c(1,3,4,8,9)) == 2
