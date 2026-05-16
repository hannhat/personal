simulate_data <- function(groups, time_periods, group_sizes, treatments) {
  group_df <- data.frame(group = seq_len(groups), group_size = group_sizes)
  time_df  <- data.frame(time = seq_len(time_periods))

  data <- dplyr::cross_join(group_df, time_df) |>
    dplyr::arrange(group, time) |>
    dplyr::mutate(treatment = mapply(
      function(g, t) treatments[[g]][t],
      group, time
    ))

  data
}
