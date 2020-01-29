stat_summed_mean_difference <- function(data) {
  data %>%
    mutate_at(vars(measurement), map_dbl, sum) %>%
    summarize(measurement[arm] - mean(measurement[1 - arm])) %>%
    pull()
}
