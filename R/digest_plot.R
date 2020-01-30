#' Plot permutation test data returned by `digest`.
#'
#' @param data A data frame, as returned by `digest`. At a minimum, this data
#' frame should have columns `test`, `stat_observed`, `stats_permuted`, and `p_val`.
#' @return A ggplot plot object.
digest_plot <- function(data) {
  histogram_data <-
    data %>%
    select(test, stats_permuted) %>%
    unnest(stats_permuted)

  data %>%
    ggplot() +
    geom_histogram(aes(stats_permuted), data = histogram_data, binwidth = 0.05) +
    geom_vline(aes(xintercept = stat_observed), color = "red") +
    geom_text(aes(label = str_c("p = ", p_val)), x = Inf, y = Inf, vjust = 1.5, hjust = 1.5) +
    facet_wrap(vars(test))
}
