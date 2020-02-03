#' Plot permutation test data returned by `digest`.
#'
#' @param data A data frame, as returned by `digest`. At a minimum, this data
#' frame should have columns `test`, `stat_observed`, `stats_permuted`, and `p_val`.
#' @return A ggplot plot object.
#' @inheritParams ggplot2::geom_histogram
#' @importFrom rlang .data
#' @export
digest_plot <- function(data, binwidth = 0.05) {
  histogram_data <-
    data %>%
    dplyr::select(.data$test, .data$stats_permuted) %>%
    tidyr::unnest(.data$stats_permuted)

  data %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(.data$stats_permuted),
      data = histogram_data,
      binwidth = binwidth
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data$stat_observed),
      color = "red"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = stringr::str_c("p = ", .data$p_val)),
      x = Inf, y = Inf,
      vjust = 1.5, hjust = 1.5
    ) +
    ggplot2::facet_wrap(dplyr::vars(.data$test))
}
