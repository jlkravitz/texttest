#' Mean difference test statistic.
#' @export
stat_summed_mean_difference <- function(data) {
  data %>%
    dplyr::mutate_at(dplyr::vars(measurement), purrr::map_dbl, sum) %>%
    dplyr::summarize(mean(measurement[arm]) - mean(measurement[1 - arm])) %>%
    dplyr::pull()
}
