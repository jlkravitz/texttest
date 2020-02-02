#' Mean difference test statistic.
#' @importFrom rlang .data
#' @export
stat_summed_mean_difference <- function(data) {
  data %>%
    dplyr::mutate_at(dplyr::vars(.data$measurement), purrr::map_dbl, sum) %>%
    dplyr::summarize(
      mean(.data$measurement[.data$trmt]) - mean(.data$measurement[!.data$trmt])
    ) %>%
    dplyr::pull()
}
