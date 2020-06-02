#' Mean difference test statistic.
#' @param data A data frame which contains columns `trmt` and `measurement`.
#' @importFrom rlang .data
#' @export
stat_summed_mean_difference <- function(data) {
  data %>%
    dplyr::mutate_at(dplyr::vars(.data$measurement), purrr::map_dbl, sum) %>%
    dplyr::summarize(
       stat =
         mean(.data$measurement[.data$trmt]) -
          mean(.data$measurement[!.data$trmt]),
       metadata = list(.data$measurement)
    )
}
