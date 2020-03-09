#' Make each element in measurement vectors a new column.
#'
#' This function is helpful for use with R modelling functions, e.g. `lm`.
#'
#' @param data A data frame with list column `measurement`.
#' @return A new data frame, with measurement vectors spread across multiple
#' columns.
#' @importFrom rlang .data
#' @export
spread_measurements <- function(data) {
  data %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::unnest(.data$measurement) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(col_name = seq_along(.data$id)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = .data$col_name,
      values_from = "measurement"
    ) %>%
    dplyr::select(-.data$id)
}
