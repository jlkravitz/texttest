#' Make each element in measurement vectors a new column.
#'
#' @param data A data frame with list column `measurement`.
#' @return A new data frame, with measurement vectors spread across multiple
#' columns.
#' @export
spread_measurements <- function(data) {
  data %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::unnest(measurement) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(col_name = seq_along(id)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = col_name, values_from = "measurement") %>%
    dplyr::select(-id)
}
