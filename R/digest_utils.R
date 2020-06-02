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
    dplyr::mutate(targets = purrr::map(.data$measurement, fetch_names)) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::unnest(c(.data$measurement, .data$targets)) %>%
    dplyr::rename(target = .data$targets) %>%
    tidyr::pivot_wider(
      names_from = .data$target,
      values_from = "measurement"
    ) %>%
    dplyr::select(-.data$id)
}

fetch_names <- function(measurement) {
  targets <- names(measurement)
  if (is.null(targets)) {
    seq_along(measurement) %>% as.character()
  } else {
    targets
  }
}
