#' Measure response text as a numeric vector.
#'
#' If `measure`
#'
#' @param data An arbitrary data frame.
#' @param cols A list of columns in `data` (must be character columns).
#' @param response_measure A function which accepts string and returns a numeric
#' measurement vector.
#' @param ... Additional parameters passed to `response_measure`.
#' @return A data frame where each column in `cols` is now a list column of
#' text measurements. If `cols` is only of length 1, the column is renamed
#' `measurement`. Calling `pool` in this case is not necessary, and since `pool`
#' generates a final pooled measurement column called `measurement` for use by
#' `digest`, we rename this column accordingly here.
#' @export
measure <- function(data, cols, response_measure, ...) {
  data <-
    data %>%
    dplyr::mutate_at(cols, purrr::map, response_measure, ...)

  # If `cols` is only of length 1, then pooling measurements together is not
  # required. The end-user will thus not call `pool`, which generates a final
  # pooled measurement column called `measurement`; we rename the column
  # accordingly.
  if (length(cols) == 1) {
    data %>%
      dplyr::rename_at(cols, ~ "measurement")
  } else {
    data
  }
}
