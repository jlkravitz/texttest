#' Measure response text as a numeric vector.
#'
#' @param data An arbitrary data frame.
#' @param cols A list of columns in `data` (must be character columns).
#' @param response_measure A function which accepts string and returns a numeric
#' measurement vector.
#' @export
measure <- function(data, cols, response_measure, ...) {
  data %>%
    mutate_at(cols, . %>% map(response_measure, ...))
}
