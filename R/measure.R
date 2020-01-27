#' Measure response text as a numeric vector.
#'
#' @param data An arbitrary data frame.
#' @param cols A list of columns in `data` (must be character columns).
#' @param measure_response A function which accepts a vector of strings and returns
#' a vector of numeric vectors (each its own text measurement).
#' @export
measure <- function(data, cols, measure_response, ...) {
  data %>%
    mutate_at(cols, . %>% map(measure_response, ...))
}
