#' Split response into tokens.
#'
#' Most of the work is done by `unnest_tokens` from the tidytext package.
#' By default, this splits the response into words.
#'
#' @param response A vector of character strings.
#' @return A tibble with a column of tokens parsed from `response`.
#' @export
split_tokens <- function(response, ...) {
  response %>%
    enframe(name = NULL, value = "response") %>%
    unnest_tokens(token, response, ...)
}

#' Measure tokens using the given function.
#'
#' @param tokens A tibble of tokens in the format returned by `split_tokens`.
#' @param token_measure A function which accepts a token (along with
#' additional parameters in `...`) and returns a numeric measurement vector.
#' @return A tibble, as returned by `split_tokens`, along with a new column
#' `measurement`, as returned by `token_measure` for each token.
#' @export
measure_tokens <- function(tokens, token_measure, ...) {
  tokens %>%
    mutate(measurement = map(token, token_measure, ...))
}

#' Pool token measurements into a single response measurement.
#'
#' @param tokens A tibble of tokens and measurements as returned by `measure_tokens`.
#' @param token_pool A function which accepts a list of token measurements
#' and returns a numeric vector representing a single response measurement.
#' @return A single pooled response measurement.
#' @export
pool_measured_tokens <- function(tokens, token_pool) {
  tokens %>%
    pull(measurement) %>%
    token_pool()
}

#' Build typical measurement function using split/measure/pool approach.
#'
#' @param token The token to split (default: words). See `tidytext::unnest_tokens`
#' for more options.
#' @param token_measure A token measurement function. See `measure_tokens`.
#' @param token_pool A token measurement pooling function. See `pool_measured_tokens`.
#' @return A function which returns a numeric measurement vector given a text
#' response.
#' @export
build_measure <- function(token, token_measure, token_pool) {
  function(response, targets, ...) {
    response %>%
      split_tokens(token = token) %>%
      measure_tokens(token_measure, targets, ...) %>%
      pool_measured_tokens(token_pool)
  }
}

#' Sum list of numeric vectors.
#'
#' This is a common function for pooling token measurements.
#'
#' @return The sum of all input vectors.
#' @example pool_measured_tokens(tokens, sum_list)
#' @export
sum_list <- function(vectors) {
  reduce(vectors, `+`)
}

#' Take element-wise maximum of list of vectors.
#'
#' This is a common function for pooling token measurements.
#'
#' @return The element-wise maximum of all input vectors.
#' @example pool_measured_tokens(tokens, pmax_list)
#' @export
pmax_list <- function(vectors) {
  reduce(vectors, pmax, na.rm = TRUE)
}
