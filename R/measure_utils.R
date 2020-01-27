#' Split each response into tokens.
#'
#' Most of the work is done by `unnest_tokens` from the tidytext package.
#' By default, this splits responses into words.
#'
#' @param responses A vector of character strings.
#' @return A tibble with a column indicating the index of the response from
#' which the token originates, along with the token itself.
split_tokens <- function(responses, ...) {
  responses %>%
    enframe(name = NULL, value = "response") %>%
    unnest_tokens(token, response, ...)
}

#' Measure tokens using the given function.
#'
#' @param tokens A tibble of tokens in the format returned by `split_tokens`.
#' @param measure_token A function which accepts a token (along with
#' additional parameters in `...`) and returns a numeric vector.
#' @return A tibble, as returned by `split_tokens`, along with a new column
#' `measurement`, as returned by `measure_token_fn` for each token.
measure_tokens <- function(tokens, token_measure, ...) {
  tokens %>%
    mutate_at(vars(token), list("measurement" = . %>% map(token_measure, ...)))
}

#' Pool token measurements into a single response measurement.
#'
#' @param tokens A tibble of tokens and measurements as returned by `measure_tokens`.
#' @param pool_token_fn A function which accepts a list of token measurements
#' and returns a numeric vector representing a single response measurement.
#' @return A vector of numeric vectors, each representing a pooled measurement
#' for a particular response.
pool_measured_tokens <- function(tokens, token_pool) {
  tokens %>%
    pull(measurement) %>%
    token_pool()
}

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
