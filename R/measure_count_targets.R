#' Check equality between `token` and tokens in `target`.
#'
#' @param token A character string.
#' @param targets A vector of relevant target tokens.
#' @return A vector of counts (a one-hot encoding).
binary_count <- function(token, targets) {
  as.integer(targets == token)
}

#' Measure free text response as a pooled set of target token counts.
#' @export
measure_count_targets <- function(response, targets) {
  build_measure(
    token = "words",
    token_measure = binary_count,
    token_pool = sum_list
  )(response, targets)
}
