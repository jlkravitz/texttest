#' Check equality between `token` and tokens in `target`.
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
