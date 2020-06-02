# Check equality between `token` and tokens in `targets`.
binary_count <- function(token, targets) {
  indicators <- as.integer(targets == token)
  names(indicators) <- targets
  indicators
}

#' Measure free text response as a pooled set of target token counts.
#' @param response String to measure.
#' @param targets Target words to consider in measuring `response`.
#' @export
measure_count_targets <- function(response, targets) {
  build_measure(
    token = "words",
    token_measure = binary_count,
    token_pool = sum_list
  )(response, targets)
}
