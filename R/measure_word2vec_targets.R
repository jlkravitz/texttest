#' Compute cosine similarity between `token` and tokens in `target`.
#'
#' @param token A character string.
#' @param targets A vector of relevant target tokens.
#' @return A vector of cosine similarities.
cosine_similarity <- function(token, targets, model) {
  map_dbl(targets, ~ cosineSimilarity(model[[.]], model[[token]]))
}

#' Measure free text response as a pooled set of cosine similarity between
#' response words and target words.
#' @export
measure_word2vec_targets <- function(response, targets, model) {
  build_measure(
    token = "words",
    token_measure = binary_count,
    token_pool = sum_list
  )(response, targets, model)
}
