#' Compute cosine similarity between `token` and tokens in `target`.
cosine_similarity <- function(token, targets, model) {
  targets %>%
    purrr::map_dbl(
      ~ cosineSimilarity(model[[.]], model[[token]]) %>%
          replace_na(0)
    )
}

#' Measure free text response as a pooled set of cosine similarity between
#' response words and target words.
#' @export
measure_word2vec_targets <- function(response, targets, model) {
  build_measure(
    token = "words",
    token_measure = cosine_similarity,
    token_pool = sum_list
  )(response, targets, model)
}
