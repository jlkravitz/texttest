# Compute cosine similarity between `token` and tokens in `targets`.
cosine_similarity <- function(token, targets, embeddings) {
  targets %>%
    purrr::map_dbl(embeddings$similarity, token)
}

#' Measure free text response as a pooled set of cosine similarity between
#' response words and target words.
#' @param model_file The path to the Magnitude word embedding model file.
#' @inheritParams measure_count_targets
#' @export
measure_word2vec_targets <- function(response, targets, model_file) {
  embeddings <- pymagnitude$Magnitude(model_file)
  build_measure(
    token = "words",
    token_measure = cosine_similarity,
    token_pool = sum_list
  )(response, targets, embeddings)
}
