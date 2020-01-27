data %>%
  unnest_tokens(...) %>%
  ftt::score() %>%
  ftt::summarize %>%
  df %>%
  ftt::measure(cols = c(post_q), type = "count", word_list = "expert")

score_word <- ifelse(
  is_function(score_fn),
  score_fn,
  scoring_fns[[score_fn]]
)

df %>%
  mutate(score = map_dbl(word, score_word)) %>%
  group_by(id) %>%
  mutate(transformed_outcome = sum(score)) %>%
  nest(data = c(word, score)) %>%
  ungroup()

str_split(post_q, pattern = "\\s+") %>%
  map_dbl(
    ~ map_dbl(., score_word) %>%
      mean(na.rm = TRUE)
  )

score_word <- function(word, expert_words) {
  as.integer(expert_words == word)
}

measure_fn <- function(text_col, expert_words) {
  text_col %>%
    enframe(name = "id", value = "text") %>%
    unnest_tokens(word, text) %>%
    mutate(
      feature_vector = map(word, score_word, expert_words)
    )
}

measure <- function(data, cols, measure_fn, ...) {
  cols <- enquo(cols)


  df %>%
    select(cols) %>%
    mutate(id = row_number()) %>%
    pivot_longer(
      cols,
      values_to = "response"
    ) %>%
    unnest_tokens(word, response) %>%
    mutate(feature_vector = map(word, score_word)) %>%
    group_by(id, arm, response_type) %>%
    summarize(
      transformed_outcome = list(
        feature_vector %>%
          bind_rows() %>%
          group_by(expert_word) %>%
          summarize_at(vars(score), max, na.rm = TRUE) %>%
          mutate(score = if_else(!is.finite(score), 0, score))
      )
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = "response_type",
      values_from = "transformed_outcome"
    ) %>%
    rename(transformed_outcome = post_q)

  # Trying to do most manipulation inside package functions
  data %>%
    select(cols) %>%
    # mutate(id = row_number()) %>%
    mutate_at(vars(cols), measure_fn)  ## seems cleaner than pivoting long then wide again
  #
  #     #
  #     pivot_longer(cols = cols, names_to = "col_name", values_to = "text") %>%
  #     unnest_tokens(word, text) %>%
  #     mutate(score = )
  #     mutate_at(vars(cols), count_words, ...)
}

pool <- function(data, cols, type) {
  message("pool")
  # cols <- enquo(cols)
  # data %>%
  #   mutate_at(vars(cols), )
}

digest <- function(data, type, trmt, ctrl) {
  message("digest")
}


#---

#' Measure free text responses as a pooled set of cosine similarity between
#' response words and target words.
#'
#' @param response A vector of strings to be measured.
#' @param targets A vector of relevant target words.
#' @return A vector of measurements. Each measurement vector is of size `length(targets)`.
#' Its `n`th element is the pooled cosine similarity for word `targets[n]` in
#' the particular response.
measure_word2vec_targets <- function(response, targets, model) {
  compute_similarity <- function(token, targets) {
    map_dbl(targets, ~ cosineSimilarity(model[[.]], model[[token]]))
  }
  response %>%
    split_tokens(token = "words") %>%
    measure_tokens(compute_similarity, targets = targets) %>%
    pool_measured_tokens(pmax_list)
}

#' Measure free text responses as a count of target words.
#'
#' @param response A string to be measured.
#' @param targets A vector of strings to match in response text.
#' @param token A string indicating how to split response (default: "words").
#' See `tidytext::unnest_tokens` documention for all options.
#' @return A vector of measurements. Each measurement vector is of size `length(targets)`.
#' Its `n`th element is an integer indicating the number of times `targets[n]`
#' appears in the particular response.
measure_count_targets <- function(response, targets, token = "words") {
  count_targets <- function(token, targets) {
    as.integer(targets == token)
  }

  response %>%
    split_tokens(token = token) %>%
    measure_tokens(count_targets, targets) %>%
    pool_measured_tokens(sum_list)
}
