# library(tidyverse)
# library(InformationValue)
# library(tidytext)
#
# stat_classifier <- function() {
#
#   # Returns the top `n` words in a given column of the data frame.
#   top_words <- function(df, text_field, n = 5) {
#     text_field <- enquo(text_field)
#     df %>%
#       mutate(
#         words =
#           !!text_field %>%
#           str_to_lower() %>%
#           str_squish() %>%
#           str_remove_all("\\.|,|;") %>%
#           str_split(" ")
#       ) %>%
#       pull(words) %>%
#       flatten_chr() %>%
#       enframe(name = NULL) %>%
#       anti_join(get_stopwords(), by = c("value" = "word")) %>%
#       count(value, sort = TRUE) %>%
#       pull(value) %>%
#       .[1:n]
#   }
#
#   transform <- function(df, word_list = "expert") {
#     word_lists <-
#       list(
#         expert = flatten_chr(arm_word_lists),
#         pre_dist = df %>% top_words(pre_q),
#         post_dist = df %>% top_words(post_q),
#         control = get_stopwords() %>% pull(word)
#       ) %>%
#       map(textstem::lemmatize_words)
#
#     word_count_fns <-
#       word_lists[[word_list]] %>%
#       enframe(name = NULL) %>%
#       mutate(fn = map(value, ~ function(text) {str_count(text, .) > 0})) %>%
#       deframe()
#
#     df %>%
#       mutate_at(vars(post_q), word_count_fns)
#   }
#
#   combine <- function(df, trmt, ctrl) {
#     df <- filter(df, arm %in% c(trmt, ctrl))
#     Concordance(
#       df$arm,
#       predict(
#         suppressWarnings(
#           glm(arm ~ ., data = df, family = binomial(link = "logit"))
#         ),
#         type = "response"
#       )
#     )$Concordance
#   }
#
#   list(
#     transform = transform,
#     combine = combine
#   )
# }
#
# # plot_results <- function(
# #   df, arm1, arm2, word_list, num_permutations = 100, binwidth = 0.01) {
# #   df %>%
# #     plot_permutations(
# #       stat_fn,
# #       num_permutations = num_permutations,
# #       binwidth = binwidth
# #     ) +
# #     labs(
# #       title = str_glue("Model binary word count on response ({arm1} v. {arm2})"),
# #       subtitle = str_glue("Uses {word_list} word list"),
# #       xlab = "Concordance statistic"
# #     )
# # }
