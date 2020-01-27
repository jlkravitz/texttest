# stat_word_count <- function() {
#   transform <- function(df, word_list = "architecture") {
#     df %>%
#       mutate(
#         transformed_outcome = count_words(post_q, expert_words[[word_list]])
#       )
#   }
#
#   combine <- function(df, trmt, ctrl) {
#     counts <-
#       df %>%
#       group_by(arm) %>%
#       summarize(mean_outcome = mean(transformed_outcome, na.rm = TRUE)) %>%
#       deframe()
#
#     counts[[trmt]] - counts[[ctrl]]
#   }
#
#   count_words <- function(text) {
#     text %>%
#       str_to_lower() %>%
#       str_count(word_list %>% str_c(collapse = "|"))
#   }
#
#   list(
#     transform = transform,
#     combine = combine
#     # stat_fn = mean_diff_post_word_count,
#     # get_labs = get_labs
#   )
# }
# #
# # # Takes the difference in word count for each observation across the pre- and post- response,
# # # then averages the difference across the treatment and control arms.
# # mean_diff_pre_post_word_count_difference <- function(df, word_list, arm1, arm2) {
# #   stat_fn <- function(df) {
# #     counts <-
# #       df %>%
# #       mutate(
# #         pre_count = count_words(pre_q, arm_word_lists[[word_list]]),
# #         post_count = count_words(post_q, arm_word_lists[[word_list]]),
# #         count_diff = post_count - pre_count
# #       ) %>%
# #       group_by(arm) %>%
# #       summarize(mean_word_count_diff = mean(count_diff, na.rm = TRUE)) %>%
# #       deframe()
# #
# #     counts[[arm1]] - counts[[arm2]]
# #   }
# #   df %>%
# #     plot_permutations(stat_fn, num_permutations = 1000) +
# #     labs(
# #       title = str_glue("Mean word count differences under null ({word_list} word list)"),
# #       subtitle = str_glue("Null is that {arm1} and {arm2} language are the same")
# #     )
# # }
#
# # get_labs <- function() {
# #   labs(
# #     title = str_glue(
# #       "Mean word count differences under null ({word_list} word list)"
# #     ),
# #     subtitle = str_glue("Null is that {arm1} and {arm2} language are the same")
# #   )
# # }
