# library(tidyverse)
# library(wordVectors)
# source("arm_word_lists.R")
#
# file_word2vec_model <- "data/GoogleNews-vectors-negative300.bin"
#
# stat_word2vec <- function() {
#   transform <- function(outcome) {
#     model <- read.vectors(file_word2vec_model, nrows = 500000)
#     # expert_vecs <- map(arm_word_lists$architecture, ~ model[[.]])
#     avg_expert_vec <- model[[expert_words$architecture]]
#
#     score_word <- function(word) {
#       cosineSimilarity(avg_expert_vec, model[[word]])
#     }
#
#     df %>%
#       mutate(
#         transformed_outcome =
#           str_split(post_q, pattern = "\\s+") %>%
#           map_dbl(
#             ~ map_dbl(., score_word) %>%
#               mean(na.rm = TRUE)
#           )
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
#   list(
#     transform = transform,
#     combine = combine
#   )
# }
#
# # Options:
# #   Get average vector for post_q, score against words
# #   Look at each word individually, get avg/max **
