test_that("spread_measurements allows for unnamed lists", {
  data <-
    tibble::tribble(
      ~ measurement, ~ arm,
      c(0, 0, 0),    "control",
      c(1, 0, 1),    "trmt"
    ) %>%
      set_trmt(trmt ~ control)

  expect_error(spread_measurements(data), NA)
})

# # test_that("multiplication works", {
#   df <- readr::read_rds(
#     "../qualitative-measure/data/surveys/strong_curb_ctrl/study_data_83.rds"
#   )
#
#   df_measure_count <-
#     df %>%
#     measure(
#       cols = dplyr::vars(post_q),
#       response_measure = measure_count_targets,
#       targets = arm_word_lists$architecture
#     )
#
#   df_measure_count %>%
#     set_trmt(arch_strong ~ control) %>%
#     stat_classifier() %>%
#     pull(metadata) %>%
#     .[[1]] %>%
#     summary()
#     # dplyr::pull(metadata)
# # })
