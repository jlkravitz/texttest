# permutation_test <- function(df, stat, ...) {
#     # Closure variables
#   # Instantiated test statistic
#   stat_closure <- stat(df, ...)
#
#   perm_to_df <- function(perm) {
#     perm$data %>%
#       mutate(arm = arm[perm$idx])
#   }
#
#   compute_pval <- function(num_permutations) {
#     stat_observed <- stat_fn(df)
#
#     permuted_stats <-
#       df %>%
#       permute(num_permutations, arm) %>%
#       mutate(
#         stat = map_dbl(
#           perm,
#           compose(
#             stat_closure$combine,
#             stat_closure$transform,
#             perm_to_df
#           )
#         )
#       )
#
#     # TODO: I don't know if I like this approach.
#     list(
#       p =
#         permuted_stats %>%
#         summarize(mean(abs(stat) >= abs(stat_observed))) %>%
#         pull(),
#       metadata = list(permuted_stats = permuted_stats)
#     )
#   }
#
#   plot <- function(compute_out, binwidth = 0.05) {
#     compute_out$metadata$permuted_stats %>%
#       ggplot(aes(stat)) +
#       geom_histogram(binwidth = binwidth) +
#       geom_vline(xintercept = stat_observed, color = "red") +
#       annotate(
#         "text",
#         label = str_c("p = ", p),
#         x = Inf, y = Inf,
#         vjust = 1.5, hjust = 1.5
#       ) +
#       stat_closure$get_labs()
#   }
#
#   list(
#     plot = plot,
#     compute_pval = compute_pval
#   )
# }
