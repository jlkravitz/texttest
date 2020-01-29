#' Digest response measurements into observed statistic, permuted statistics,
#' and a p-value.
#'
#' @param data A data frame with columns specified by `measure_col` and `arm_col`.
#' @param formula A formula specifying groups to compare. The left-hand side is
#' considered the treatment.
#' @param stat_compute A function which accepts a vector of numeric vectors
#' (measurements) and a vector of treatment assignments ("trmt" or "ctrl") and
#' returns an arbitrary _numeric_ test statistic.
#' @return A data frame containing the observed and permuted statistics, along with
#' the associated p-value.
digest <- function(data, formula, stat_compute, num_permutations = 1000) {
  data %>%
    set_arms(formula) %>%
    summarize(
      test = format(formula),
      stat_observed = stat_compute(.),
      stats_permuted = list(
        compute_permuted_stats(
          ., stat_compute, num_permutations
        )
      ),
      p_val = map2_dbl(stat_observed, stats_permuted, compute_p_val)
    )
}

set_arms <- function(data, formula) {
  arms_all <- all.vars(formula)
  arms_trmt <- all.vars(formula[[2]])
  arms_ctrl <- all.vars(formula[[3]])
  arms_both <- intersect(arms_trmt, arms_ctrl)
  if (length(arms_both) > 0) {
    warning("Trial arm in both treatment and control; assigning to treatment.")
  }

  data %>%
    filter(arm %in% arms_all) %>%
    mutate_at(vars(arm), ~ as.integer(. %in% arms_trmt))
}

#' Compute statistic for permutations of treatment assignment.
compute_permuted_stats <- function(data, stat_compute, num_permutations = 1000) {
  map_dbl(
    1:num_permutations,
    ~ data %>%
      mutate_at(vars(arm), sample) %>%
      stat_compute()
  )
}

#' Compute p-value based on permutation test statistics.
compute_p_val <- function(stat_observed, stats_permuted) {
  num_larger <- length(
    stats_permuted[abs(stats_permuted) >= abs(stat_observed)]
  )
  num_permutations <- length(stats_permuted)

  num_larger / num_permutations
}
