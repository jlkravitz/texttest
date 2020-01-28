#' Digest response measurements into observed statistic, permuted statistics,
#' and a p-value.
#'
#' @param data A data frame with columns specified by `measure_col` and `arm_col`.
#' @param formula A formula specifying groups to compare. The left-hand side is
#' considered the treatment.
#' @param stat_compute A function which accepts a vector of numeric vectors
#' (measurements) and a vector of treatment assignments ("trmt" or "ctrl") and
#' returns an arbitrary _numeric_ test statistic.
#' @param measure_col The name of the column in `data` which holds the measurements.
#' @param arm_col The name of the column in `data` which holds the treatment assignments.
#' @return A data frame containing the observed and permuted statistics, along with
#' the associated p-value.
digest <- function(
  data, formula, stat_compute,
  measure_col = measurement, arm_col = arm,
  num_permutations = 1000
) {
  measure_col <- enquo(measure_col)
  arm_col <- enquo(arm_col)

  arms_all <- all.vars(formula)
  arms_trmt <- all.vars(formula[[2]])
  data %>%
    filter(!!arm_col %in% arms_all) %>%
    mutate_at(vars(!!arm_col), ~ if_else(. %in% arms_trmt, "trmt", "ctrl")) %>%
    summarize(
      test = format(formula),
      stat_observed = stat_compute(!!measure_col, !!arm_col),
      stats_permuted = list(
        compute_permuted_stats(
          stat_compute, !!measure_col, !!arm_col, num_permutations
        )
      ),
      p_val = map2_dbl(stat_observed, stats_permuted, compute_p_val)
    )
}

#' Compute statistic for permutations of treatment assignment.
compute_permuted_stats <- function(stat_compute, measurements, arms, num_permutations) {
  map_dbl(1:num_permutations, ~ stat_compute(measurements, sample(arms)))
}

#' Compute p-value based on permutation test statistics.
compute_p_val <- function(stat_observed, stats_permuted) {
  num_larger <- length(
    stats_permuted[abs(stats_permuted) >= abs(stat_observed)]
  )
  num_permutations <- length(stats_permuted)

  num_larger / num_permutations
}
