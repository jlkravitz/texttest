#' Digest response measurements into observed statistic, permuted statistics,
#' and a p-value.
#'
#' @param data A data frame with columns `arm` and `measurement`. The column
#' `arm` should be a character column, specifying which treatment arm the
#' particular observation is in. The column `measurement` should be a list
#' column, and each measurement should be a numeric vector.
#' @param test_formula A formula specifying treatment arms to compare. The left-hand
#' side is considered the treatment, the right-hand side the control.
#' @param stat_compute A function which accepts a data frame (with the same
#' specifications as those of `data`) and returns an arbitrary _numeric_ test
#' statistic.
#' @return A data frame containing the observed and permuted statistics, along with
#' their associated p-value.
#' @export
digest <- function(data, test_formula, stat_compute, num_permutations = 1000) {
  if (is.list(test_formula)) {
    purrr::map_dfr(test_formula, digest_, data, stat_compute, num_permutations)
  } else {
    digest_(test_formula, data, stat_compute, num_permutations)
  }
}

#' @importFrom rlang .data
digest_ <- function(test_formula, data, stat_compute, num_permutations) {
  data <- set_trmt(data, test_formula)

  tibble(
    test = format(test_formula),
    stat_observed = stat_compute(data),
    stats_permuted =
      compute_permuted_stats(data, stat_compute, num_permutations) %>%
        list(),
    p_val = compute_p_val(stat_observed, stats_permuted[[1]])
  )
}

#' Filters data down to treatment arms specified in `test_formula` and sets
#' column `trmt` to a logical, specifying if the observation is from the treatment
#' or control group.
#' @importFrom rlang .data
set_trmt <- function(data, test_formula) {
  arms_all <- all.vars(test_formula)
  arms_trmt <- all.vars(test_formula[[2]])
  arms_ctrl <- all.vars(test_formula[[3]])
  arms_both <- dplyr::intersect(arms_trmt, arms_ctrl)
  if (length(arms_both) > 0) {
    warning("Trial arm in both treatment and control; assigning to treatment.")
  }

  data %>%
    dplyr::filter(.data$arm %in% arms_all) %>%
    dplyr::mutate(trmt = .data$arm %in% arms_trmt)
}

#' Compute statistic for permutations of treatment assignment.
#' i
#' @importFrom rlang .data
compute_permuted_stats <- function(data, stat_compute, num_permutations = 1000) {
  purrr::map_dbl(
    1:num_permutations,
    ~ data %>%
      dplyr::mutate_at(dplyr::vars(.data$trmt), sample) %>%
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
