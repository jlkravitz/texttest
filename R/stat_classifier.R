#' Classifier test statistic which returns the concordance (C-statistic, AUC, etc.)
#' of a classifier trained on measurements to predict treatment arm.
#' @importFrom rlang .data
#' @export
stat_classifier <- function(data) {
  data <-
    spread_measurements(data) %>%
    dplyr::mutate(trmt = as.integer(.data$trmt))

  InformationValue::Concordance(
    data$arm,
    stats::predict(
      suppressWarnings(
        stats::glm(
          trmt ~ .,
          data = data,
          family = stats::binomial(link = "logit")
        )
      ),
      type = "response"
    )
  )$Concordance
}
