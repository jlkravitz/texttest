#' Classifier test statistic which returns the concordance of a classifier
#' trained on measurements to predict treatment arm.
#' @inheritParams stat_summed_mean_difference
#' @importFrom rlang .data
#' @export
stat_classifier <- function(data) {
  data <-
    data %>%
    dplyr::select(.data$trmt, .data$measurement) %>%
    spread_measurements() %>%
    dplyr::mutate(trmt = as.integer(.data$trmt))

  model <- suppressWarnings(
    stats::glm(
      trmt ~ .,
      data = data,
      family = stats::binomial(link = "logit")
    )
  )

  tibble::tibble(
    stat =
      InformationValue::Concordance(
        data$trmt,
        stats::predict(
          model,
          type = "response"
        )
      )$Concordance,
    metadata = list(model)
  )
}
