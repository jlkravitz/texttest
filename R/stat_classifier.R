#' Classifier test statistic which returns the concordance (C-statistic, AUC, etc.)
#' of a classifier trained on measurements to predict treatment arm.
#' @export
stat_classifier <- function(data) {
  data <-
    spread_measurements(data) %>%
    mutate(trmt = as.integer(trmt))

  Concordance(
    data$arm,
    predict(
      suppressWarnings(
        glm(trmt ~ ., data = data, family = binomial(link = "logit"))
      ),
      type = "response"
    )
  )$Concordance
}
