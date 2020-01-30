#' Classifier test statistic which returns the concordance (C-statistic, AUC, etc.)
#' of a classifier trained on measurements to predict treatment arm.
#' @export
stat_classifier <- function(data) {
  data <- spread_measurements(data)
  Concordance(
    data$arm,
    predict(
      suppressWarnings(
        glm(arm ~ ., data = data, family = binomial(link = "logit"))
      ),
      type = "response"
    )
  )$Concordance
}
