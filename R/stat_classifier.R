stat_classifier <- function(data) {
  data <-
    data %>%
    mutate(id = row_number()) %>%
    unnest(measurement) %>%
    group_by(id) %>%
    mutate(col_name = seq_along(id)) %>%
    ungroup() %>%
    pivot_wider(names_from = col_name, values_from = "measurement") %>%
    select(-id)

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
