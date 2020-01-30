#' Make each element in measurement vectors a new column.
#'
#' @param data A data frame with list column `measurement`.
#' @return A new data frame, with measurement vectors spread across multiple
#' columns.
#' @export
spread_measurements <- function(data) {
  data %>%
    mutate(id = row_number()) %>%
    unnest(measurement) %>%
    group_by(id) %>%
    mutate(col_name = seq_along(id)) %>%
    ungroup() %>%
    pivot_wider(names_from = col_name, values_from = "measurement") %>%
    select(-id)
}
