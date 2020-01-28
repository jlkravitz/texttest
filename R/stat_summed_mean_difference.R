stat_summed_mean_difference <- function(measurements, arms) {
  summed <- map_dbl(measurements, sum)
  mean(summed[arms == "trmt"]) - mean(summed[arms == "ctrl"])
}
