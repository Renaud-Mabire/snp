snp_plot <- function(simulations, CI = 0.95, theme = ggthemes::theme_solarized()) {

  # Get the sample size
  sample_size <- length(simulations[["simulations"]][[1]][["sum_score"]])

  # Extract the sum of scores for each simulation
  list_sums_score_simulation <- lapply(1:length(simulations[[1]]), function(x)
    simulations[["simulations"]][[x]][["sum_score"]])
  list_intervention <- c(1:length(simulations[[1]]))

  # Create a data frame with the sum of scores and intervention
  res_df <- data.frame(
    sumscore = unlist(list_sums_score_simulation),
    intervention = rep(paste0(list_intervention), each = sample_size)
  )

  # Calculate the summary statistics for the sum of scores
  data_summary_snp <- summary_snp(simulations, CI = CI)

  # Perform ANOVA
  anova_result <- res_df %>% rstatix::anova_test(sumscore ~ intervention)

  # Extract the results
  F_value <- anova_result$F
  df1 <- anova_result$DFn
  df2 <- anova_result$DFd
  p_value <- anova_result$p
  ges <- anova_result$ges

  # Create a plot of the sum of scores with error bars
  sumScoresPlot <- ggplot2::ggplot(data = data_summary_snp,
                                   mapping = aes(x = intervention, y = sumscore_mean, group = 1)) +
                   ggplot2::geom_line() +
                   ggplot2::ggplot2::geom_point() +
                   ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ciLower, ymax = ciUpper), width = .15) +
                   ggplot2::labs(x = "Intervention", y = "Sum score") +
                   ggplot2::labs(subtitle = paste( "ANOVA: F(", df1, ",", df2, ") =",
                                          round(F_value, 3), "; p =",
                                          format.pval(p_value, digits = 3),
                                          "; ges =", ges)) +
                   theme

  return(sumScoresPlot)
}
