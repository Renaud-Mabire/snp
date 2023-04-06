snp_plot <- function(simulations, CI = 0.95, theme = ggthemes::theme_solarized()) {

  # Load required packages
  require('ggthemes')
  require('ggplot2')
  require('rstatix')

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
  sumScoresPlot <- ggplot(data = data_summary_snp,
                          mapping = aes(x = intervention, y = sumscore_mean, group = 1)) +
                   geom_line() +
                   geom_point() +
                   geom_errorbar(mapping = ggplot2::aes(ymin = ciLower, ymax = ciUpper), width = .15) +
                   labs(x = "Intervention", y = "Sum score") +
                   labs(subtitle = paste( "ANOVA: F(", df1, ",", df2, ") =",
                                          round(F_value, 3), "; p =",
                                          format.pval(p_value, digits = 3),
                                          "; ges =", ges)) +
                   theme

  return(sumScoresPlot)
}
