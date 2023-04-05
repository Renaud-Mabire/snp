

hist_sumscore <- function(simulations,
                          display_mean = FALSE,
                          display_median = FALSE,
                          theme = ggthemes::theme_solarized()) {

  require(dplyr)
  require(ggplot2)
  require(ggthemes)

list_sums_score_simulation <- lapply(1:length(simulations[[1]]), function(x)
  simulations[["simulations"]][[x]][["sum_score"]])
list_intervention <- c(1:length(simulations[[1]]))
res_df <- data.frame(
  sumscore = unlist(list_sums_score_simulation),
  intervention = rep(paste0("Intervention: ", list_intervention),
                     each = length(simulations[["simulations"]][[1]][["sum_score"]]))
  )


plot <- res_df %>%
  ggplot(aes(x = sumscore)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(intervention ~ .) +
  theme +
  theme(legend.position = "right") +
  labs(title = "Individual scores per intervention")

if (display_mean | display_median) {
  intervention_stats <- res_df %>%
    group_by(intervention) %>%
    summarise(mean_score = mean(sumscore), median_score = median(sumscore))

  plot <- plot + scale_color_manual(name = "", values = c("Mean" = "blue", "Median" = "red")) +
    facet_wrap(~ intervention, ncol = 4, labeller = labeller(intervention = function(x) {
      label <- paste0("Intervention ", x)
      if (display_mean) {
        label <- paste0(label, "\nMean: ", round(intervention_stats$mean_score[intervention_stats$intervention == x], 2))
      }
      if (display_median) {
        label <- paste0(label, ", Median: ", round(intervention_stats$median_score[intervention_stats$intervention == x], 2))
      }
      label
    }))
} else {
  plot <- plot + facet_wrap(~ intervention, ncol = 4)
}


print(plot)
}
