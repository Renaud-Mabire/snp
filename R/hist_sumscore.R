# This function creates a histogram of individual scores per intervention
hist_sumscore <- function(simulations,
                          display_mean = FALSE,
                          display_median = FALSE,
                          theme = ggthemes::theme_solarized()) {
  # Create a list of sum_score values from the simulations
  list_sums_score_simulation <-
    lapply(1:length(simulations[[1]]), function(x)
      simulations[["simulations"]][[x]][["sum_score"]])

  # Create a list of intervention numbers
  list_intervention <- c(1:length(simulations[[1]]))

  # Create a data frame with sumscore and intervention columns
  res_df <- data.frame(
    sumscore = unlist(list_sums_score_simulation),
    intervention = rep(
      paste0("Intervention: ", list_intervention),
      each = length(simulations[["simulations"]][[1]][["sum_score"]])
    )
  )

  # Create the base histogram plot using ggplot2
  plot <- res_df %>%
    ggplot2::ggplot(aes(x = sumscore)) +  # Set the x-axis to 'sumscore'
    ggplot2::geom_histogram(binwidth = 0.5) +  # Add histogram with bin width of 0.5
    ggplot2::facet_grid(intervention ~ .) +  # Facet the plot by intervention
    theme +  # Apply the specified theme
    ggplot2::theme(legend.position = "right") +  # Set legend position to the right
    ggplot2::themelabs(title = "Individual scores per intervention")  # Set the plot title

  # If display_mean or display_median is TRUE, add mean and/or median to the facet labels
  if (display_mean | display_median) {
    intervention_stats <- res_df %>%
      dplyr::group_by(intervention) %>%  # Group data by intervention
      dplyr::summarise(mean_score = mean(sumscore),
                       median_score = median(sumscore))  # Calculate mean and median scores

    plot <- plot +
      ggplot2::scale_color_manual(name = "",
                                  values = c("Mean" = "blue", "Median" = "red")) +  # Set colors for mean and median lines
      ggplot2::facet_wrap(~ intervention,
                          ncol = 4,
                          labeller = labeller(
                            intervention = function(x) {
                              label <- paste0("Intervention ", x)  # Base intervention label
                              if (display_mean) {
                                label <-
                                  paste0(label,
                                         "\nMean: ",
                                         round(intervention_stats$mean_score[intervention_stats$intervention == x], 2))  # Add mean score to the label
                              }
                              if (display_median) {
                                label <-
                                  paste0(label,
                                         ", Median: ",
                                         round(intervention_stats$median_score[intervention_stats$intervention == x], 2))  # Add median score to the label
                              }
                              label  # Return the modified label
                            }
                          ))
  } else {
    # If display_mean and display_median are FALSE, just facet the plot by intervention without modifying the labels
    plot <- plot + ggplot2::facet_wrap(~ intervention, ncol = 4)
  }

  # Print the final plot
  print(plot)
}
