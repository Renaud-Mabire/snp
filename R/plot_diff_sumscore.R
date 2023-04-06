# This function creates a plot to compare sum scores between interventions
plot_diff_sumscore <- function(simulations,
                               type = "parametric",
                               pairwise.display = "significant",
                               plot.type = "box",
                               showdots = FALSE,
                               ylab = "Sum score",
                               theme = ggthemes::theme_solarized(),
                               ...) {
  # Load required library
  require(ggstatsplot)

  # Create a list of sum_score values from the simulations
  list_sums_score_simulation <- lapply(1:length(simulations[[1]]), function(x)
    simulations[["simulations"]][[x]][["sum_score"]])

  # Create a list of intervention numbers
  list_intervention <- c(1:length(simulations[[1]]))

  # Create a data frame with sumscore and intervention columns
  res_df <- data.frame(
    sumscore = unlist(list_sums_score_simulation),
    intervention = rep(paste0("Intervention: ", list_intervention),
                       each = length(simulations[["simulations"]][[1]][["sum_score"]]))
  )

  # Check if dots should be displayed on the plot
  if (showdots) {
    # Create the plot using ggbetweenstats function from ggstatsplot package with dots
    plot <- ggbetweenstats(
      data = res_df,
      x = intervention,
      y = sumscore,
      type = type,
      pairwise.display = pairwise.display,
      plot.type = plot.type,
      ylab = ylab,
      ...
    ) +
      theme +  # Apply the specified theme
      theme(legend.position = "none")  # Remove the legend
  } else {
    # Create the plot using ggbetweenstats function from ggstatsplot package without dots
    plot <- ggbetweenstats(
      data = res_df,
      x = intervention,
      y = sumscore,
      type = type,
      pairwise.display = pairwise.display,
      plot.type = plot.type,
      point.args = list(alpha = 0),  # Make dots transparent
      centrality.point.args = list(alpha = 0),  # Make central tendency dots transparent
      point.path = FALSE,  # Do not display lines connecting dots
      ylab = ylab,
      ...
    ) +
      theme +  # Apply the specified theme
      theme(legend.position = "none")  # Remove the legend
  }

  # Print the final plot
  print(plot)
}
