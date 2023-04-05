

plot_diff_sumscore <- function(simulation,
                          type = "parametric",
                          pairwise.display = "significant",
                          plot.type = "box",
                          showdots = FALSE,
                          ylab = "Sum score",
                          theme = ggthemes::theme_solarized(),
                          ...) {
  require(ggstatsplot)
  list_sums_score_simulation <- lapply(1:length(simulation[[1]]), function(x)
    simulation[[1]][[x]][["sum_score_list"]][[1]])
  list_intervention <- c(1:length(simulation[[1]]))
  res_df <- data.frame(
    sumscore = unlist(list_sums_score_simulation),
    intervention = rep(paste0("Intervention: ", list_intervention),
                       each = length(simulation[[1]][[1]][["sum_score_list"]][[1]]))
  )

 if (showdots) {
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
     theme +
     theme(legend.position = "none")
 } else {
   plot <- ggbetweenstats(
     data = res_df,
     x = intervention,
     y = sumscore,
     type = type,
     pairwise.display = pairwise.display,
     plot.type = plot.type,
     point.args = list(alpha = 0),
     centrality.point.args = list(alpha = 0),
     point.path = FALSE,
     ylab = ylab,
     ...
   ) +
     theme +
     theme(legend.position = "none")
}
  print(plot)
}

