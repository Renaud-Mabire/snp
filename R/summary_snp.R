summary_snp <- function(simulations, CI = 0.95) {
  # Load required packages
  require('Rmisc')
  require('dplyr')

  # Extract the sum of scores for each simulation
  list_sums_score_simulation <- lapply(1:length(simulations[[1]]), function(x)
                                       simulations[["simulations"]][[x]][["sum_score"]])

  list_intervention <- c(1:length(simulations[[1]]))

  # Create a data frame with the sum of scores and intervention
  res_df <- data.frame(sumscore = unlist(list_sums_score_simulation),
                       intervention = rep(paste0("Intervention: ", list_intervention),
                       each = length(simulations[["simulations"]][[1]][["sum_score"]])
    )
  )

  # Calculate the summary statistics for the sum of scores
  simulations_summary <- summarySE(
    data = res_df,
    measurevar = "sumscore",
    groupvars = "intervention",
    conf.interval = CI
  )

  # Rename the columns of the summary data frame
  colnames(simulations_summary) <-
    c("intervention", "n", "sumscore_mean", "sd", "se", "ci")

  # Calculate the lower and upper bounds of the confidence interval
  simulations_summary <- simulations_summary %>%
                         mutate(ciLower = sumscore_mean - ci,
                                ciUpper = sumscore_mean + ci) %>%
                         dplyr::select(-ci)

  return(simulations_summary)
}
