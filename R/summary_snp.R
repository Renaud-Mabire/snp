

summary_snp <- function(simulations, CI = 0.95) {

  require('Rmisc')
  require('dplyr')

list_sums_score_simulation <- lapply(1:length(simulations[[1]]), function(x)
  simulations[["simulations"]][[x]][["sum_score"]])
list_intervention <- c(1:length(simulations[[1]]))
res_df <- data.frame(
  sumscore = unlist(list_sums_score_simulation),
  intervention = rep(paste0("Intervention: ", list_intervention),
                     each = length(simulations[["simulations"]][[1]][["sum_score"]]))
)


simulations_summary <- summarySE(data = res_df,
                                        measurevar = "sumscore",
                                        groupvars = "intervention",
                                        conf.interval = CI)

colnames(simulations_summary) <- c("intervention", "n", "sumscore_mean", "sd", "se", "ci")


simulations_summary <- simulations_summary %>%
                       mutate(ciLower = sumscore_mean - ci,
                              ciUpper = sumscore_mean + ci) %>%
                      dplyr::select(-ci)


return(simulations_summary)

}



