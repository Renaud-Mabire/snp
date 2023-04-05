

snp_Ising <- function(df_list,
                                    nodes_to_influence_list,
                                    centrality_indices_list = NULL,
                                    relation_list = NULL,
                                    Fit_Ising_list = NULL,
                                    Fit_Ising_thresholds_list,
                                    n,
                                    thresholds_IsingSampler_list,
                                    beta_list) {

  # List of packages to install and/or load
  packages <- c(
    "IsingFit",
    "ggplot2",
    "ggdist",
    "ggthemes",
    "gghalves",
    "magrittr",
    "qgraph",
    "IsingSampler",
    "dplyr",
    "rstatix"
  )

  # Package loading
  lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  })


  if (is.null(centrality_indices_list)) {
    centrality_indices_list <- vector("list", length(df_list))
  }

  if (is.null(relation_list)) {
    relation_list <- vector("list", length(df_list))
  }

  if (is.null(Fit_Ising_list)) {
    Fit_Ising_list <- lapply(df_list, function(x) IsingFit(x, plot = FALSE))
  }

  # Check that all argument lists have the same length
  cat(paste0("Length of 'df_list': ", length(df_list), "\n"))
  cat(paste0("Length of 'nodes_to_influence_list': ", length(nodes_to_influence_list), "\n"))
  cat(paste0("Length of 'centrality_indices_list': ", length(centrality_indices_list), "\n"))
  cat(paste0("Length of 'relation_list': ", length(relation_list), "\n"))
  cat(paste0("Length of 'Fit_Ising_thresholds_list': ", length(Fit_Ising_thresholds_list), "\n"))
  cat(paste0("Length of 'thresholds_IsingSampler_list': ", length(thresholds_IsingSampler_list), "\n"))
  cat(paste0("Length of 'beta_list': ", length(beta_list), "\n"))

  if (length(unique(c(
    length(df_list),
    length(nodes_to_influence_list),
    length(centrality_indices_list),
    length(relation_list),
    length(Fit_Ising_thresholds_list),
    length(thresholds_IsingSampler_list),
    length(beta_list)
  ))) > 1) {
    stop("All lists must have the same length.")
  }



  # Iteration of the main function
  simulations <- Map(
    easy_ising_simulations,
    df_list,
    centrality_indices_list,
    nodes_to_influence_list,
    relation_list,
    Fit_Ising_list,
    Fit_Ising_thresholds_list,
    n,
    thresholds_IsingSampler_list,
    beta_list
  )



  class(simulations) <- c("multi easy ising simulations obj")


  #multi_ising_simulations_obj <- list(simulations = simulations, anova_table = anova_table)
  multi_ising_simulations_obj <- list(simulations = simulations)
  return(multi_ising_simulations_obj)
}


# # Regression for ANOVA
# # ANOVA
#
# list_sums_score_simulation <- lapply(1:length(simulations), function(x) simulations[[x]][["sum_score_list"]])
# list_intervention <- c(1:length(simulations))
#
# res_df <- data.frame(
#   sumscore = unlist(list_sums_score_simulation),
#   intervention = rep(paste0("Intervention: ", list_intervention),
#     each = length(list_sums_score_simulation[[1]][[1]])
#   )
# )
# # ANOVA
# res_ANOVA <- res_df %>% anova_test(sumscore ~ intervention)
# anova_table <- as.data.frame(res_ANOVA)

# class(anova_table) <- c("ANOVA")
