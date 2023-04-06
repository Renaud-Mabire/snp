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

  # Initialize default values for lists if not provided
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

  # Iterate the main function over the input lists
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

  # Assign a class and return the result as a list
  class(simulations) <- c("multi easy ising simulations obj")
  multi_ising_simulations_obj <- list(simulations = simulations)
  return(multi_ising_simulations_obj)
}
