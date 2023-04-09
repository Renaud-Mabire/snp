# This function creates a list of network graphs visualizing the structure of social networks from simulation results
snp_graph <- function(simulations,
                      network_community_detection = FALSE,
                      layout = "spring",
                      fade = TRUE,
                      negDashed = TRUE,
                      edge.labels = FALSE,
                      legend = FALSE,
                      grah_nb_col = 3,
                      ...) {

    # Create a list of IsingFit objects from the simulations
  Fit_Ising_list <- lapply(seq_along(simulations$simulations), function(x) simulations[["simulations"]][[x]][["Fit_sample"]])

  # Find the maximum absolute weight across all networks
  max_value <- max(unlist(lapply(simulations[["simulations"]], function(x) max(abs(x[["Fit_sample"]][["weiadj"]])))))

  # Create a function to generate a network layout based on the specified layout type
  net_layout_creation <- function(simulations, net_layout_type = layout) {
    nb_simulations <- length(simulations[["simulations"]])
    fit_samples <- lapply(simulations[["simulations"]], function(x) x[["Fit_sample"]][["weiadj"]])
    net_layout <- do.call("averageLayout", c(fit_samples, list(layout = net_layout_type)))
    return(net_layout)
  }
  net_layout <- net_layout_creation(simulations, net_layout_type = layout)

  # Perform community detection if network_community_detection = TRUE
  if (network_community_detection) {
    list_NetworkCom <- lapply(
      Fit_Ising_list,
      function(x)
        igraph::graph_from_adjacency_matrix(abs(x$weiadj), "undirected", weighted = TRUE, add.colnames = FALSE) %>% # Create igraph from IsingFit matrix
        igraph::cluster_walktrap() %>%
        igraph::communities() # Detect communities
    )
  }

  # Initialize variables for creating the plot grid
  list_graph <- list()
  max_cols <- grah_nb_col
  n_plots <- length(simulations[["simulations"]])
  n_rows <- ceiling(n_plots / max_cols)

  # Create the network graphs with or without community detection
  if (network_community_detection){
    par(mfrow = c(n_rows, max_cols), mar = c(3, 3, 1, 1), oma = c(0, 0, 2, 0))
    for (i in 1:n_plots) {
      set.seed(335)
      list_graph[[i]] <- qgraph::qgraph(simulations[["simulations"]][[i]][["Fit_sample"]][["weiadj"]],
                                groups = list_NetworkCom[[i]],
                                nodeNames = simulations[["simulations"]][[i]][["df_names"]],
                                edge.color = Fit_Ising_list[[i]]$q$graphAttributes$Edges$color,
                                layout = net_layout,
                                maximum = max_value,
                                negDashed = negDashed,
                                edge.labels = edge.labels,
                                fade = fade,
                                legend = legend,
                                title = paste0("Attitude - Graph" , i),
                                title.cex = 1.25,
                                mar = c(3,3,3,3),
                                ...)
      if (i <= n_plots) {
        par(mfg = c(((i - 1) %/% max_cols) + 1, ((i - 1) %% max_cols) + 1))
        graphics::plot.new()
      }
    }
  } else {
    par(mfrow = c(n_rows, max_cols), mar = c(3, 3, 1, 1), oma = c(0, 0, 2, 0))
    for (i in 1:n_plots) {
      set.seed(335)
      list_graph[[i]] <- qgraph::qgraph(simulations[["simulations"]][[i]][["Fit_sample"]][["weiadj"]],
                                nodeNames = simulations[["simulations"]][[i]][["df_names"]],
                                edge.color = Fit_Ising_list[[i]]$q$graphAttributes$Edges$color,
                                layout = net_layout,
                                maximum = max_value,
                                negDashed = negDashed, # Allow negative edges to be dashed for greyscale images
                                edge.labels = edge.labels,
                                fade = fade,
                                legend = legend,
                                title = paste0("Attitude - Graph" , i),
                                title.cex = 1.25,
                                mar = c(3,3,3,3), # Bottom, left, top, right
                                ...)
      if (i <= n_plots) {
        par(mfg = c(((i - 1) %/% max_cols) + 1, ((i - 1) %% max_cols) + 1))
        graphics::plot.new()
      }
    }
  }

  # Return the list of network graphs and, if applicable, the list of detected communities
  if (network_community_detection) {
    result <- list(list_graph, list_NetworkCom)
  } else {
    result <- list_graph
  }

  return(result)
}

