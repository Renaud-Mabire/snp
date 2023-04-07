# This function compares the centrality measures of simulated networks
compare_centrality_snp <- function(simulations,
                                   include = c("Strength",
                                               "Closeness",
                                               "Betweenness",
                                               "ExpectedInfluence",
                                               "all",
                                               "All"),
                                   orderBy = c("Strength",
                                               "Closeness",
                                               "Betweenness",
                                               "ExpectedInfluence"),
                                   legendName = '',
                                   networkNames = NULL,
                                   theme = ggthemes::theme_solarized()) {

  # If 'all' or 'All' is included in the 'include' parameter, use all centrality measures
  if (any(include == "All", include == "all")) {
    include = c("Strength", "Closeness", "Betweenness", "ExpectedInfluence")
  }

  # Get the number of simulations
  Nb_simulations <- length(simulations$simulations)

  # Create a list of weighted adjacency matrices from the simulations
  weiadj_list <- lapply(
    1:Nb_simulations, function(x) {
      simulations[["simulations"]][[x]][["Fit_sample"]][["weiadj"]]
    }
  )

  # Use do.call to pass the list of networks to centralityTable function
  df <- do.call(qgraph::centralityTable, weiadj_list) %>% dplyr::filter(measure %in% include)

  # Generate network names if not provided
  if (is.null(networkNames)) {
    networkNames <- paste0("Network ", 1:Nb_simulations)
  }

  # Create a named list to map graph labels to network names
  networkNameMapping <- stats::setNames(networkNames, paste0("graph ", 1:Nb_simulations))

  # Create the plot using ggplot2
  df %>%
    dplyr::mutate(
      graph = recode(graph,!!!networkNameMapping),
      # Map graph labels to network names
      graph = as.factor(graph),
      # Convert 'graph' to a factor
      node = as.factor(node)) %>%  # Convert 'node' to a factor
    dplyr::mutate(node = forcats::fct_reorder(node, value)) %>%  # Reorder 'node' based on 'value'
    ggplot2::ggplot(ggplot2::aes(x = node, y = value, group = graph)) +  # Initialize ggplot with data and aesthetics
    ggplot2::geom_line(ggplot2::aes(linetype = graph), size = 1) +  # Add lines with different linetypes for each graph
    ggplot2::labs(x = '', y = '') +  # Remove axis labels
    ggplot2::scale_linetype_discrete(name = legendName) +  # Set legend title
    ggplot2::coord_flip() +  # Flip the coordinates (x and y axes)
    ggplot2::facet_grid( ~ measure) +  # Facet the plot by centrality measure
    theme  # Fixe the theme
}
