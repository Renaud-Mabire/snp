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
                                   networkNames = NULL) {

  # Load required libraries
  library('ggplot2')
  library('forcats')

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
  df <- do.call(centralityTable, weiadj_list) %>% filter(measure %in% include)

  # Generate network names if not provided
  if (is.null(networkNames)) {
    networkNames <- paste0("Network ", 1:Nb_simulations)
  }

  # Create a named list to map graph labels to network names
  networkNameMapping <- setNames(networkNames, paste0("graph ", 1:Nb_simulations))

  # Create the plot using ggplot2
  df %>%
    mutate(
      graph = recode(graph,!!!networkNameMapping),
      # Map graph labels to network names
      graph = as.factor(graph),
      # Convert 'graph' to a factor
      node = as.factor(node)) %>%  # Convert 'node' to a factor
    mutate(node = fct_reorder(node, value)) %>%  # Reorder 'node' based on 'value'
    ggplot(aes(x = node, y = value, group = graph)) +  # Initialize ggplot with data and aesthetics
    geom_line(aes(linetype = graph), size = 1) +  # Add lines with different linetypes for each graph
    labs(x = '', y = '') +  # Remove axis labels
    scale_linetype_discrete(name = legendName) +  # Set legend title
    coord_flip() +  # Flip the coordinates (x and y axes)
    facet_grid( ~ measure) +  # Facet the plot by centrality measure
    theme_bw()  # Use black and white theme
}
