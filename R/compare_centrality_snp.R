
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

  library('ggplot2')
  library('forcats')

  if (any(include == "All", include == "all")) {
    include = c("Strength", "Closeness", "Betweenness", "ExpectedInfluence")
  }


  Nb_simulations <- length(simulations$simulations)

  # Create a list of weiadj
  weiadj_list <- lapply(
    1:Nb_simulations, function(x) {
      simulations[["simulations"]][[x]][["Fit_sample"]][["weiadj"]]
    }
  )

  # Use do.call to pass the list of networks to centralityTable
  df <- do.call(centralityTable, weiadj_list) %>% filter(measure %in% include)

  # Generate network names if not provided
  if (is.null(networkNames)) {
    networkNames <- paste0("Network ", 1:Nb_simulations)
  }

  # Create a named list to map graph labels to network names
  networkNameMapping <- setNames(networkNames, paste0("graph ", 1:Nb_simulations))

  df %>%
    mutate(graph = recode(graph, !!!networkNameMapping),
           graph = as.factor(graph),
           node = as.factor(node)) %>%

    mutate(node = fct_reorder(node, value)) %>%

    ggplot(aes(x = node, y = value, group = graph)) +

    geom_line(aes(linetype = graph), size = 1) +

    labs(x = '', y = '') +

    scale_linetype_discrete(name = legendName) +

    coord_flip() +

    facet_grid(~measure) +

    theme_bw()
}

