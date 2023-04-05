

easy_ising_simulations <- function(df,
                                   centrality_indices = NULL,
                                   nodes_to_influence,
                                   relation = NULL,
                                   Fit_Ising = NULL,
                                   Fit_Ising_thresholds = FALSE,
                                   n,
                                   thresholds_IsingSampler,
                                   beta = 1) {




  library('IsingFit')
  library('IsingSampler')
  library('qgraph')
  library('dplyr')


  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  # Check if centrality_indices is NULL or not
  if (!is.null(centrality_indices)) {
    # Check if centrality_indices is one of the acceptable values
    acceptable_values_centrality_indices <- c("Betweenness", "Closeness", "Strength", "ExpectedInfluence")
    if (!centrality_indices %in% acceptable_values_centrality_indices) {
      stop(sprintf("centrality_indices must be one of the following values: %s", paste(acceptable_values_centrality_indices, collapse = ", ")))
    }
  }

  # Check if nodes_to_influence is one of the acceptable values
  if (!is.numeric(nodes_to_influence) & !all(nodes_to_influence %in% colnames(df))) {
    stop("'nodes_to_influence' must be either a numeric vector or a vector composed of the name of one or more dataframe variables.")
  }

  # If it is a numerical value, check that there is only one.
  if (is.numeric(nodes_to_influence) && length(nodes_to_influence) != 1) {
    stop("nodes_to_influence should be a numeric vector with a single value (or a vector composed of the name of one or more dataframe variables).")
  }

  # Check if centrality_indices is not NULL if nodes_to_influence is numeric
  if (is.numeric(nodes_to_influence) && is.null(centrality_indices)) {
    stop("centrality_indices cannot be NULL if nodes_to_influence is numeric.")
  }

  # Check if centrality_indices is NULL, nodes_to_influence must be one one or more variable names belonging to the df data frame
  if (is.null(centrality_indices) && (!is.character(nodes_to_influence) || any(!nodes_to_influence %in% names(df)))) {
    stop("If centrality_indices is NULL, nodes_to_influence must be one or more variable names belonging to the df data frame.")
  }

  # Check if n is a numeric value and is greater than 1
  if (!is.numeric(n) || n < 1) {
    stop("n must be a numeric value greater than 1.")
  }

  # Check that thresholds_IsingSampler is an integer
  if (!is.numeric(thresholds_IsingSampler)){
    stop("The 'thresholds_IsingSampler' argument must be a positive or negative integer")
  }

  # Check if beta is a single numeric value and not NULL
  if (is.null(beta) || !is.numeric(beta) || length(beta) != 1) {
    stop("beta must be a single numeric value.")
  }

  if (!is.null(relation)) {
    # Check if 'relation' is either ">=" or "<="
    if (!(relation == ">=" || relation == "<=")) {
      stop("relation must be either '>=' or '<='.")
    }
  }

  # Check that 'relation' is not NULL if nodes_to_influence is numeric and centrality_indices is not NULL
  if (is.numeric(nodes_to_influence) && !is.null(centrality_indices) && is.null(relation)) {
    stop("If nodes_to_influence is numeric and centrality_indices is not NULL, then relation cannot be NULL and must be either '>=' or '<='.")
  }
  # If nodes_to_influence is not numeric, relation can't be NULL
  if (!is.numeric(nodes_to_influence) && !is.null(relation)) {
    stop("If nodes_to_influence is not numeric, relation must be NULL.")
  }

  if (!is.null(centrality_indices) && !is.numeric(nodes_to_influence)) {
    stop("If centrality_indices is not NULL, nodes_to_influence must be a numeric vector.")
  }


  # Creation of Ising fit if it is not given as an argument
  if (is.null(Fit_Ising)) {
    Fit_Ising <- IsingFit(df, plot = FALSE)
  }

  # Check if Fit_Ising is an object created by IsingFit
  if (!inherits(Fit_Ising, "IsingFit")) {
    stop("Fit_Ising must be an object created by IsingFit")
  }



  if (!is.null(centrality_indices)) {
    # Sort the centrality values
    centrality_values_sorted <-
      qgraph(Fit_Ising$weiadj, labels = names(df), DoNotPlot = TRUE) %>%
      centralityTable(standardized = FALSE) %>%
      filter(measure == centrality_indices) %>%
      arrange(desc(value)) %>%
      select(node, value)
  }

  # Check if nodes_to_influence is a vector of variable names
  if (all(nodes_to_influence %in% colnames(df))) {
    Position_in_the_df_of_the_modified_nodes <- which(names(df) %in% nodes_to_influence)
  } else {
    if (is.numeric(nodes_to_influence)) {
      # Determine the quantile value based on nodes_to_influence
      quantile_value <- nodes_to_influence
      # Get the threshold value
      threshold <- quantile(centrality_values_sorted$value, quantile_value)
      # Get the highest centrality values
      if (relation == ">=") {
        highest_values <- centrality_values_sorted %>%
          filter(value >= threshold)
      } else if (relation == "<=") {
        highest_values <- centrality_values_sorted %>%
          filter(value <= threshold)
      }
      Influenced_nodes <- highest_values$node
      # Position of each variable in the data frame
      Position_in_the_df_of_the_modified_nodes <- which(names(df) %in% Influenced_nodes)
    }
  }

  # Setting the level of threshold change
  modif <- sd(Fit_Ising$thresholds) * thresholds_IsingSampler
  # Thresholds list
  if (Fit_Ising_thresholds) {
    list_thresholds <- Fit_Ising$thresholds
    list_thresholds[Position_in_the_df_of_the_modified_nodes] <- Fit_Ising$thresholds[Position_in_the_df_of_the_modified_nodes] + modif
  } else {
    # Create a new vector with 0 values
    list_thresholds <- rep(0, length(Fit_Ising$thresholds))
    # Replace the modified values with the updated values
    list_thresholds[Position_in_the_df_of_the_modified_nodes] <- Fit_Ising$thresholds[Position_in_the_df_of_the_modified_nodes] + modif
  }



  # Linear transformation
  SimInput <- LinTransform(Fit_Ising$weiadj, Fit_Ising$thresholds)

  # Simulation of the sample(s) that can contain the -1/1 values

  sample <- IsingSampler(n, SimInput$graph, thresholds = list_thresholds, beta = beta, responses = c(-1L, 1L))
  # Recode the -1 by 0
  sample[sample == -1] <- 0
  # Estimation of an Ising network
  Fit_sample <- IsingFit(sample, plot = FALSE)
  # Calculation of individual scores for each of the networks
  sum_score <- apply(sample, 1, sum)




  # Calculation of mean, median, sd, min and max of each of the networks
  mean_score <- mean(sum_score)
  median_score <- median(sum_score)
  sd_score <- sd(sum_score)
  min_score <- min(sum_score)
  max_score <- max(sum_score)

  #
  if (is.numeric(nodes_to_influence)) {
    easy_ising_simulations_obj <- list(
      centrality_values_sorted = centrality_values_sorted,
      Influenced_nodes = Influenced_nodes,
      list_thresholds = list_thresholds,
      Position_in_the_df_of_the_modified_nodes = Position_in_the_df_of_the_modified_nodes,
      Fit_sample = Fit_sample,
      sample_list = sample,
      sum_score = sum_score,
      mean_score = mean_score,
      median_score = median_score,
      sd_score = sd_score,
      min_score = min_score,
      max_score = max_score,
      df_names = names(df)
    )
  } else {
    easy_ising_simulations_obj <- list(
      list_thresholds = list_thresholds,
      Position_in_the_df_of_the_modified_nodes = Position_in_the_df_of_the_modified_nodes,
      Fit_sample = Fit_sample,
      sample = sample,
      sum_score = sum_score,
      mean_score = mean_score,
      median_score = median_score,
      sd_score = sd_score,
      min_score = min_score,
      max_score = max_score,
      df_names = names(df)

    )
  }

  class(easy_ising_simulations_obj) <- c("easy ising simulations obj")
  return(easy_ising_simulations_obj)
}

