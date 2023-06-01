easy_ising_simulations <- function(df,
                                   centrality_indices = NULL,
                                   nodes_to_influence,
                                   relation = NULL,
                                   Fit_Ising = NULL,
                                   Fit_Ising_thresholds = FALSE,
                                   n,
                                   thresholds_IsingSampler,
                                   beta = 1,
                                   IsingSampler_method = IsingSampler_method) {
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  # Check if centrality_indices is one of the acceptable values
  if (!is.null(centrality_indices)) {
    acceptable_values_centrality_indices <- c("Betweenness", "Closeness", "Strength", "ExpectedInfluence")
    centrality_indices <- match.arg(centrality_indices, acceptable_values_centrality_indices)
  }

  # Check if nodes_to_influence is one of the acceptable values
  is_numeric_nodes_to_influence <- is.numeric(nodes_to_influence)
  if (!is_numeric_nodes_to_influence & !all(nodes_to_influence %in% colnames(df))) {
    stop("'nodes_to_influence' must be either a numeric vector or a vector composed of the name of one or more dataframe variables.")
  }

  # Check if nodes_to_influence is numeric and there is only one value
  if (is_numeric_nodes_to_influence && length(nodes_to_influence) != 1) {
    stop("nodes_to_influence should be a numeric vector with a single value (or a vector composed of the name of one or more dataframe variables).")
  }

  # Check if centrality_indices is not NULL if nodes_to_influence is numeric
  if (is_numeric_nodes_to_influence && is.null(centrality_indices)) {
    stop("centrality_indices cannot be NULL if nodes_to_influence is numeric.")
  }

  # Check if relation is one of the acceptable values
  if (!is.null(relation)) {
    relation <- match.arg(relation, c(">=", "<="))
  }

  # Check if n is a numeric value and is greater than 1
  if (!is.numeric(n) || n < 1) {
    stop("n must be a numeric value greater than 1.")
  }

  # Check that thresholds_IsingSampler is an integer
  if (!is.numeric(thresholds_IsingSampler)) {
    stop("The 'thresholds_IsingSampler' argument must be a positive or negative integer")
  }

  # Check if beta is a single numeric value and not NULL
  if (is.null(beta) || !is.numeric(beta) || length(beta) != 1) {
    stop("beta must be a single numeric value.")
  }

  ## Check if 'relation' is either ">=" or "<="
  if (!is.null(relation)) {
    if (!(relation == ">=" || relation == "<=")) {
      stop("relation must be either '>=' or '<='.")
    }
  }

  ## Check that 'relation' is not NULL if nodes
  if (is.numeric(nodes_to_influence) && !is.null(centrality_indices) && is.null(relation)) {
    stop("If nodes_to_influence is numeric and centrality_indices is not NULL, then relation cannot be NULL and must be either '>=' or '<='.")
  }

  ## If nodes_to_influence is not numeric, relation can't be NULL
  if (!is.numeric(nodes_to_influence) && !is.null(relation)) {
    stop("If nodes_to_influence is not numeric, relation must be NULL.")
  }

  ## If centrality_indices is not NULL, nodes_to_influence must be a numeric vector.
  if (!is.null(centrality_indices) && !is.numeric(nodes_to_influence)) {
    stop("If centrality_indices is not NULL, nodes_to_influence must be a numeric vector.")
  }

  ## Creation of Ising fit if it is not given as an argument
  if (is.null(Fit_Ising)) {
    Fit_Ising <- IsingFit::IsingFit(df, plot = FALSE)
  }

  ## Check if Fit_Ising is an object created by IsingFit
  if (!inherits(Fit_Ising, "IsingFit")) {
    stop("Fit_Ising must be an object created by IsingFit")
  }

  ## If centrality_indices is not NULL, sort the centrality values
  if (!is.null(centrality_indices)) {
    centrality_values_sorted <-
      qgraph::qgraph(Fit_Ising$weiadj, labels = names(df), DoNotPlot = TRUE) %>%
      qgraph::centralityTable(standardized = FALSE) %>%
      dplyr::filter(measure == centrality_indices) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::select(node, value)
  }

  ## Check if nodes_to_influence is a vector of variable names
  if (all(nodes_to_influence %in% colnames(df))) {
    Position_in_the_df_of_the_modified_nodes <- which(names(df) %in% nodes_to_influence)
  } else {
    if (is.numeric(nodes_to_influence)) {
      ## Determine the quantile value based on nodes_to_influence
      quantile_value <- nodes_to_influence
      ## Get the threshold value
      threshold <- quantile(centrality_values_sorted$value, quantile_value)
      ## Get the highest centrality values
      if (relation == ">=") {
        highest_values <- centrality_values_sorted %>%
          dplyr::filter(value >= threshold)
      } else if (relation == "<=") {
        highest_values <- centrality_values_sorted %>%
          filter(value <= threshold)
      }
      Influenced_nodes <- highest_values$node
      ## Position of each variable in the data frame
      Position_in_the_df_of_the_modified_nodes <- which(names(df) %in% Influenced_nodes)
    }
  }

  ## Setting the level of threshold change
  modif <- sd(Fit_Ising$thresholds) * thresholds_IsingSampler

  ## Thresholds list
  if (Fit_Ising_thresholds) {
    list_thresholds <- Fit_Ising$thresholds
    list_thresholds[Position_in_the_df_of_the_modified_nodes] <- Fit_Ising$thresholds[Position_in_the_df_of_the_modified_nodes] + modif
  } else {
    ## Create a new vector with 0 values
    list_thresholds <- rep(0, length(Fit_Ising$thresholds))
    ## Replace the modified values with the updated values
    list_thresholds[Position_in_the_df_of_the_modified_nodes] <- Fit_Ising$thresholds[Position_in_the_df_of_the_modified_nodes] + modif
  }

  ## Linear transformation
  SimInput <- IsingSampler::LinTransform(Fit_Ising$weiadj, Fit_Ising$thresholds)

  ## Simulation of the sample(s) that can contain the -1/1 values
  sample <- IsingSampler::IsingSampler(n, SimInput$graph, thresholds = list_thresholds, beta = beta, responses = c(-1L, 1L), method = IsingSampler_method)
  sample_resc <- sample

  ## Recode the -1 by 0
  sample_resc[sample_resc == -1] <- 0

  ## Estimation of an Ising network
  Fit_sample <- IsingFit::IsingFit(sample_resc, plot = FALSE)

  ## Calculation of individual scores for each of the networks
  sum_score <- apply(sample, 1, sum)

  ## Calculation of mean, median, sd, min and max of each of the networks
  mean_score <- mean(sum_score)
  median_score <- median(sum_score)
  sd_score <- sd(sum_score)
  min_score <- min(sum_score)
  max_score <- max(sum_score)

  ## Create the output object based on the input parameters
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

  ## Assign the class to the output object
  class(easy_ising_simulations_obj) <- c("easy ising simulations obj")

  ## Return the output object
  return(easy_ising_simulations_obj)
}
