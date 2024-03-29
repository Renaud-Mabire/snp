PottsFit <- function(data, possible_responses = c(1, 2, 3),
                     alphaSeq = 1, nfolds = 10, nlambda = 100,
                     progressbar = TRUE, plot = TRUE, ...) {
  # --------------------------------------------------
  # --------------- Control Conditions ---------------
  # --------------------------------------------------
  if (!is.data.frame(data)) {
    stop("Error: The 'data' argument must be a dataframe.")
  }

  # Check that 'data' does not contain NA values
  if (any(is.na(data))) {
    stop("Error: 'data' should not contain NA values.")
  }

  # Check that all responses are contained in 'possible_responses'
  if (!all(unlist(data) %in% possible_responses)) {
    stop(paste(
      "Error: Some responses in 'data' are not contained in 'possible_responses'.",
      "Please verify that all variables in your dataframe have the same range of possible responses.",
      "The expected possible responses are: ", paste(possible_responses, collapse = ", "),
      ". If your data has a different range of possible responses, please specify that by passing an appropriate vector to the 'possible_responses' argument."
    ))
  }

  if (!is.numeric(alphaSeq) || any(alphaSeq < 0) || any(alphaSeq > 1)) {
    stop("Error: The 'alphaSeq' argument must be a positive number between 0 and 1.")
  }

  if (!is.numeric(nfolds) || nfolds <= 0 || nfolds != round(nfolds)) {
    stop("Error: The 'nfolds' argument must be a positive integer.")
  }

  # Check that 'nfolds' is less or equal to the number of rows in 'data'
  if (nfolds > nrow(data)) {
    stop("Error: 'nfolds' must be less or equal to the number of observations in 'data'.")
  }

  if (!is.logical(progressbar)) {
    stop("Error: The 'progressbar' argument must be a boolean (TRUE or FALSE).")
  }


  # -------------------------------------------------------------
  # --------------- Part 1: Initialize Parameters ---------------
  # -------------------------------------------------------------

  K <- length(possible_responses)

  # Initialize J as a 4D array
  J <- array(0, dim = c(ncol(data), ncol(data), K, K))
  #Here's how each dimension of J is interpreted in the code above:
  #The first dimension corresponds to the response variable (the variable for which the regression model has been fitted).
  # The second dimension corresponds to the predictor variable (the variable whose coefficient is being stored).
  # The third dimension corresponds to the level of the response variable for which the coefficient has been calculated.
  # The fourth dimension corresponds to the level of the predictor variable for which the coefficient has been calculated.

  # Initialize h as a matrix of zeros with a row for each variable and a column for each level
  h <- matrix(0, nrow = ncol(data), ncol = K)


  # ----------------------------------------------------------------------------------------------
  # --------------- Part 2: Incorporate Regularized Multinomial Regression (Lasso) ---------------
  # ----------------------------------------------------------------------------------------------

  # Get all variable names
  variables <- names(data)

  # Check that each variable exists in 'data'
  if (any(!variables %in% names(data))) {
    stop(paste(
      "Error: All specified variables must exist in 'data'. The following variables are missing: ",
      paste(variables[!variables %in% names(data)], collapse = ", ")
    ))
  }

  # Create a list to store all models
  models <- list()

  # If Tbar is true, create a progress bar
  if (progressbar) {
    pb <- txtProgressBar(min = 0, max = length(variables), style = 3)
  }

  # Loop over each variable in our dataset.
  # Each iteration of the loop will create a regression model for a different variable.
  for (var in variables) {
    # Generate the formula string
    tryCatch(
      {
        formula_str <- sprintf("%s ~ %s", var, paste(setdiff(variables, var), collapse = " + ")) # The formula string is of the form "var ~ var1 + var2 + var3 + ..."
      },
      error = function(e) {
        stop(paste("An error occurred while creating the formula for the variable", var, ": ", e$message))
      }
    )

    # Convert the formula string into a real formula with as.formula()
    formula_str <- as.formula(formula_str) # Convert the formula string into a real formula with as.formula()

    # Create a model matrix
    # Create a model matrix we use the formula string to create a model matrix.
    # model.matrix takes a formula and a dataset, and creates a matrix where each column corresponds to an independent variable in the formula, and each row corresponds to an observation in the dataset.
    # The [, -1] option is used to exclude the first column from the matrix, which is an intercept column (a 1s column) that we don't need for glmnet.
    tryCatch(
      {
        x <- model.matrix(formula_str, data)[, -1] # Exclude the intercept column (the first column) from the matrix because we don't need it for glmnet 
      },
      error = function(e) {
        stop(paste("An error occurred while creating the model matrix for the variable", var, ": ", e$message))
      }
    )

    # Set the dependent variable
    y <- data[[var]] 


    tryCatch(
      {
        # Perform regularized multinomial regression
        cvfit <- cv.glmnet(x, y, alpha = alphaSeq, nfolds = nfolds, nlambda = nlambda, family = "multinomial") # Perform regularized multinomial regression
      },
      error = function(e) {
        stop(paste("An error occurred when calling cv.glmnet for the variable", var, ": ", e$message))
      }
    )

    # Store our model in the list of models
    models[[var]] <- cvfit 

  }



  if (progressbar) {
    setTxtProgressBar(pb, which(variables == var))
  }

  if (progressbar) {
    close(pb)
  }


  # -------------------------------------------------------------------
  # --------------- Part 3: Parameter recovery J & h ---------------
  # -------------------------------------------------------------------


  # Find J ---------------------------------------------------------------

  # Loop over each response variable to get the coefficients
  for (i in seq_along(variables)) { # i corresponds to the response variable
    var <- variables[i]

    # Find the lambda that minimizes the cross-validation error
    lambda_min <- models[[var]]$lambda.min

    # Retrieves and assigns the model coefficients (beta) for the specific variable 'var' of the fitted glmnet model, stored in the 'models' list
    beta <- models[[var]]$glmnet.fit$beta

    # Find the s corresponding to this lambda
    s_min <- 1 / lambda_min

    # Find the column of beta corresponding to this s
    s_min_col <- which.min(abs(models[[var]]$glmnet.fit$lambda - s_min))

    # Convert s_min_col to a string, subtract 1 because s0 corresponds to the first column, s1 to the second, etc.
    # subtract 1 because s0 corresponds to the first column, s1 to the second, etc.
    s_min_col_str <- paste0("s", s_min_col - 1)


    # Get the coefficients for each level of the response variable and each predictor variable
    for (k in seq_along(beta)) { # k corresponds to the level of the response variable
      for (l in seq_along(variables)) { # l corresponds to the predictor variable
        if (i != l) {
          # Get the coefficients for all levels of the predictor variable at s_min_col
          coefs <- as.matrix(beta[[k]])[, s_min_col_str, drop = FALSE]

          for (j in seq_along(coefs)){
            # Store the coefficients for all levels of the predictor variable
            J[i, l, k, ] <- coefs[[j]]
          }
        }
      }
    }
  }

  # To have h ---------------------------------------------------------------

  for (i in seq_along(variables)) {
    var <- variables[i]  # variable courante

    # Trouver la valeur de lambda qui minimise l'erreur de validation croisée.
    lambda_min <- models[[var]]$lambda.min

    # Convertir cette valeur de lambda en s.
    s_min <- 1 / lambda_min

    # Trouver la colonne de a0 qui correspond à cette valeur de s.
    s_min_col <- which.min(abs(models[[var]]$glmnet.fit$lambda - s_min))

    # Obtenir les intercepts correspondant à cette valeur de s.
    intercepts <- models[[var]]$glmnet.fit$a0[, s_min_col]

    # Store these intercepts in h. It's no longer necessary to take the average, as we have a location for each level. # nolint: line_length_linter.
    h[i, ] <- intercepts
  }



  # -------------------------------------------------------------------
  # -------- Part 4: Calculation of the average weight matrix ---------
  # -------------------------------------------------------------------

  # Initialize J_mean as a matrix of zeros with a row for each variable and a column for each variable
  # Initialize J_mean as a matrix of zeros with a row for each variable and a column for each variable
  J_mean <- matrix(0, nrow = length(variables), ncol = length(variables))

  # Iterate over each pair of variables
  for (i in seq_along(variables)) {
    for (j in seq_along(possible_responses)) {
      # Skip the case where i == j, as J[i, , , m] would be empty
      if (i != j) {
        # Calculate the mean of all coefficients between this pair of variables
        J_mean[i, j] <- mean(J[i, , , j])
      }
    }
  }


  # -------------------------------------------------------------
  # --------------- Part 5: Network Visualization ---------------
  # -------------------------------------------------------------
  # Plot the network if plot = TRUE
  if (plot) {

    qgraph::qgraph(J_mean,
                   labels = names(data),
                   layout = "spring",
                   ...)

  }
  message("The graph represents an aggregation of the average weights of the Potts model.")

  # ------------------------------------------------------
  # --------------- Part 6: Return Results ---------------
  # ------------------------------------------------------

  # Create a list with J, h, J_mean, and glmnet models
  result <- list(J = J, h = h, J_mean = J_mean, models = models)

  # Assign the "pottsFit" class to the object
  class(result) <- "PottsFit"

  # Return the result
  return(result)

}
