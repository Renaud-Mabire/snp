\name{PottsFit}
\alias{PottsFit}
\title{Function to fit a Potts model using regularized multinomial regression (Lasso)}
\description{
This function fits a Potts model to a given dataset using regularized multinomial regression.
}
\usage{
PottsFit(data, possible_responses = c(1, 2, 3),
         alphaSeq = 1, nfolds = 10, nlambda = 100,
         progressbar = TRUE, plot = TRUE, ...)
}
\arguments{
  \item{data}{
     The input dataframe.
  }
  \item{possible_responses}{
     A list of possible responses.
  }
  \item{alphaSeq}{
     Regularization parameter.
  }
  \item{nfolds}{
     The number of folds for cross-validation.
  }
  \item{nlambda}{
     The number of lambda values to be tested.
  }
  \item{progressbar}{
     A Boolean to display a progress bar.
  }
  \item{plot}{
     A Boolean to display a graph.
  }
  \item{...}{
     Possible arguments for qgraph
  }
}
\details{
This function first checks input conditions, then initializes the model parameters (the J tensor and the h vector), and fits a regularized multinomial regression to each variable in the dataframe, storing the results in a list of models. Finally, it calculates the coefficients and intercept terms, the average weights matrix, creates a network visualization if "plot" is TRUE, and returns a "PottsFit" class object.
}
\value{
  A list with:
  \item{J }{The J tensor which stores the regression coefficients.}
  \item{h }{The h vector which stores the intercept terms.}
  \item{average weights matrix }{Calculated from J.}
  \item{list of models }{A list of fitted models for each variable in the dataframe.}
}
\references{
None at this time.
}
\author{
  Renaud MABIRE-YON
}
\note{
  ...
}
\seealso{
\code{\link{glmnet}}, \code{\link{cv.glmnet}}, \code{\link{qgrpah}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
# Insert examples here
}
\keyword{ regression }
\keyword{ Potts model }
\concept{ lasso }
