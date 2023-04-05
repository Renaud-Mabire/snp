\name{easy_ising_simulations}
\alias{easy_ising_simulations}
\title{Perform Ising Model Simulations with Easy Modification of Thresholds}
\description{
The \code{easy_ising_simulations} function performs Ising model simulations with the option to easily modify the thresholds of specific nodes based on their centrality indices. It can be used to simulate various scenarios and analyze the effects of threshold modifications on the overall network.
}
\usage{
easy_ising_simulations(df,
                       centrality_indices = NULL,
                       nodes_to_influence,
                       relation = NULL,
                       Fit_Ising = NULL,
                       Fit_Ising_thresholds = FALSE,
                       n,
                       thresholds_IsingSampler,
                       beta = 1)
}
\arguments{
\item{df}{A data frame containing the input data for the Ising model.}
\item{centrality_indices}{A character string specifying the centrality index to use for selecting nodes to influence. Acceptable values include "Betweenness", "Closeness", "Strength", and "ExpectedInfluence". Default is NULL.}
\item{nodes_to_influence}{A numeric vector or a character vector specifying the nodes to influence based on their centrality indices. If numeric, it should have a single value representing the quantile of the centrality values to select. If character, it should contain the names of one or more variables in the input data frame.}
\item{relation}{A character string specifying the relation to use when selecting nodes based on their centrality indices. Acceptable values include ">=" or "<=". Default is NULL.}
\item{Fit_Ising}{An object created by the \code{IsingFit} function. If NULL, the function will create a new IsingFit object based on the input data frame. Default is NULL.}
\item{Fit_Ising_thresholds}{A logical value indicating whether to use the original thresholds from the Fit_Ising object. If FALSE, a new vector with modified thresholds will be created. Default is FALSE.}
\item{n}{A numeric value specifying the number of samples to simulate. Must be greater than 1.}
\item{thresholds_IsingSampler}{A numeric value indicating the extent of modification applied to the threshold level. This value can be a positive or negative integer, representing the number of standard deviations added or subtracted from the original threshold. For instance, a value of 1 signifies the addition of one standard deviation to the existing threshold, whereas a value of -0.5 denotes the removal of half a standard deviation from the original threshold.}
\item{beta}{A single numeric value representing the inverse temperature parameter for the Ising model. Default is 1.}
}
\details{
This function performs simulations of the Ising model by first estimating an Ising network based on the input data frame using the \code{IsingFit} function. The user can modify the thresholds of specific nodes in the network based on their centrality indices and run the simulation to observe the effects of these modifications on the overall network.

The function returns an object of class "easy ising simulations obj" containing various information about the simulation, such as the centrality values sorted by the chosen centrality index, the influenced nodes, the list of modified thresholds, the IsingFit object for the simulated sample, and summary statistics of the simulated sample.
}
\value{
A list of class "easy ising simulations obj" with the following components:
\describe{
\item{centrality_values_sorted}{A data frame containing the centrality values sorted by the chosen centrality index. Available only if nodes_to_influence is numeric.}
\item{Influenced_nodes}{A character vector containing the names of the influenced nodes. Available only if nodes_to_influence is numeric.}
\item{list_thresholds}{A numeric vector containing the modified thresholds.}
\item{Position_in_the_df_of_the_modified_nodes}{A numeric vector containing the positions of the modified nodes within the input data frame.}
\item{Fit_Ising_simulated_sample}{An object created by the \code{IsingFit} function for the simulated sample.}
\item{summary_simulated_sample}{A list containing summary statistics of the simulated sample, such as mean, standard deviation, minimum, and maximum values for each variable.}
}
}
\references{
Ref
}
\author{
Renaud Mabire-Yon (he/him/his) renaud.mabire.yon@gmail.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{

## Load necessary libraries ##
library('IsingFit')
library('IsingSampler')
library('qgraph')
library('dplyr')

## Simulate dataset ##
## Input:
N <- 10 # Number of nodes
nSample <- 1000 # Number of samples

## Ising parameters:
Graph <- matrix(sample(0:1, N^2, TRUE, prob = c(0.8, 0.2)), N, N) * runif(N^2, 0.5, 2)
Graph <- pmax(Graph, t(Graph))
diag(Graph) <- 0
Thresh <- -rowSums(Graph) / 2

## Simulate:
Data <- IsingSampler(nSample, Graph, Thresh) %>% as.data.frame()

## Run the easy_ising_simulations function with the following parameters:
# - Use the simulated Data data frame as input
# - Use the "Betweenness" centrality index for selecting nodes
# - Influence the top 10% of nodes based on the centrality index
# - Use the ">=" relation for selecting nodes
# - Create a new IsingFit object for the input data frame
# - Modify the thresholds of the selected nodes by one standard deviation
# - Generate 500 samples for the simulation
# - Use the default beta value of 1

results <- easy_ising_simulations(df = Data,
                                  centrality_indices = "Strength",
                                  nodes_to_influence = 0.9,
                                  relation = ">=",
                                  Fit_Ising = NULL,
                                  Fit_Ising_thresholds = FALSE,
                                  n = 500,
                                  thresholds_IsingSampler = 1,
                                  beta = 1)

Examine the results object to analyze the effects of threshold modifications
on the overall network and view the summary statistics of the simulated sample
results.



}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
