\name{compare_centrality_snp}
\alias{compare_centrality_snp}
\title{Compare centrality measures across networks from snp_Ising simulations}
\description{
  This function takes the output of snp_Ising simulations and compares the centrality measures across networks. The user can specify which centrality measures to include, the order in which they should be displayed, and provide custom names for the networks.
}
\usage{
compare_centrality_snp(simulations,
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
                        networkNames = NULL)
}
\arguments{
  \item{simulations}{
    A list of snp_Ising simulation outputs.
  }
  \item{include}{
    A character vector specifying the centrality measures to include in the comparison. Defaults to all centrality measures: Strength, Closeness, Betweenness, and ExpectedInfluence.
  }
  \item{orderBy}{
    A character vector specifying the order in which the centrality measures should be displayed. Defaults to the same order as the 'include' argument.
  }
  \item{legendName}{
    A character string specifying the name of the legend. Defaults to an empty string.
  }
  \item{networkNames}{
    A character vector providing custom names for the networks. If NULL (default), network names are automatically generated as "Network 1", "Network 2", etc.
  }
}
\value{
  A ggplot object displaying the comparison of centrality measures across networks.
}
\examples{
\dontrun{
  # Generate snp_Ising simulations (not provided)

  # Compare centrality measures across networks
  compare_centrality_snp(simulations, include = c("Strength", "Closeness"))
}
}
\author{Your Name}
\seealso{
  \code{\link{ggplot2}},
  \code{\link{forcats}}
}
\keyword{networks}
\keyword{centrality}
