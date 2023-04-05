\name{hist_sumscore}
\alias{hist_sumscore}
\title{Plot histograms of sum scores for each intervention}
\description{
This function plots histograms of sum scores for each intervention. It uses ggplot2 to create a facet grid, allowing users to visualize the distribution of sum scores across all interventions. Optionally, mean and median values can be displayed on the plot.
}
\usage{
hist_sumscore(simulations,
              display_mean = FALSE,
              display_median = FALSE,
              theme = ggthemes::theme_solarized())
}
\arguments{
\item{simulations}{
A list containing simulation results. The list should have a structure similar to the output of the \code{snpgen()} function.
}
\item{display_mean}{
A logical value. If TRUE, mean values of sum scores will be displayed on the plot for each intervention. Default is FALSE.
}
\item{display_median}{
A logical value. If TRUE, median values of sum scores will be displayed on the plot for each intervention. Default is FALSE.
}
\item{theme}{
A ggplot2 theme to be used for the plot. Default is \code{ggthemes::theme_solarized()}.
}
}
\details{
This function is designed to help visualize the distribution of sum scores for each intervention in the given simulations. It creates histograms for each intervention using ggplot2, and displays them in a facet grid. This allows users to easily compare the distribution of sum scores across different interventions.

Optionally, users can choose to display mean and/or median values of sum scores for each intervention directly on the plot. This can help provide additional context when comparing the distributions.
}
\value{
The function returns a ggplot2 object, which can be further customized if desired or simply printed to display the plot.
}
\author{
%% ~put your name and email here ~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
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
