% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Shanon_Ent.R
\name{shannon_entropy_weights}
\alias{shannon_entropy_weights}
\title{Shannon Entropy Weights}
\usage{
shannon_entropy_weights(data)
}
\arguments{
\item{data}{A data frame for which weights are to be calculated.}
}
\value{
A vector of Shannon entropy weights.
}
\description{
This function calculates Shannon entropy weights for the columns of a data frame.
}
\examples{
data <- data.frame(ID = 1:5, A = c(0.2, 0.3, 0.1, 0.4, 0.0), B = c(0.5, 0.1, 0.2, 0.1, 0.1))
entropy_weights <- shannon_entropy_weights(data)
}
