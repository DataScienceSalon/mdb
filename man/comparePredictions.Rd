% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparePredictions.R
\name{comparePredictions}
\alias{comparePredictions}
\title{comparePredictions}
\usage{
comparePredictions(mods, test)
}
\arguments{
\item{mods}{List containing linear models used to perform predictions}

\item{test}{Data frame containing the movies data set}
}
\value{
Data frame with regression summary statistics for each model
}
\description{
\code{comparePredictions} Performs predictions on the list of regression
models and returns the summary statistics
}
\seealso{
Other movies functions: \code{\link{associate}},
  \code{\link{backwardElimination}}, \code{\link{back}},
  \code{\link{challenger}}, \code{\link{correlate}},
  \code{\link{createDependent}},
  \code{\link{evalForwardModel}}, \code{\link{forward}},
  \code{\link{prediction}}, \code{\link{preprocess}},
  \code{\link{process}}, \code{\link{regressionAnalysis}},
  \code{\link{univariateQual}},
  \code{\link{univariateQuant}}, \code{\link{univariate}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
