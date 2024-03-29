% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_threshold.R
\name{find_threshold}
\alias{find_threshold}
\title{Find Outlier Threshold}
\usage{
find_threshold(
  outlier_score,
  alpha = 0.01,
  outtail = c("max", "min"),
  p = 0.5,
  tn = 50
)
}
\arguments{
\item{outlier_score}{A vector of outlier scores. Can be a named vector or a vector with no names.}

\item{alpha}{Threshold for determining the cutoff for outliers. Observations are considered
outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
distances between exemplars.}

\item{outtail}{Direction of the outlier tail.}

\item{p}{Proportion of possible candidates for outliers. This defines the starting point for the
bottom up searching algorithm.}

\item{tn}{Sample size to calculate an empirical threshold}
}
\value{
The indexes (or names, if the input is named vactor) of the observations determined to be outliers.
}
\description{
Find Outlier Threshold
}
