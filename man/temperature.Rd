% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weights.R
\name{temperature}
\alias{temperature}
\title{Temperature Scaling for Probability Distributions}
\usage{
temperature(w, temp)
}
\arguments{
\item{w}{A numeric vector of probabilities that must sum to 1. All values must be
between 0 and 1 (inclusive). The function will validate that the sum equals 1
within floating-point precision (tolerance of 1e-10).}

\item{temp}{A single numeric value specifying the temperature parameter. Must be
non-negative (>= 0).
\itemize{
  \item \code{temp > 1}: Makes the distribution more uniform (smoother, less peaked)
  \item \code{temp < 1}: Makes the distribution less uniform (sharper, more peaked)
  \item \code{temp = 1}: No change to the original distribution
  \item \code{temp = 0}: Returns a vector with 1 at the position of the maximum probability and 0 otherwise
}}
}
\value{
A numeric vector of the same length as \code{w} containing the temperature-scaled
  probabilities. The returned vector will sum to 1 and all values will be between 0 and 1.
}
\description{
Applies temperature scaling to a probability distribution using the softmax function.
}
\examples{

w <- runif(4)
w <- w / sum(w)

w

temperature(w, temp = 0)
temperature(w, temp = 0.5)
temperature(w, temp = 1)

}
