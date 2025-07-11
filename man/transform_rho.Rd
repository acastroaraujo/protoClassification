% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlations.R
\name{transform_rho}
\alias{transform_rho}
\title{Transform correlation matrix by setting specific correlations}
\usage{
transform_rho(rho, el)
}
\arguments{
\item{rho}{A correlation matrix}

\item{el}{A matrix with 3 columns specifying the transformations to apply:
\describe{
  \item{Column 1}{Row indices (i) of correlations to modify}
  \item{Column 2}{Column indices (j) of correlations to modify}
  \item{Column 3}{New correlation values to set for rho[i,j]}
}
All correlation values must be between -1 and 1}
}
\value{
A transformed correlation matrix that is positive definite.
  The function automatically ensures symmetry (sets both rho[i,j] and rho[j,i])
  and uses \code{\link[Matrix]{nearPD}} to find the nearest positive definite matrix
  if the specified correlations make the matrix non-positive definite
}
\description{
Modifies a correlation matrix by setting specific off-diagonal elements to
desired values, then adjusts the matrix to ensure it remains positive definite.
This is useful for creating correlation matrices with specific patterns while
maintaining mathematical validity.
}
\details{
The function first sets the specified correlations in both symmetric positions
of the matrix, then uses the \code{nearPD} function from the Matrix package
to find the nearest positive definite correlation matrix. Information about
the convergence is printed to the console.
}
\examples{
# Start with a random correlation matrix
rho <- rlkjcorr(n = 1, K = 4, eta = 1)
round(rho, 2)

# Define specific correlations to set
el <- rbind(
  c(i = 1, j = 2, r = +0.7), # Set correlation between variables 1 and 2 to 0.7
  c(i = 3, j = 4, r = -0.5)  # Set correlation between variables 3 and 4 to -0.5
)

# Transform the matrix
rho_new <- transform_rho(rho, el)
round(rho_new, 2)

# Verify the specified correlations were set
rho_new[1, 2]  # Should be close to 0.7
rho_new[3, 4]  # Should be close to -0.5


}
\seealso{
\code{\link{rlkjcorr}}
}
