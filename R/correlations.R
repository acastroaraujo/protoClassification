#' Generate random correlation matrices from the LKJ distribution
#'
#' Generates random correlation matrices from the LKJ (Lewandowski-Kurowicka-Joe)
#' distribution. The LKJ distribution is a prior distribution over correlation
#' matrices that generalizes the uniform distribution over correlation matrices.
#' As eta increases, more probability mass is concentrated around the identity matrix.
#'
#' @param n Integer. Number of correlation matrices to generate
#' @param K Integer. Dimension of the correlation matrix (number of rows/columns).
#'   Must be at least 2
#' @param eta Numeric. Shape parameter controlling the concentration around the
#'   identity matrix. Must be positive. When eta = 1, the distribution is uniform
#'   over correlation matrices. As eta increases, more mass is placed on matrices
#'   closer to the identity matrix
#'
#' @return If \code{n = 1}, returns a single K×K correlation matrix.
#'   If \code{n > 1}, returns a list of \code{n} correlation matrices.
#'   All matrices are symmetric and positive definite with 1s on the diagonal.
#'
#' @export
#'
#' @source These functions come from Richard McElreath's \href{https://github.com/rmcelreath/rethinking/blob/master/R/distributions.r}{rethinking} package
#' and, judging by the source code, it seems that he got them from Ben Goodrich.
#'
#' @examples
#' # Generate a single 4x4 correlation matrix
#' rho1 <- rlkjcorr(n = 1, K = 4, eta = 1)
#' round(rho1, 2)
#'
#' # Generate 5 correlation matrices with more concentration around identity
#' rho_list <- rlkjcorr(n = 5, K = 3, eta = 4)
#' lapply(rho_list, round, 2)
#'
#' # Compare different eta values
#' rho_uniform <- rlkjcorr(1, 3, eta = 1)    # Uniform over correlations
#' rho_concentrated <- rlkjcorr(1, 3, eta = 10) # Concentrated near identity
#'
#' @references
#' Lewandowski, D., Kurowicka, D., & Joe, H. (2009). Generating random
#' correlation matrices based on vines and extended onion method.
#' Journal of multivariate analysis, 100(9), 1989-2001.
#'
#' @seealso \code{\link{transform_rho}}
#'
rlkjcorr <- function(n, K, eta = 1) {
  stopifnot(is.numeric(K), K >= 2, K == as.integer(K))
  stopifnot(eta > 0)

  f <- function() {
    alpha <- eta + (K - 2) / 2
    r12 <- 2 * stats::rbeta(1, alpha, alpha) - 1
    R <- matrix(0, K, K) # upper triangular Cholesky factor until return()
    R[1, 1] <- 1
    R[1, 2] <- r12
    R[2, 2] <- sqrt(1 - r12^2)
    if (K > 2) {
      for (m in 2:(K - 1)) {
        alpha <- alpha - 0.5
        y <- stats::rbeta(1, m / 2, alpha)

        # Draw uniformly on a hypersphere
        z <- stats::rnorm(m, 0, 1)
        z <- z / sqrt(crossprod(z)[1])

        R[1:m, m + 1] <- sqrt(y) * z
        R[m + 1, m + 1] <- sqrt(1 - y)
      }
    }
    return(crossprod(R))
  }

  R <- replicate(n, f(), simplify = FALSE)

  if (n == 1) {
    R <- R[[1]]
  }
  return(R)
}


#' Transform correlation matrix by setting specific correlations
#'
#' Modifies a correlation matrix by setting specific off-diagonal elements to
#' desired values, then adjusts the matrix to ensure it remains positive definite.
#' This is useful for creating correlation matrices with specific patterns while
#' maintaining mathematical validity.
#'
#' @param rho A correlation matrix
#' @param el A matrix with 3 columns specifying the transformations to apply:
#'   \describe{
#'     \item{Column 1}{Row indices (i) of correlations to modify}
#'     \item{Column 2}{Column indices (j) of correlations to modify}
#'     \item{Column 3}{New correlation values to set for rho[i,j]}
#'   }
#'   All correlation values must be between -1 and 1
#'
#' @return A transformed correlation matrix that is positive definite.
#'   The function automatically ensures symmetry (sets both rho[i,j] and rho[j,i])
#'   and uses \code{\link[Matrix]{nearPD}} to find the nearest positive definite matrix
#'   if the specified correlations make the matrix non-positive definite
#'
#' @export
#'
#' @details
#' The function first sets the specified correlations in both symmetric positions
#' of the matrix, then uses the \code{nearPD} function from the Matrix package
#' to find the nearest positive definite correlation matrix. Information about
#' the convergence is printed to the console.
#'
#' @examples
#' # Start with a random correlation matrix
#' rho <- rlkjcorr(n = 1, K = 4, eta = 1)
#' round(rho, 2)
#'
#' # Define specific correlations to set
#' el <- rbind(
#'   c(i = 1, j = 2, r = +0.7), # Set correlation between variables 1 and 2 to 0.7
#'   c(i = 3, j = 4, r = -0.5)  # Set correlation between variables 3 and 4 to -0.5
#' )
#'
#' # Transform the matrix
#' rho_new <- transform_rho(rho, el)
#' round(rho_new, 2)
#'
#' # Verify the specified correlations were set
#' rho_new[1, 2]  # Should be close to 0.7
#' rho_new[3, 4]  # Should be close to -0.5
#'
#'
#' @seealso \code{\link{rlkjcorr}}
#'
transform_rho <- function(rho, el) {
  stopifnot(ncol(el) == 3)
  stopifnot(all(el[, 3] >= -1 & el[, 3] <= 1))

  rho[el[, 1:2]] <- el[, 3]
  rho[el[, 2:1]] <- el[, 3]

  out <- Matrix::nearPD(rho, corr = TRUE, base.matrix = TRUE)
  message("iterations: ", out$iterations)
  message("converged: ", out$converged)

  return(out$mat)
}
