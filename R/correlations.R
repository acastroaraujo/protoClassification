
#' LKJ distribution
#'
#' @source These functions come from Richard McElreath's `rethinking` package
#' and, judging by the source code, it seems that he got them from Ben Goodrich.
#'
#' @param n number of draws
#' @param K number of rows and columns in the matrix
#' @param eta prior, as eta increases more mass is placed over identity matrices
#'
#' @returns a matrix or list of matrices
#' @export
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
    if (K > 2)
      for (m in 2:(K - 1)) {
        alpha <- alpha - 0.5
        y <- stats::rbeta(1, m / 2, alpha)

        # Draw uniformly on a hypersphere
        z <- stats::rnorm(m, 0, 1)
        z <- z / sqrt(crossprod(z)[1])

        R[1:m, m + 1] <- sqrt(y) * z
        R[m + 1, m + 1] <- sqrt(1 - y)
      }
    return(crossprod(R))
  }

  R <- replicate(n, f(), simplify = FALSE)

  if (n == 1) {
    R[[1]]
  } else {
    R
  }
}


dlkjcorr <- function(x, eta = 1, log = TRUE) {
  ll <- det(x)^(eta - 1)
  if (log == TRUE) ll <- log(ll)
  return(ll)
}
