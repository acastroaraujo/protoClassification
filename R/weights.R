#' Temperature Scaling for Probability Distributions
#'
#' Applies temperature scaling to a probability distribution using the softmax function.
#'
#' @param w A numeric vector of probabilities that must sum to 1. All values must be
#'   between 0 and 1 (inclusive). The function will validate that the sum equals 1
#'   within floating-point precision (tolerance of 1e-10).
#' @param temp A single numeric value specifying the temperature parameter. Must be
#'   non-negative (>= 0).
#'   \itemize{
#'     \item \code{temp > 1}: Makes the distribution more uniform (smoother, less peaked)
#'     \item \code{temp < 1}: Makes the distribution less uniform (sharper, more peaked)
#'     \item \code{temp = 1}: No change to the original distribution
#'     \item \code{temp = 0}: Returns a vector with 1 at the position of the maximum probability and 0 otherwise
#'   }
#'
#' @return A numeric vector of the same length as \code{w} containing the temperature-scaled
#'   probabilities. The returned vector will sum to 1 and all values will be between 0 and 1.
#'
#' @export
#'
#' @examples
#'
#' w <- runif(4)
#' w <- w / sum(w)
#'
#' w
#'
#' temperature(w, temp = 0)
#' temperature(w, temp = 0.5)
#' temperature(w, temp = 1)
#'
temperature <- function(w, temp) {
  stopifnot(length(temp) == 1, temp >= 0)
  stopifnot(all(w >= 0 & w <= 1))
  stopifnot((sum(w) - 1) < 1e-10) # `sum(w) == 1` for floating point math

  if (temp == 0) {
    i <- which.max(w)
    w[] <- 0L
    w[i] <- 1L
    return(w)
  }

  ## Convert probabilities into log-probabilities
  x <- log(w)
  # Apply temperature
  xtemp <- x / temp
  # Apply `SoftMax` with numerical stability
  out <- exp(xtemp - max(xtemp))
  return(out / sum(out))
}
