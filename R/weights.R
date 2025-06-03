#' Temperature Scaling
#'
#' @param w numeric vector of probabilities (must sum to 1)
#' @param temp numeric, temperature parameter
#'   - temp > 1: makes distribution more uniform
#'   - temp < 1: makes distribution less uniform
#'   - temp = 1: no change
#'
#' @return numeric vector of temperature-scaled probabilities
#' @export
#'
#' @examples
#'
#' w <- runif(10)
#' w <- w / sum(w)
#'
#' temperature(w, temp = 0)
#' temperature(w, temp = 0.5)
#' temperature(w, temp = 1)
#'
temperature <- function(w, temp = 1) {
  stopifnot(length(temp) == 1, temp >= 0)
  stopifnot(all(w >= 0 & w <= 1))
  stopifnot((sum(w) - 1) < 1e-12) # `sum(w) == 1` for floating point math

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
