#' Make Binary Data
#'
#' @param marginals a K-sized vector of marginal probabilities
#' @param rho a correlation matrix
#' @param obs number of rows in the dataset
#'
#' @returns a matrix of simulated observations
#' @export
#'
#' @examples
#' K <- 8
#' marginals <- rbeta(K, 2, 3)
#' rho <- rlkjcorr(1, K, eta = 1 / 4)
#' out <- make_binary_data(marginals, rho)
#' str(out)
make_binary_data <- function(marginals, rho, obs = 1e3) {
  stopifnot(obs > 0)
  stopifnot(isSymmetric(rho))

  mu <- stats::qnorm(marginals)
  names(mu) <- names(marginals)
  colnames(rho) <- rownames(rho) <- names(marginals)
  S <- diag(length(mu)) %*% rho %*% diag(length(mu))
  out <- mvtnorm::rmvnorm(obs, mean = mu, sigma = S)
  out <- stats::pnorm(out) > 0.5
  out[] <- as.integer(out)
  return(out)
}
