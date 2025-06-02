
#' Make Data
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
#' out <- make_data(marginals, rho)
#' str(out)
make_data <- function(marginals, rho, obs = 1e3) {
  mu <- stats::qnorm(marginals)
  names(mu) <- names(marginals)
  colnames(rho) <- rownames(rho) <- names(marginals)
  S <- diag(length(mu)) %*% rho %*% diag(length(mu))
  out <- mvtnorm::rmvnorm(obs, mean = mu, sigma = S)
  out <- round(stats::pnorm(out)) ## equivalent to pnorm(out) > 0.5
  out
}


#' Get all size K permutations of `0` and `1`
#'
#' @param K number of dimensions
#'
#' @returns a data frame of permutations
#' @export
#'
#' @examples
#' all_permutations(3)
all_permutations <- function(K) {
  stopifnot(K > 1)
  stopifnot(length(K) == 1)
  arrangements::permutations(0:1, K, replace = TRUE)
}




