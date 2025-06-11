#' Make Correlated Binary Data
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
#' head(out)
#' colMeans(out)
#' marginals
make_binary_data <- function(marginals, rho, obs = 1e3) {
  stopifnot(obs > 0)
  stopifnot(isSymmetric.matrix(rho))

  if (is.null(names(marginals))) {
    names(marginals) <- paste0("x", 1:length(marginals))
  }

  mu <- stats::qnorm(marginals)
  names(mu) <- names(marginals)
  dimnames(rho) <- list(names(marginals), names(marginals))
  out <- mvtnorm::rmvnorm(obs, mean = mu, sigma = rho)
  out <- stats::pnorm(out) > 0.5
  out[] <- as.integer(out)
  out <- as.data.frame(out)
  structure(
    out,
    class = c("prototypeData", class(out)),
    params = list(marginals = marginals, rho = rho)
  )
}

#' @export
#'
print.prototypeData <- function(x, digits = 2, ...) {
  output <- utils::capture.output(utils::str(x, give.attr = FALSE))
  output <- gsub(".*?\\t", "", output)
  params <- attr(x, "params")

  cli::cli_h2("Data")
  cat(output, sep = "\n")
  cli::cli_text("")
  cli::cli_h2("Parameters")
  cli::cli_h3("Marginal Probabilities:")
  print(round(params$marginals, digits))
  cli::cli_text("")
  cli::cli_h3("Correlation Matrix:")
  print(round(params$rho, digits))
}


#' Extract parameters from `prototypeData` object
#'
#' @param x a `prototypeData` object
#'
#' @returns a list with (1) the marginal probabilities of x and (2) the correlation matrix of x
#' @export
#'
get_params <- function(x) {
  stopifnot(inherits(x, "prototypeData"))
  attr(x, "params", exact = TRUE)
}


#' Extract Theoretical Conditional Probabilities when all attention is set on kstar
#'
#' @param parameters an objection as created by `get_params()`
#' @param kstar the dimension k with all attention
#'
#' @returns a vector of conditional probabilities
#' @export
#'
bivariateCondProb <- function(parameters, kstar) {
  stopifnot(names(parameters) == c("marginals", "rho"))
  stopifnot(length(parameters) == 2)
  stopifnot(kstar %in% seq_along(parameters$marginals))

  correlations <- parameters$rho[kstar, ]
  pstar <- parameters$marginals[[kstar]]

  output <- purrr::map_dbl(seq_along(parameters$marginals), function(j) {

    rhojk <- correlations[[j]]
    pj <- parameters$marginals[[j]]

    # P(Xj = 1 | Xk* = 1) = P(Xj = 1, Xk* = 1) / P(Xk* = 1)
    joint_prob <- mvtnorm::pmvnorm(
      lower = c(stats::qnorm(1 - pj), stats::qnorm(1 - pstar)),
      upper = c(Inf, Inf),
      sigma = rbind(c(1, rhojk), c(rhojk, 1))
    )
    attributes(joint_prob) <- NULL
    joint_prob / pstar
  })

  names(output) <- names(parameters$marginals)
  return(output)
}
