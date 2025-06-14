#' Make Correlated Binary Data
#'
#' Generates correlated binary data. The function first generates multivariate
#' normal data with specified correlations, then transforms it to binary data while preserving the
#' correlation structure. Apparently this is known as a "Gaussian copula" approach.
#'
#' @param marginals A numeric vector of marginal probabilities for each variable.
#' @param rho A symmetric correlation matrix with dimensions matching the length
#'   of \code{marginals}
#' @param obs Integer. Number of observations (rows) to generate.
#'
#' @return A \code{prototypeData} object containing:
#'   \describe{
#'     \item{Binary data}{A data frame with \code{obs} rows and \code{length(marginals)} columns}
#'     \item{params attribute}{List containing the original marginals and correlation matrix}
#'   }
#'
#' @seealso \code{\link{get_data_params}}, \code{\link{print.prototypeData}}
#'
#' @examples
#' # Generate 8-dimensional correlated binary data
#' K <- 8
#' marginals <- rbeta(K, 2, 3)
#' rho <- rlkjcorr(1, K, eta = 1 / 4)
#' out <- make_binary_data(marginals, rho)
#' out
#'
#' @export
#'
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

#' Print method for prototypeData objects
#'
#' Displays a formatted summary of a prototypeData object, showing the
#' data structure and the parameters used to generate it (marginal
#' probabilities and correlation matrix).
#'
#' @param x A prototypeData object created by \code{\link{make_binary_data}}
#' @param digits Integer. Number of decimal places to display for numeric values (default: 2)
#' @param ... Currently unused.
#'
#' @method print prototypeData
#' @export
#'
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
  invisible(x)
}


#' Extract parameters from prototypeData object
#'
#' Retrieves the parameters (marginal probabilities and correlation matrix)
#' that were used to generate a prototypeData object.
#'
#' @param x A prototypeData object created by \code{\link{make_binary_data}}
#'
#' @return A list with two components:
#'   \describe{
#'     \item{marginals}{Named numeric vector of marginal probabilities}
#'     \item{rho}{Correlation matrix used to generate the data}
#'   }
#'
#' @export
#'
#' @seealso \code{\link{make_binary_data}}, \code{\link{bivariateCondProb}}
#'
get_data_params <- function(x) {
  stopifnot(inherits(x, "prototypeData"))
  attr(x, "params", exact = TRUE)
}


#' Extract theoretical conditional probabilities for bivariate case
#'
#' Computes the theoretical conditional probabilities P(X_j = 1 | C = c1)
#' for all variables j when attention is focused entirely on variable k*.
#' In this case, we have that P(X_j = 1 | C = c1) = P(X_j = 1 | X_k* = 1)
#' This uses the bivariate normal distribution to compute exact conditional
#' probabilities based on the correlation structure.
#'
#' @param parameters A list containing marginal probabilities and correlation matrix,
#'   as returned by \code{\link{get_data_params}}.
#' @param kstar Integer. The index of the variable receiving all attention
#'   (must be between 1 and the number of variables)
#'
#' @return A named numeric vector of conditional probabilities P(X_j = 1 | X_k* = 1)
#'   for all variables j, where the variable at position \code{kstar} will have
#'   probability 1 (since X_k* = 1 is the conditioning event)
#'
#' @export
#'
#' @seealso \code{\link{get_data_params}}, \code{\link{make_binary_data}}
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

    joint_prob <- mvtnorm::pmvnorm(
      lower = c(-Inf, -Inf),
      upper = c(stats::qnorm(pj), stats::qnorm(pstar)),
      sigma = rbind(c(1, rhojk), c(rhojk, 1))
    )
    attributes(joint_prob) <- NULL
    joint_prob / pstar
  })

  names(output) <- names(parameters$marginals)
  return(output)
}

