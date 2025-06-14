


#' Extract theoretical conditional probabilities for the rule-based case
#' with exclusive attention on one feature.
#'
#' Computes the theoretical conditional probabilities P(X_j = 1 | C = c1)
#' for all variables j when attention is focused entirely on variable k*.
#' In this case, we have that P(X_j = 1 | C = c1) = P(X_j = 1 | X_k* = 1)
#' This uses the bivariate normal distribution to compute exact conditional
#' probabilities based on the correlation structure.
#'
#' @param pxk Numeric. Marginal probability P(X_k = 1) for variable k.
#' @param pxkstar Numeric. Marginal probability P(X_k* = 1) for the attention variable k*.
#' @param corr Numeric. Correlation between variables Xk and Xk*.
#'
#' @return Numeric value representing the conditional probability P(X_k = 1 | X_k* = 1)
#'
#' @details The function computes the joint probability P(X_k = 1, X_k* = 1) using
#'   the bivariate normal distribution with the given correlation structure, then
#'   divides by P(X_k* = 1) to obtain the conditional probability.
#'
#' @export
#'
#' @examples
#' # Compute conditional probability for positively correlated variables
#' ruleStarCondProb(pxk = 0.3, pxkstar = 0.4, corr = 0.5)
#'
#' # Compare with negatively correlated variables
#' ruleStarCondProb(pxk = 0.3, pxkstar = 0.4, corr = -0.5)
#'
ruleStarCondProb <- function(pxk, pxkstar, corr) {
  stopifnot(corr < 1 & corr > -1)
  stopifnot(pxk < 1 & pxk > 0)
  stopifnot(pxkstar < 1 & pxkstar > 0)

  joint_prob <- mvtnorm::pmvnorm(
    lower = c(-Inf, -Inf),
    upper = c(stats::qnorm(pxk), stats::qnorm(pxkstar)),
    sigma = rbind(c(1, corr), c(corr, 1))
  )
  return(joint_prob[[1]] / pxkstar)
}
