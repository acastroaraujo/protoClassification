#' Compositional Effects
#'
#' @param reference a simulation, as provided by the `compute()` function
#' @param comparison another simulation, as provided by the `compute()` function
#'
#' @returns a list of data frames showing difference in probabilities and relative risk ratios
#' @export
#'
compositionalEffect <- function(reference, comparison) {
  stopifnot(inherits(reference, "prototypeComputation"))
  stopifnot(inherits(comparison, "prototypeComputation"))

  reference_probs <- summary(reference, s = 1e3)
  comparison_probs <- summary(comparison, s = 1e3)

  diff <- comparison_probs$conditional$features -
    reference_probs$conditional$features
  rr <- comparison_probs$conditional$features /
    reference_probs$conditional$features

  list(diff = diff, rr = rr)
}
