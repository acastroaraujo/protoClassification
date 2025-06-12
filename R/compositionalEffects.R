#' Compare Compositional Effects by Modifying Model Parameters
#'
#' Computes the difference in conditional probabilities and relative risk ratios
#' between a baseline prototype computation and a modified version where specific
#' parameters are changed. This function provides a streamlined way to analyze
#' how changes in attention weights, sensitivity parameters, or prototype definitions
#' affect the resulting probability distributions and category assignments.
#'
#' @param baseline A \code{prototypeComputation} object, as created by
#'   \code{\link{compute}}. This serves as the baseline condition from which
#'   specific parameters will be modified for comparison.
#' @param ... Named arguments specifying the parameters to modify. Must be one
#'   or more of:
#'   \describe{
#'     \item{\code{w}}{A K-sized numeric vector of attention weights. Must sum
#'       to 1 and all values must be non-negative.}
#'     \item{\code{g}}{A numeric vector of sensitivity parameters, one per
#'       prototype. All values must be non-negative (>= 0).}
#'     \item{\code{prototypes}}{A list of prototype vectors. Each prototype
#'       must be a binary vector of the same length as the number of features
#'       in the baseline data.}
#'   }
#'   All other parameters (data, distance type) are inherited from the baseline.
#' @param s Integer. Number of draws to sample for probability estimation.
#'   Default is 1000. Higher values provide more precision but increase
#'   computation time.
#'
#' @return A \code{compositionalEffect} object (inherits from \code{list})
#'   containing two data frames:
#'   \describe{
#'     \item{\code{diff}}{Data frame of probability differences
#'       (comparison - baseline). Positive values indicate higher probabilities
#'       in the modified condition, negative values indicate lower probabilities.}
#'     \item{\code{rr}}{Data frame of relative risk ratios
#'       (comparison / baseline). Values > 1 indicate higher relative probability
#'       in the modified condition, values < 1 indicate lower relative probability,
#'       and values = 1 indicate no change.}
#'   }
#'   Both data frames have rows representing categories and columns representing
#'   features.
#'
#' @details
#' The function creates a modified prototype computation by:
#' \enumerate{
#'   \item Taking the baseline computation's data and parameters
#'   \item Replacing specified parameters with the new values provided in \code{...}
#'   \item Computing a new prototype analysis with \code{\link{compute}}
#'   \item Comparing conditional feature probabilities P(X|C) between baseline and modified versions
#' }
#'
#' The comparison metrics computed are:
#' \itemize{
#'   \item \strong{Difference}: modified_prob - baseline_prob
#'   \item \strong{Relative Risk}: modified_prob / baseline_prob
#' }
#'
#' This approach allows for systematic sensitivity analysis by modifying one
#' parameter at a time while holding others constant.
#'
#' @seealso
#' \code{\link{compute}} for creating prototypeComputation objects,
#' \code{\link{summary.prototypeComputation}} for the underlying probability calculations
#'
#' @export
#'
#'
compositionalEffect <- function(baseline, ..., s = 1e3) {
  stopifnot(inherits(baseline, "prototypeComputation"))

  params <- list(...)
  nms <- names(params)
  stopifnot(!is.null(nms))
  stopifnot(nms %in% c("w", "g", "prototypes"))
  stopifnot(!any(duplicated(nms)))

  baseline_param_nms <- setdiff(c("w", "g", "prototypes"), nms)
  baseline_params <- purrr::map(baseline_param_nms, \(x) attr(baseline, x))
  names(baseline_params) <- baseline_param_nms
  baseline_params$data <- baseline$data
  new <- do.call(compute, append(params, baseline_params))

  before <- summary(baseline, s = 1e3)
  after <- summary(new, s = 1e3)

  diffparams <- purrr::map(list(baseline, new), function(x) {
    purrr::map(nms, \(nm) attr(x, nm)) |> purrr::set_names(nms)
  })
  names(diffparams) <- c("baseline", "new")

  ## needed for changing number of prototypes
  cats <- colnames(baseline$probabilities)

  diffProb <- after$conditional$features[cats, ] - before$conditional$features
  RRR <- after$conditional$features[cats, ] / before$conditional$features

  structure(
    list(diff = as.data.frame(diffProb), rr = as.data.frame(RRR)),
    class = c("compositionalEffect", "list"),
    baseline = baseline,
    new = new,
    diffparams = diffparams
  )
}


#' Print method for compositionalEffect objects
#'
#' Displays a formatted summary of the effects of parameter modifications on
#' prototype computations, showing changes in category marginal probabilities,
#' probability differences, and relative risk ratios.
#'
#' @param x A \code{compositionalEffect} object created by
#'   \code{\link{compositionalEffect}}.
#' @param ... Additional arguments passed to print methods (currently unused).
#'
#' @return Invisibly returns the input object \code{x}
#'
#' @details
#' The print method displays:
#' \describe{
#'   \item{Category Marginal Probabilities}{Shows the marginal category probabilities
#'     before and after the parameter modification, computed as column means of the
#'     probability matrices}
#'   \item{Δ Probs}{The probability differences (modified - baseline) for each
#'     feature within each category. Positive values indicate higher probabilities
#'     in the modified condition}
#'   \item{Risk Ratio}{The relative risk ratios (modified / baseline) for each
#'     feature within each category. Values > 1 indicate higher relative probability
#'     in the modified condition, values < 1 indicate lower relative probability}
#' }
#'
#' All numeric values are rounded to 3 decimal places for readability.
#'
#' @method print compositionalEffect
#' @export
#'
#' @seealso \code{\link{compositionalEffect}} for creating compositionalEffect objects
#'
print.compositionalEffect <- function(x, ...) {

  original <- attr(x, "baseline")
  new <- attr(x, "new")
  diffparams <- attr(x, "diffparams")

  nms <- names(purrr::list_transpose(diffparams))

  if ("prototypes" %in% nms) {
    i <- which(nms == "prototypes")
    diffparams$baseline <- c(diffparams$baseline[[i]], diffparams$baseline[-i])
    diffparams$new <- c(diffparams$new[[i]], diffparams$new[-i])
  }

  cli::cli_h2("Changed Parameters")
  cli::cli_h2("Baseline:")
  print(purrr::map(diffparams$baseline, round, 3))
  cli::cli_h2("New:")
  print(purrr::map(diffparams$new, round, 3))
  cli::cli_h2("Category Marginal Probabilities")
  cli::cli_h3("Baseline:")
  print(round(colMeans(original$probabilities), 3))
  cli::cli_h3("New:")
  print(round(colMeans(new$probabilities), 3))
  cli::cli_h2("\u0394 Probs") # Unicode escape for Delta (Δ)
  print(round(x$diff, 3))
  cli::cli_text("")
  cli::cli_h2("Risk Ratio")
  print(round(x$rr, 3))
  invisible(x)
}
