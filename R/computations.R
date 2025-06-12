#' Calculate distances and similarity.
#'
#' Calculate distances and similarity from features to prototype.
#' The \code{\link{compute}} function does this and more when you have
#' multiple prototypes.
#'
#' @param P a K-sized vector of binary features (i.e., the prototype)
#' @param w a K-sized vector of attention weights
#' @param data a data frame, as returned by the `make_data()` function
#' @param g a sensitivity parameter, a number larger than zero
#' @param r the type of distance, 1 for Manhattan, 2 for Euclidean
#'
#' @returns a data frame with distance and similarity
#' @export
#'
calculateDistSim <- function(data, P, w, g, r = 1) {
  stopifnot(g >= 0)
  stopifnot(length(w) == length(P))
  stopifnot(r == 1L | r == 2L)
  distance <- apply(data, 1, \(x) sum(w * abs(x - P)^r))
  similarity <- exp(-g * distance)
  data.frame(distance, similarity)
}

#' Calculate Distances and Similarities to Multiple Prototypes
#'
#' Calculates distances and similarity scores from a set of observations to a
#' single prototype using weighted distance metrics. This function implements
#' the core distance and similarity calculations used in prototype-based
#' classification models.
#'
#' @param data A data frame of binary features (0s and 1s), as returned by
#'   \code{\link{make_binary_data}}. Each row represents an observation and
#'   each column represents a binary feature.
#' @param prototypes A list of prototype vectors. Each must be the same length
#'   as the number of columns in \code{data} and contain only binary values (0 or 1).
#' @param w A numeric vector of attention weights, one for each feature. Must be:
#'   \describe{
#'     \item{Length}{Equal to \code{length(P)} and \code{ncol(data)}}
#'     \item{Values}{Non-negative and sum to 1}
#'   }
#' @param g A numeric sensitivity parameter controlling the steepness of the
#'   similarity function. Must be non-negative (>= 0). Higher values make the
#'   similarity function more sensitive to distances.
#' @param r Integer specifying the distance metric type. Note that this is irrelevant when working with binary data.
#'   \describe{
#'     \item{1}{Manhattan distance (L1 norm)}
#'     \item{2}{Euclidean distance (L2 norm)}
#'   }
#'
#' @return A data frame with \code{nrow(data)} rows and two columns:
#'   \describe{
#'     \item{\code{distance}}{Numeric vector of weighted distances from each
#'       observation to the prototype}
#'     \item{\code{similarity}}{Numeric vector of similarity scores, computed as
#'       \code{exp(-g * distance)}}
#'   }
#'
#' @return A \code{prototypeComputation} object containing:
#'   \describe{
#'     \item{\code{distance}}{Data frame of distances from each observation to
#'       each prototype}
#'     \item{\code{similarity}}{Data frame of similarity scores for each
#'       observation to each prototype}
#'     \item{\code{probabilities}}{Data frame of category membership probabilities}
#'     \item{\code{data}}{The original input data}
#'   }
#'   The object also stores the input parameters as attributes.
#'
#' @details
#' The function implements a prototype-based categorization model where:
#'
#' 1. **Distance Calculation**: For each observation \eqn{x} and prototype \eqn{P_j}:
#'    \deqn{d(x, P_j) = \sum_{k=1}^{K} w_k |x_k - P_{j,k}|^r}
#'
#' 2. **Similarity Calculation**:
#'    \deqn{s(x, P_j) = \exp(-g_j \cdot d(x, P_j))}
#'
#' 3. **Probability Calculation**:
#'    \deqn{P(C_j|x) = \frac{s(x, P_j)}{\sum_{i=1}^{n} s(x, P_i)}}
#'
#' @export
#'
compute <- function(data, prototypes, w, g, r = 1L) {
  stopifnot(r == 1L | r == 2L)
  stopifnot(all(g >= 0))
  stopifnot(is.list(prototypes))
  stopifnot(length(prototypes) >= 2)
  stopifnot(length(prototypes) == length(g))
  stopifnot(all(purrr::map_dbl(prototypes, length) == length(w)))
  stopifnot((sum(w) - 1) < 1e-10) # `sum(w) == 1` for floating point math
  stopifnot(all(unlist(prototypes) %in% 0:1)) # temporary restriction to binary case

  if (is.null(names(prototypes))) {
    names(prototypes) <- paste0("P", 1:length(prototypes))
  }

  if (is.null(names(w))) {
    names(w) <- paste0("w", 1:length(w))
  }

  if (is.null(names(g))) {
    names(g) <- paste0("g", 1:length(prototypes))
  }

  out <- purrr::map2(prototypes, g, function(P, g) {
    distance <- apply(data, 1, \(x) sum(w * abs(x - P)^r))
    similarity <- exp(-g * distance)
    data.frame(distance, similarity)
  })

  out <- purrr::list_transpose(out)
  out <- purrr::map(out, as.data.frame)
  out$probabilities <- out$similarity / rowSums(out$similarity)
  names(out$probabilities) <- gsub("P", "C", names(out$probabilities))
  out$data <- data
  structure(
    out,
    class = c("prototypeComputation", class(out)),
    prototypes = prototypes,
    w = w,
    g = g,
    r = r
  )
}


#' Print method for prototypeComputation objects
#'
#' Displays a formatted summary of a prototypeComputation object, including
#' the data structure, prototypes, distance type, sensitivity parameters,
#' attention weights, and marginal probabilities.
#'
#' @param x A \code{prototypeComputation} object created by \code{\link{compute}}.
#' @param ... Additional arguments passed to print methods (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @method print prototypeComputation
#' @export
#'
print.prototypeComputation <- function(x, ...) {
  output <- utils::capture.output(utils::str(
    x,
    give.attr = FALSE,
    max.level = 1,
    no.list = TRUE
  ))

  output <- gsub(":.*?:", "", output)
  output <- gsub(":$", "", output)

  r <- attr(x, "r")
  dist <- c("Manhattan", "Euclidean")

  cli::cli_h2("Output")
  cat(output, sep = "\n")
  cli::cli_text("")
  cli::cli_h2("Prototypes")
  utils::str(attr(x, "prototypes"), no.list = TRUE)
  cli::cli_text("")
  cli::cli_h2("Distance")
  cli::cli_text("{dist[[r]]} (r = {r})")
  cli::cli_text("")
  cli::cli_h2("Sensitivity")
  print(attr(x, "g"))
  cli::cli_text("")
  cli::cli_h2("Attention Weights")
  print(round(attr(x, "w"), 3))
  cli::cli_text("")
  cli::cli_h2("Marginal Probabilities")
  cli::cli_h3("{.code colMeans(.$data)}")
  print(colMeans(x$data))
  cli::cli_h3("{.code colMeans(.$probabilities)}")
  print(colMeans(x$probabilities))
  invisible(x)
}

#' Consolidate Prototype Computation Results
#'
#' Combines all components of a prototype computation (probabilities, similarities,
#' distances, and original data) into a single data frame for easier analysis
#' and export.
#'
#' @param x A \code{prototypeComputation} object created by \code{\link{compute}}.
#'
#' @return A data frame containing:
#'   \describe{
#'     \item{Probability columns}{Named \code{prob1}, \code{prob2}, etc.,
#'       representing category membership probabilities}
#'     \item{Similarity columns}{Named \code{sim1}, \code{sim2}, etc.,
#'       representing similarity scores to each prototype}
#'     \item{Distance columns}{Named \code{dist1}, \code{dist2}, etc.,
#'       representing distances to each prototype}
#'     \item{Feature columns}{Original binary feature data}
#'   }
#'   The number of probability, similarity, and distance columns equals the
#'   number of prototypes.
#'
#' @seealso \code{\link{compute}} for creating \code{prototypeComputation} object.
#'
#' @export
#'
consolidate <- function(x) {
  stopifnot(inherits(x, "prototypeComputation"))
  out <- purrr::imap(x, function(x, i) {
    if (i == "data") {
      return(x)
    }
    nms <- switch(
      i,
      "distance" = paste0("dist", 1:ncol(x)),
      "similarity" = paste0("sim", 1:ncol(x)),
      "probabilities" = paste0("prob", 1:ncol(x))
    )
    names(x) <- nms
    x
  })

  with(out, cbind(probabilities, similarity, distance, data))
}


#' Sample Conditional Probabilities from Prototype Model
#'
#' Generates posterior draws of conditional probabilities from a prototype
#' computation, either P(X|C) (feature probabilities given categories) or
#' P(C|X) (category probabilities given feature values). This function is
#' primarily used internally by other functions but can be useful for
#' uncertainty quantification.
#'
#' @param x A \code{prototypeComputation} object created by \code{\link{compute}}.
#' @param type Character string specifying the type of conditional probabilities:
#'   \describe{
#'     \item{\code{"features"}}{Returns P(X|C) - probability of features given categories}
#'     \item{\code{"categories"}}{Returns P(C|X) - probability of categories given feature values}
#'   }
#' @param s Integer. Number of posterior draws to sample. Default is 500.
#'   Higher values provide more stable estimates but increase computation time.
#'
#' @return The return structure depends on the \code{type} parameter:
#'
#'   For \code{type = "features"}:
#'   A list with one element per category, where each element is an \code{s × K}
#'   matrix of feature probabilities (K = number of features).
#'
#'   For \code{type = "categories"}:
#'   A list with one element per feature, where each element is an
#'   \code{s × n_categories × 2} array showing category probabilities for
#'   feature values 0 and 1.
#'
#' @details
#' The function implements a sampling-based approach to estimate conditional
#' probabilities:
#'
#' 1. For each observation, sample category assignments based on the computed
#'    membership probabilities
#' 2. Repeat this process \code{s} times to create multiple datasets
#' 3. Calculate conditional probabilities from each sampled dataset
#'
#' This approach captures the uncertainty in category assignments and provides
#' a distribution of conditional probability estimates rather than point estimates.
#'
#' @seealso
#' \code{\link{conditionalProbs}} for point estimates of conditional probabilities,
#' \code{\link{summary.prototypeComputation}} for summary statistics,
#' \code{\link{compute}} for creating \code{prototypeComputation} objects
#'
#' @export
#'
conditionalProbsSample <- function(
  x,
  type = c("features", "categories"),
  s = 500
) {
  stopifnot(inherits(x, "prototypeComputation"))
  stopifnot(s >= 1)
  type <- match.arg(type)

  categories <- apply(x$probabilities, 1, function(p) {
    sample(seq_along(p), size = s, replace = TRUE, prob = p)
  })

  if (s == 1) {
    dim(categories) <- c(s, nrow(x$probabilities))
  }

  if (type == "features") {
    output <- apply(categories, 1, function(i) {
      i <- factor(i, levels = 1:ncol(x$probabilities))
      purrr::map(split(x$data, i), colMeans)
    })
    output <- purrr::list_transpose(output)
    output <- purrr::map(output, \(x) do.call(rbind, x))
    names(output) <- paste0("C", seq_along(output))
  }

  if (type == "categories") {
    output <- purrr::imap(x$data, function(x_vals, feature_name) {
      n_cat <- ncol(x$probabilities)
      x_nms <- paste0(feature_name, "=", 0:1)

      index_0 <- which(x_vals == 0)
      index_1 <- which(x_vals == 1)

      result <- array(
        data = 0L,
        dim = c(s, n_cat, 2),
        dimnames = list(1:s, paste0("C", 1:n_cat), x_nms)
      )

      out0 <- purrr::map(1:n_cat, \(cat) rowMeans(categories[, index_0] == cat))
      out1 <- purrr::map(1:n_cat, \(cat) rowMeans(categories[, index_1] == cat))

      result[,, x_nms[1]] <- do.call(cbind, out0)
      result[,, x_nms[2]] <- do.call(cbind, out1)
      result
    })
  }

  return(output)
}

#' Calculate Conditional Probabilities from Prototype Model
#'
#' Computes point estimates of conditional probabilities from a prototype
#' computation, either P(X|C) (feature probabilities given categories) or
#' P(C|X) (category probabilities given feature values). This function
#' provides the expected values of the conditional probability distributions.
#'
#' @param x A \code{prototypeComputation} object created by \code{\link{compute}}.
#' @param type Character string specifying the type of conditional probabilities:
#'   \describe{
#'     \item{\code{"features"}}{Returns P(X|C) - probability of features given categories}
#'     \item{\code{"categories"}}{Returns P(C|X) - probability of categories given feature values}
#'   }
#' @param s Integer. Number of posterior draws used for sampling-based estimation.
#'   Default is 500. Results are averaged across all draws to provide point estimates.
#'
#' @return The return structure depends on the \code{type} parameter:
#'
#'   For \code{type = "features"}:
#'   A data frame with categories as rows and features as columns, where each cell
#'   contains P(X_k = 1 | C_j). To get P(X_k = 0 | C_j) you can calculate 1 - "data frame".
#'
#'   For \code{type = "categories"}:
#'   A list with two elements:
#'   \describe{
#'     \item{\code{Xk=0}}{Matrix of P(C_j | X_k = 0) for each feature k and category j}
#'     \item{\code{Xk=1}}{Matrix of P(C_j | X_k = 1) for each feature k and category j}
#'   }
#'
#' @details
#' This function provides point estimates by taking the mean of the sampling
#' distributions generated by \code{\link{conditionalProbsSample}}. The conditional
#' probabilities represent:
#'
#' \itemize{
#'   \item \strong{P(X|C)}: Given that an observation belongs to category C,
#'     what is the probability that feature X has value 1?
#'   \item \strong{P(C|X)}: Given that feature X has a specific value (0 or 1),
#'     what is the probability that the observation belongs to category C?
#' }
#'
#' @seealso
#' \code{\link{conditionalProbsSample}} for full sampling distributions,
#' \code{\link{summary.prototypeComputation}} for comprehensive summaries,
#' \code{\link{compute}} for creating \code{prototypeComputation} objects
#'
#' @export
#'
conditionalProbs <- function(x, type = c("features", "categories"), s = 500) {
  type <- match.arg(type)
  output <- conditionalProbsSample(x, type, s)

  if (type == "categories") {
    out1 <- purrr::map(output, function(m) {
      data.frame(t(colMeans(m[,, 1])))
    })

    out2 <- purrr::map(output, function(x) {
      data.frame(t(colMeans(x[,, 2])))
    })

    output <- list("Xk=0" = do.call(rbind, out1), "Xk=1" = do.call(rbind, out2))
  }
  if (type == "features") {
    output <- purrr::map(output, colMeans)
    output <- as.data.frame(t(as.data.frame(output)))
  }
  return(output)
}

#' Summary Method for Prototype Computation Objects
#'
#' Computes and returns comprehensive summary statistics for a
#' \code{prototypeComputation} object, including marginal and conditional
#' probabilities for both categories and features. This provides a complete
#' statistical overview of the prototype model results.
#'
#' @param object A \code{prototypeComputation} object created by \code{\link{compute}}.
#' @param s Integer. Number of draws to sample from the probabilities for
#'   computing conditional probabilities. Default is 500. Higher values provide
#'   more stable estimates.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{summary.prototypeComputation} object containing:
#'   \describe{
#'     \item{\code{marginal}}{List with marginal probabilities:
#'       \itemize{
#'         \item \code{categories}: Vector of category marginal probabilities
#'         \item \code{features}: Vector of feature marginal probabilities
#'       }
#'     }
#'     \item{\code{conditional}}{List with conditional probabilities:
#'       \itemize{
#'         \item \code{categories}: P(C|X) for feature values 0 and 1
#'         \item \code{features}: P(X|C) matrix
#'       }
#'     }
#'   }
#'
#' @details
#' The summary provides four key probability distributions:
#'
#' \itemize{
#'   \item \strong{Category Marginals}: Overall probability of each category
#'     across all observations
#'   \item \strong{Feature Marginals}: Overall probability of each feature
#'     being 1 across all observations
#'   \item \strong{Conditional Features}: P(X_k = 1 | C_j) for each feature k
#'     and category j
#'   \item \strong{Conditional Categories}: P(C_j | X_k = 0) and P(C_j | X_k = 1)
#'     for each category j and feature k
#' }
#'
#' @method summary prototypeComputation
#' @export
#'
#' @seealso \code{\link{conditionalProbs}}, \code{\link{compute}}
#'
summary.prototypeComputation <- function(object, s = 500, ...) {

  categories <- conditionalProbs(object, "categories", s)
  cat_marginals <- colMeans(object$probabilities)
  features <- conditionalProbs(object, "features", s) ## !!!!! change this to data.frame()
  feature_marginals <- colMeans(object$data)

  output <- list(
    marginal = list(categories = cat_marginals, features = feature_marginals),
    conditional = list(categories = categories, features = features)
  )

  structure(output, class = c("summary.prototypeComputation", class(output)))
}

#' Print Summary Statistics for Prototype Computation Objects
#'
#' Displays a formatted summary of prototype-based classification results,
#' showing both marginal and conditional probability distributions for
#' categories and features. This method provides a comprehensive overview
#' of how the prototype model distributes probability mass across categories
#' and features.
#'
#' @param x A \code{summary.prototypeComputation} object created by
#'   \code{\link{summary.prototypeComputation}}.
#' @param ... Additional arguments passed to print methods (currently unused).
#'
#' @return Invisibly returns the input object \code{x} unchanged. The function
#'   is called primarily for its side effect of printing formatted output.
#'
#' @details
#' The printed output is organized into two main sections:
#'
#' \strong{Categories Section:}
#' \itemize{
#'   \item \emph{Marginals}: Overall probability of assignment to each category
#'     across all observations, computed as \code{colMeans(object$probabilities)}
#'   \item \emph{Conditionals}: Two sub-lists showing P(C|X) - the probability
#'     of each category given feature values:
#'     \itemize{
#'       \item \code{Xk=0}: Category probabilities when each feature equals 0
#'       \item \code{Xk=1}: Category probabilities when each feature equals 1
#'     }
#' }
#'
#' \strong{Features Section:}
#' \itemize{
#'   \item \emph{Marginals}: Overall probability that each feature equals 1
#'     across all observations, computed as \code{colMeans(object$data)}
#'   \item \emph{Conditionals}: The probability that each feature equals 1
#'     given membership in each category, P(X|C).
#' }
#'
#' All probability values are rounded to 3 decimal places for readability.
#'
#' @method print summary.prototypeComputation
#' @export
#'
print.summary.prototypeComputation <- function(x, ...) {
  cli::cli_h2("Categories")
  cli::cli_h3("Marginals:")
  print(round(x$marginal$categories, 3))
  cli::cli_h3("Conditionals:")
  print(purrr::map(x$conditional$categories, round, 3))
  cli::cli_h2("Features")
  cli::cli_h3("Marginals:")
  print(round(x$marginal$features, 3))
  cli::cli_h3("Conditionals:")
  print(round(x$conditional$features, 3))
  invisible(x)
}
