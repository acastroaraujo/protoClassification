#' Calculate distances and similarity from features to prototype
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
calculateDistSim <- function(P, w, data, g, r = 1) {
  stopifnot(g >= 0)
  stopifnot(length(w) == length(P))
  stopifnot(r == 1L | r == 2L)
  distance <- apply(data, 1, \(x) sum(w * abs(x - P)^r))
  similarity <- exp(-g * distance)
  data.frame(distance, similarity)
}

#' Calculate distance, similarity, and membership probabilities
#'
#' @param data a data frame, as returned by the `make_data()` function
#' @param prototypes a list of prototypes, i.e., K-sized vectors of binary
#'    features.
#' @param w a K-sized vector of attention weights
#' @param g a sensitivity parameter, a number larger than zero
#' @param r the type of distance, 1 for Manhattan, 2 for Euclidean
#'  (irrelevant when working with binary data).
#'
#' @returns A `prototype` object. A list of probabilities, similarities,
#'  distances, and the data used to calculate them.
#' @export
#'
compute <- function(data, prototypes, w, g, r = 1L) {
  stopifnot(r == 1L | r == 2L)
  stopifnot(all(g >= 0))
  stopifnot(is.list(prototypes))
  stopifnot(length(prototypes) >= 2)
  stopifnot(length(prototypes) == length(g))
  stopifnot(all(purrr::map_dbl(prototypes, length) == length(w)))

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

#' Consolidate computation into a single data frame
#'
#' @param x a `prototype` object created by the `compute()` function.
#'
#' @returns a data frame
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


#' Get Posterior Draws of Conditional Probabilities
#'
#' @param x a `prototype` object created by the `compute()` function.
#' @param type whether "features" `Pr(X|C)` or "categories" `Pr(C|X)`
#' @param s number of draws to sample from the `.$probabilities` object created
#'  by the `compute()` function
#'
#' @returns a list of posterior draws
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


#' Get Conditional Probabilities
#'
#' @param x a `prototype` object created by the `compute()` function.
#' @param type whether "features" `Pr(X|C)` or "categories" `Pr(C|X)`
#' @param s number of draws to sample from the `.$probabilities` object created
#'  by the `compute()` function
#'
#' @returns a list of conditional probabilities
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
    output <- t(as.data.frame(output))
  }
  return(output)
}

#' @export
#'
summary.prototypeComputation <- function(object, s = 500, ...) {
  categories <- conditionalProbs(object, "categories", s)
  cat_marginals <- colMeans(object$probabilities)
  features <- conditionalProbs(object, "features", s)
  feature_marginals <- colMeans(object$data)

  output <- list(
    marginal = list(categories = cat_marginals, features = feature_marginals),
    conditional = list(categories = categories, features = features)
  )

  structure(output, class = c("summary.prototypeComputation", class(output)))
}

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

}
