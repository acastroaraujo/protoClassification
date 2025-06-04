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
#' @param prototypes a list of prototypes, i.e., K-sized vectors of binary
#'    features.
#' @param w a K-sized vector of attention weights
#' @param data a data frame, as returned by the `make_data()` function
#' @param g a sensitivity parameter, a number larger than zero
#' @param r the type of distance, 1 for Manhattan, 2 for Euclidean
#'
#' @returns A `prototype` object. A list of probabilities, similarities,
#'  distances, and the data used to calculate them.
#' @export
#'
compute <- function(prototypes, w, data, g, r = 1L) {
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
  out$data <- data
  structure(
    out,
    class = c("prototype", class(out)),
    prototypes = prototypes,
    w = w,
    g = g,
    r = r
  )
}


#' @export
#'
print.prototype <- function(x, ...) {
  output <- utils::capture.output(utils::str(
    x,
    give.attr = FALSE,
    max.level = 1,
    no.list = TRUE
  ))

  r <- attr(x, "r")
  dist <- c("Manhattan", "Euclidean")

  output <- gsub("'data\\.frame':\\s*", "", output)
  cli::cli_h2("Overview")
  cli::cli_h3("Output:")
  cli::cat_line(paste(output, collapse = "\n"))
  cli::cli_h3("Prototypes:")
  utils::str(attr(x, "prototypes"), no.list = TRUE)
  cli::cli_h3("Distance:")
  cli::cli_text("{dist[[r]]} (r = {r})")
  cli::cli_h3("Sensitivity:")
  print(attr(x, "g"))
  cli::cli_h3("Attention Weights:")
  print(round(attr(x, "w"), 3))
  cli::cli_h3("Marginal Probabilities, or {.code colMeans(.$data)}")
  print(colMeans(x$data))
  cli::cli_h3("Category Prevalence, or {.code colMeans(.$probabilities)}")
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
  stopifnot(inherits(x, "prototype"))
  out <- purrr::imap(x, function(x, i) {
    if (i == "data") return(x)
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


#' Get Conditional Probabilities for K Features
#'
#' @param x a `prototype` object created by the `compute()` function.
#'
#' @returns a list of conditional probabilities; e.g., `Pr(K = 1 | C = 1)`
#' @export
#'
conditionalProbsWhichMax <- function(x) {
  stopifnot(inherits(x, "prototype"))
  category <- apply(x$probabilities, 1, which.max)
  category <- factor(category, levels = 1:ncol(x$probabilities))
  purrr::map(split(x$data, category), colMeans)
}


#' Get Conditional Probabilities for K Features
#'
#' @param x a `prototype` object created by the `compute()` function.
#' @param s number of draws to sample from the `.$probabilities` object created
#'  by the `compute()` function
#'
#' @returns a list of conditional probabilities; e.g., `Pr(K = 1 | C = 1)`
#' @export
#'
conditionalProbsSample <- function(x, s = 300) {
  stopifnot(inherits(x, "prototype"))
  stopifnot(s > 1)

  categories <- apply(x$probabilities, 1, function(x) {
    sample(seq_along(x), size = s, replace = TRUE, prob = x)
  })

  output <- apply(categories, 1, function(i) {
    i <- factor(i, levels = 1:ncol(x$probabilities))
    purrr::map(split(x$data, i), colMeans)
  })

  output <- purrr::list_transpose(output)
  purrr::map(output, \(x) do.call(rbind, x))
}
