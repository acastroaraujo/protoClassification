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
  stopifnot(g >= 0)
  stopifnot(r == 1L | r == 2L)
  stopifnot(is.list(prototypes))
  stopifnot(all(purrr::map_dbl(prototypes, length) == length(w)))

  if (is.null(names(prototypes))) {
    names(prototypes) <- paste0("P", 1:length(w))
  }

  if (is.null(names(w))) {
    names(w) <- paste0("w", 1:length(w))
  }

  out <- purrr::map(prototypes, function(P) {
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
    nms <- switch(i,
      "distance" = paste0("dist", 1:ncol(x)),
      "similarity" = paste0("sim", 1:ncol(x)),
      "probabilities" = paste0("prob", 1:ncol(x))
    )
    names(x) <- nms
    x
  })

  with(out, cbind(probabilities, similarity, distance, data))
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
  output <- gsub("'data\\.frame':\\s*", "", output)
  cli::cli_h2("Overview")
  cli::cli_h3("Output:")
  cat(paste(output, collapse = "\n"), "\n")
  cli::cli_h3("Prototypes:")
  utils::str(attr(x, "prototypes"), no.list = TRUE)
  cli::cli_h3("Attention Weights:")
  print(round(attr(x, "w"), 3))
  cli::cli_h3("Other Parameters:")
  print(c(g = attr(x, "g"), r = attr(x, "r")))
  cli::cli_h3("Marginal Probabilities (From Data):")
  print(colMeans(x$data))
  invisible(x)
}
