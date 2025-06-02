#' Calculate distances and similarity from features to prototype
#'
#' @param X a matrix, as returned by the `make_data()` function
#' @param w a K-sized vector of attention weights
#' @param P a K-sized vector of binary features (i.e., the prototype)
#' @param g a sensitivity parameter, a number larger than zero
#' @param r the type of distance, 1 for Manhattan, 2 for Euclidean
#'
#' @returns a data frame with distance and similarity
#' @export
#'
calculateDS <- function(X, w, P, g, r = 1) {
  stopifnot(g >= 0)
  stopifnot(length(w) == length(P))
  stopifnot(r == 1L | r == 2L)
  distance <- apply(X, 1, \(x) sum(w * abs(x - P)^r))
  similarity <- exp(-g * distance)
  data.frame(distance, similarity)
}
