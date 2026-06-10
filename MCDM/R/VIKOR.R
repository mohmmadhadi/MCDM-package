# vikor_algorithm.R

#' Calculate VIKOR rankings
#'
#' This function implements the VIKOR algorithm to rank alternatives based on multiple criteria.
#'
#' @param matrix Decision matrix where rows represent alternatives and columns represent criteria.
#' @param weights Criteria weights.
#' @param q Criteria direction ("min" for minimization, "max" for maximization).
#' @param v Parameter (0 <= v <= 1).
#' @return A vector of rankings.
#'
#' @examples
#' decision_matrix <- matrix(c(3, 5, 7, 4, 6, 8, 2, 7, 9, 6, 5, 8), nrow = 4, byrow = TRUE)
#' weights <- c(0.4, 0.6)
#' direction <- c("min", "max")
#' v <- 0.5
#' vikor_ranking(decision_matrix, weights, direction, v)
#'
#' @export
vikor <- function(matrix, weights, q, v) {
  # Calculate the worst and best values for each criterion
  worst <- apply(matrix, 2, function(x) ifelse(q == "min", min(x), max(x)))
  best <- apply(matrix, 2, function(x) ifelse(q == "min", max(x), min(x)))

  # Calculate S and R values for each alternative
  S <- apply(matrix, 1, function(x) sum(weights * (x - worst) / (best - worst)))
  R <- apply(matrix, 1, function(x) max(weights * abs(x - v)))

  # Calculate the Q values
  Q <- v * S + (1 - v) * R

  # Rank alternatives based on Q values
  rank <- order(Q)

  return(data.frame(Alternatives = 1:nrow(matrix), S = S, R = R, Q = Q, Ranking = rank))

}
