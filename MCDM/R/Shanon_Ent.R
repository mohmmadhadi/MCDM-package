#' Shannon Entropy Weights
#'
#' This function calculates Shannon entropy weights for the columns of a data frame.
#'
#' @param data A data frame for which weights are to be calculated.
#'
#' @return A vector of Shannon entropy weights.
#' @export
#'
#' @examples
#' data <- data.frame(ID = 1:5, A = c(0.2, 0.3, 0.1, 0.4, 0.0), B = c(0.5, 0.1, 0.2, 0.1, 0.1))
#' entropy_weights <- shannon_entropy_weights(data)
shannon_entropy_weights <- function(data) {
  k <- 1/log(length(data[, 1]))
  Ej <- vector()
  dj <- vector()
  weights <- vector()

  for (i in 2:length(data[1, ])) {
    Ej[i - 1] <- -k * sum(data[, i] * log(data[, i]))
  }

  for (i in 1:length(Ej)) {
    dj[i] <- 1 - Ej[i]
  }

  for (i in 1:length(dj)) {
    weights[i] <- dj[i] / sum(dj)
  }

  return(weights)
}
