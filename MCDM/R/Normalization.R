#' Normalize Data
#'
#' This function normalizes the data by dividing each element in columns 2 to the last column by the sum of that column.
#'
#' @param data A data frame to be normalized.
#'
#' @return A normalized data frame.
#' @export
#'
#' @examples
#' data <- data.frame(ID = 1:5, A = c(10, 20, 30, 40, 50), B = c(5, 10, 15, 20, 25))
#' normalized_data <- normalize_data(data)
normalize_data <- function(data) {
  num_columns <- ncol(data)
  colsum <- vector()

  for (i in 2:num_columns) {
    colsum[i - 1] <- sum(data[, i])
  }
  for (j in 2:length(data[1, ])) {
    for (i in 1:length(data[, 1])) {
      data[i, j] <- data[i, j] / colsum[j - 1]
    }
  }

  return(data)
}
