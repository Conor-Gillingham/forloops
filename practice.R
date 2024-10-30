

#' Column Means Calculator
#'
#' Calculates the mean of each numeric column in a data frame without using the colMeans() function.
#'
#' @param df A data frame containing numeric and/or non-numeric columns.
#' @return A named vector with the mean of each numeric column. Non-numeric columns will have NA.
#' @examples
#' df <- data.frame(a = c(1, 2, 3), b = c(4, NA, 6), c = c("x", "y", "z"))
#' @export
col_means <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  means <- numeric(ncol(df))
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      means[i] <- mean(df[[i]], na.rm = TRUE)
    } else {
      means[i] <- NA
    }
  }
  names(means) <- names(df)
  return(means)
}
#' NA Counter
#'
#' Counts the number of NA values in a vector.
#'
#' @param vec A vector.
#' @return An integer representing the count of NA values in the vector.
#' @examples
#' # Example usage
#' vec <- c(1, NA, 3, NA, 5, NA)
#' @export
count_na <- function(vec) {
  if (!is.vector(vec)) {
    stop("Input must be a vector")
  }
  na_count <- 0
  for (i in vec) {
    if (is.na(i)) {
      na_count <- na_count + 1
    }
  }
  return(na_count)
}
?col_means
