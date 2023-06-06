#' Function to display scientific notation on the y axis
#'
#' @description This function converts a number into a string
#' that represents the number in scientific notation.
#' @param x The number to convert.
#' @return A string representing the number in scientific notation.
#' @examples
#' scientific_10(10000)
#'
scientific_10 <- function(x) {
  parse(text = gsub("e\\+*", "%*%10^", scales::scientific_format(digits = 1)(x)))
}
