#' Create a grid plot for Ambr data
#'
#' @description This function takes a dataframe with Ambr data and returns a ggplot object with a grid plot of the data
#' @param data The dataframe with Ambr data
#' @return A ggplot object with the grid plot
#' @examples
#' \dontrun{
#' data <- data.frame(time_from_inoculation = runif(100),
#'                    value = runif(100), vessel_id = sample(letters, 100, replace = TRUE))
#' ambr_grid_plot(data)
#' }
#' @importFrom rlang .data
ambr_grid_plot <- function(data){
  x <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$time_from_inoculation, y = .data$value, col = .data$vessel_id)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~.data$vessel_id) +
    ggplot2::theme_minimal()
  return(x)
}
