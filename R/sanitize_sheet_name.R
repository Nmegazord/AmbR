#' Sanitize sheet names for Excel
#'
#' @description This function cleans up sheet names for Excel by replacing special characters and spaces with underscores,
#' and removing leading or trailing underscores and square brackets
#' @param name The original sheet name
#' @return The sanitized sheet name
#' @examples
#' sanitize_sheet_name("Sheet [1]")
#'
sanitize_sheet_name <- function(name) {

  # Replace special characters with underscore
  clean_name <- gsub("[^[:alnum:] ]", "_", name)

  # Replace spaces with underscore
  clean_name <- gsub(" ", "_", clean_name)

  # Remove leading or trailing underscores
  clean_name <- gsub("^_|_$", "", clean_name)

  # Remove square brackets
  clean_name <- gsub("\\[|\\]", "", clean_name)

  return(clean_name)
}
