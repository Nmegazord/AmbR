#' Export a list of dataframes to an Excel file
#'
#' @description This function takes a list of dataframes and writes each dataframe to a separate worksheet in an Excel file
#' @param df A list of dataframes to be written to the Excel file
#' @param file_name The name of the Excel file to be written
#' @return No return value, but an Excel file is written as a side effect
#' @examples
#' write_to_excel(list(df1, df2), "my_excel_file.xlsx")
#'
write_to_excel <- function(df, file_name) {

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Loop over the list of data frames and write each to a separate worksheet
  for(name in names(df)) {
    clean_name <- sanitize_sheet_name(name)

    # Print the cleaned name
    print(clean_name)

    openxlsx::addWorksheet(wb, clean_name)
    openxlsx::writeData(wb, clean_name, df[[name]])
  }

  # Save the workbook to disk
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}
