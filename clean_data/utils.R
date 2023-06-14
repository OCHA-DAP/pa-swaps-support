library(tidyverse)
library(jsonlite)
library(openxlsx)

input_dir <- file.path(
  Sys.getenv("SWAPS_SUPPORT"),
  "input"
)

output_dir <- file.path(
  Sys.getenv("SWAPS_SUPPORT"),
  "output"
)

# originallly thought these helpers would work with all sheets
# but only on the main ones where formatting was
# simpler to do year to year for comparisons

# helper function to format worksheets
# use integers to specify columns
# use for single column
format_2022_col <- function(wb, sheet, rows, col) {
  # get reference for 1st previous column and starting row
  prev_col <- int2col(col - 1)
  curr_col <- int2col(col)
  start_row <- rows[1]
  rule <- paste0(
    curr_col,
    start_row,
    "!=",
    prev_col,
    start_row
  )

  conditionalFormatting(
    wb = wb,
    sheet = sheet,
    cols = col,
    rows = rows,
    rule = rule,
    type = "expression"
  )
}

# applies formatting across multiple columns
# has to be done separately
format_2022_cols <- function(wb, sheet, rows, cols) {
  walk(
    .x = cols,
    .f = \(col) {
      format_2022_col(
        wb = wb,
        sheet = sheet,
        rows = rows,
        col = col
      )
    }
  )
}

# create sheet, write data and format at the same time
# assume data has column names and first row of years
write_swaps_yearly_data <- function(wb, sheet, df) {
  # create secondary row with year to prepare for notebooking for Excel
  years <- str_extract(names(df), "([0-9]{4}$)")
  df_names <- names(df)
  names(years) <- df_names
  df_wb <- add_row(df, !!!years, .before = 1)
  names(df_wb) <- str_remove(df_names, "_[0-9]{4}$")

  # get dimensions of the data frame
  years <- unlist(df_wb[1,], use.names = FALSE)
  cols <- which(years == 2022)
  rows <- 3:(nrow(df) + 1) # need to account for adding headers as rows

  addWorksheet(wb, sheet)
  writeData(wb, sheet, df_wb)
  format_2022_cols(wb, sheet, rows, cols)
}




#' read_all_tabs from excel file
#'
#' @param fp \code{character} file path
#' @param skip \code{numeric} Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given.
#'
#' @return list of data.frames
#'

read_all_tabs <- function(fp,clean_names=T,skip=1,col_names=T, sheet_names=NULL){
  if(is.null(sheet_names)){
    sheet_names <- readxl::excel_sheets(fp)
  }
  sheet_names %>%
    purrr::map(
      \(x){
        ret <- readxl::read_xlsx(path = fp,sheet = x,skip=skip,col_names = col_names)
        if(clean_names){
          ret <- ret %>% janitor::clean_names()
        }
        return(ret)

      }
    ) %>%
    set_names(sheet_names)
}


