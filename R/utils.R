#' read_all_tabs from excel file
#'
#' @param fp \code{character} file path
#' @param skip \code{numeric} Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given.
#'
#' @return list of data.frames
#'

read_all_tabs <- function(fp, clean_names = T, skip = 1, col_names = T, sheet_names = NULL, .name_repair = "unique") {
  if (is.null(sheet_names)) {
    sheet_names <- readxl::excel_sheets(fp)
  }
  sheet_names %>%
    purrr::map(
      \(x){
        ret <- readxl::read_xlsx(path = fp, sheet = x, skip = skip, col_names = col_names, .name_repair = .name_repair)
        if (clean_names) {
          ret <- ret %>% janitor::clean_names()
        }
        return(ret)
      }
    ) %>%
    set_names(sheet_names)
}
