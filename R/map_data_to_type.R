

#' map_data_to_type
#' @description
#' takes workbook list and tool and maps each column to a question type ("sm","so","num").
#' It splits each workbook into a data.frame for each question type.
#'
#'
#' @param df_list workbook converted to list of data.frames
#' @param tool kobo tool read in as list of data.frames named "survey" and "choices"
#' @param keep_cols name of columns to keep in final data.frames regardless of qtype
#'
#' @return
#' nested list:
#'   level 1: tab name
#'   level 2: data.frame containing columns by question type ("num","sm","so)
#'
#'
#' @examples \dontrun{
#' library(tidverse)
#' library(targets)
#' tar_load(iccg_clean)
#' tar_load(tool_iccg)
#' map_data_to_type(df_list = iccg_clean,tool = tool_iccg)
#' }

map_data_to_type <- function(df_list,
                             tool,
                             keep_cols=c("IN_Operation","submissionId","year")
                             ){
  rgx_by_type<- get_q_rgx(df_list = df_list,tool = tool)
  df_list %>%
    map(
      \(dft){
        rgx_by_type %>%
          map(\(rgx)
              dft %>%
                select(any_of(keep_cols),matches(rgx))
          )
      }
    )
}

#' get_subgroup_name_rgx
#' @description
#' Column names of sub-groups data were adjusted so that they no longer match the kobo tool. The sub-group was removed
#' from the original column name. However, the name of tab in the workbook contains the name of sub-grou, but with the
#' underscore removed (i.e  HCT_Org-> HCTOrg).
#' This function just takes the name of the tab and inserts the underscore back to orignal and then creates regex which
#' can be used in combination with `str_remove` to remove the group name from the kobo tool `name` column.

#' @param df_list workbook converted to list of data.frames
#'
#' @return tool group/subgroup names concatenated with "^" prefix and "|" separator so that they can be targeted
#'  string operations
#' @export
#'
#' @examples
get_subgroup_name_rgx <- function(df_list){
  # gnarly rgx modified from BARD
  put_underscore_before <- str_extract(names(df_list), "\\w{1}[a-z]")

  underscore_inserted <- str_replace(string = names(df_list),
                               pattern = put_underscore_before,
                               replacement = paste0("_", put_underscore_before))
  underscore_inserted <- underscore_inserted[!is.na(underscore_inserted)]
  subgroup_rgx <- paste(paste0("^",underscore_inserted),collapse = "|")
  return(subgroup_rgx)
}


#' get_q_rgx
#' @description
#' Goes through kobo survey sheet and pulls question names by type (i.e select one, select multiple, numeric). It then
#' removes the subgroup names as they were removed in the data and concatenates them together "^" prefix and "|" separator
#' so that they can be targeted with string operations
#'
#' @param df_list workbook converted to list of data.frames
#' @param tool kobo tool read in as list of data.frames named "survey" and "choices"
#'
#' @return regex pattern that can be used select columns by type from data format we have in output folder

get_q_rgx <- function(df_list,tool){
  subgroup_rm_rgx <- get_subgroup_name_rgx(df_list)

  qtypes_extract <- list(so= "^select_one",
                         sm="^select_multiple",
                         num=c("integer|calculate"))
  qtypes_extract %>%
    map(
      \(qtype){
        q_names <- tool$survey %>%
          filter(str_detect(type,qtype)) %>%
          pull(name)
        q_name_subgroup_rm <- str_remove(string = q_names,subgroup_rm_rgx)
        q_name_rgx <- paste(paste0("*",q_name_subgroup_rm,"*"), collapse = "|")
        return(q_name_rgx)
      }
    )
}

