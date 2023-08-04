
#' Title
#'
#' @param df_list
#' @param rm_cols
#'
#' @return
#' @export
#'
#' @examples
numeric_vars_by_sheet <-  function(df_list=iccg_clean,rm_cols=c("submissionId","year")){
  df_list %>%
    map(\(dft){
      var_names <- dft %>%
        select(where(is.numeric)) %>%
        colnames()
      setdiff(var_names,rm_cols)
    }
    )
}


so_vars_by_sheet <- function(df_list,rm_cols=c("submissionId","year"),keep_2022_only= T){
  df_list %>%
    map(\(dft){
      so_long <- dft %>%
        select(where(is.character)) %>%
        mutate(
          across(
            .cols=everything(),\(coltemp) str_count(coltemp,' ')+1
          )
        ) %>%
        summarise(
          across(.cols= everything(),\(coltemp) max(coltemp,na.rm=T))
        ) %>%
        pivot_longer(cols=everything()) %>%
        filter(value==1)
      if(keep_2022_only){
        so_long <- so_long %>%
          filter(!str_detect(name,"_2020$|2021$"))
      }
        var_names <- so_long %>%
          pull(name)
      setdiff(var_names,rm_cols)
    })
}




#
#
# boom <- iccg_clean %>%
#   map(\(dft){
#     var_names <- dft %>%
#       select(where(is.character)) %>%
#       mutate(
#         across(.col=everything(), \(coltemp) str_count(coltemp,' ')+1,.names = "{.col}_words"),
#         across(.col=everything(), \(coltemp) length(unique(coltemp)),.names = "{.col}_unique")
#       ) %>%
#       summarise(
#         across(.col= ends_with("_words"),\(coltemp) max(coltemp,na.rm=T),.names="{.col}_max"),
#         across(.col= ends_with("_words"),\(coltemp) median(coltemp,na.rm=T),.names="{.col}_med"),
#         across(.col= ends_with("_unique"),\(coltemp) unique(coltemp),.names="{.col}_unique")
#       ) %>%
#       pivot_longer(cols=everything()) %>%
#       mutate(
#         stat= str_extract(name,"max|med|unique"),
#         name= str_remove (name, "_words_max|_words_med|_unique")
#       )
#   }
#   )
# boom %>%
#   bind_rows() %>%
#   filter(stat=="unique") %>%
#   filter(value>1) %>%
#   ggplot(aes(x=value))+
#     geom_histogram()

