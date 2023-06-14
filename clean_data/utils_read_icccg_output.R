library(unheadr)
library(janitor)
library(tidyverse)

iccg_fp <- file.path(
  Sys.getenv("SWAPS_SUPPORT"),
  "output",
  "swaps_iccg_data.xlsx"
)

# ICCG tab has different format headers - so read in w/ colnames and rename
iccg_main <- readxl::read_xlsx(path = iccg_fp,
                               sheet = "ICCG",
                               col_names = F)

# create new name
new_name <- tibble(
  a = iccg_main[1,] %>% unlist(),
  b =  iccg_main[2,] %>% unlist()
)%>%
  unite(a:b, col="new_name",na.rm=T) %>%
  pull(new_name)

# remove naming rows
iccg_main_df <-  iccg_main[3:nrow(iccg_main),]

# rename
names(iccg_main_df) <- new_name
iccg_main_clean <- iccg_main_df %>%
  type_convert()


# first tab ("ICCG") is formatted differently with year in second row
# so read in others first
norm_header_sheets <- readxl::excel_sheets(iccg_fp)[readxl::excel_sheets(iccg_fp)!="ICCG"]

iccg_rep_tabs<- read_all_tabs(fp = iccg_fp,
                                 skip=0,
                                 sheet_names = norm_header_sheets,
                                 clean_names = T)

iccg_tabs <- list(iccg_main_df,
                  iccg_rep_tabs
                  ) %>%
  list_flatten() %>%
  set_names(c("main",names(iccg_rep_tabs)))





