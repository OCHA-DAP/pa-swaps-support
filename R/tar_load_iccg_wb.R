load_swaps <- function(fp = iccg_fp) {
  # ICCG tab has different format headers - so read in w/ colnames and rename
  df_list <- read_all_tabs(
    fp = fp,
    skip = 0,
    col_names = T,
    clean_names = F,
    .name_repair = "minimal"
  )
  df_list %>%
    map(\(dft){
      # ck if year contains all 4 digit numbers
      first_row <- dft[1, ] %>% unlist() # unlist so we don't have issues with mixed data types.
      first_row_na_rm <- first_row[!is.na(first_row)]
      first_row_is_year <- all(str_detect(first_row_na_rm, "\\d{4}"))

      if (first_row_is_year) {
        new_name <- tibble(
          a = colnames(dft),
          b = first_row
        ) %>%
          unite(a:b, col = "new_name", na.rm = T) %>%
          pull(new_name)

        colnames(dft) <- new_name
        dft <- dft[2:nrow(dft), ]
      }
      dft %>%
        clean_names()
    })
}
