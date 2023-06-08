source(
  file.path(
    "clean_data",
    "utils.R"
  )
)

#########################
#### LOAD INPUT JSON ####
#########################

df_iccg <- read_json(
  file.path(
    input_dir,
    "CDM Survey HCT-ICCG Data extract 230601.json"
  ),
  simplifyVector = TRUE
) %>%
  pluck(1) %>%
  as_tibble() %>%
  mutate(
    year = 2019 + reportingWindowId,
  )

###################
#### ICCG DATA ####
###################

# ICCG data has a single response for each operation, so use that as the
# unique identifier, we deal with repeat groups separately

wb_iccg <- createWorkbook()

df_iccg_wide <- df_iccg %>%
  select(
    IN_Operation,
    everything(),
    -reportingWindowId,
    -where(is.list)
  ) %>%
  pivot_wider(
    id_cols = IN_Operation,
    names_from = year,
    values_from = submissionId:HCT_CashIncreaseNo,
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    across(
      .cols = everything(),
      .fns = as.character
    )
  ) %>%
  arrange(
    IN_Operation
  )

write_swaps_yearly_data(
  wb = wb_iccg,
  sheet = "ICCG",
  df = df_iccg_wide
)

############################
#### ICCG SUBGROUP: HCT ####
############################

# ICCG data that is in repeat groups are held separately from the main data
# need to explore that separately for cleaning
# for now doing these separately to

df_hct <- df_iccg %>%
  select(
    IN_Operation,
    submissionId,
    year,
    HCTOrg
  ) %>%
  unnest(
    HCTOrg
  ) %>%
  rename_with(
    .fn = \(x) str_remove(x, "^HCT_Org"),
    .cols = everything()
  ) %>%
  select(
    -starts_with("Att") # not collected in 2022
  ) %>%
  type_convert() %>%
  mutate(
    across(
      .cols = c(TypeINGO, TypeNNGO),
      .fns = \(x) replace_na(x, 0)
    )
  )

# do a simple match of 2022 data with 2021 to highlight when something was
# present in 2022 but not in 2021 and vice versa

df_hct_2021 <- filter(df_hct, year == 2021)
df_hct_2022 <- filter(df_hct, year == 2022)

join_cols <- c("IN_Operation", "Type", "Name", "Status", "NameOther", "StatusOther", "TypeINGO", "TypeNNGO")

df_hct_wb <- left_join(
  x = df_hct_2022,
  y = df_hct_2021 %>%
    mutate(in_2021 = TRUE) %>%
    select(-year, -submissionId, -Calc),
  by = join_cols,
  relationship = "many-to-many"
) %>%
  mutate(
    in_2021 = replace_na(in_2021, FALSE)
  ) %>%
  bind_rows(
    anti_join(
      df_hct_2021,
      df_hct_2022,
      by = join_cols
    ) %>%
      mutate(
        year = 2021,
        in_2022 = FALSE
      )
  ) %>%
  group_by(
    year,
    IN_Operation,
    Type,
    Name,
    Status,
    NameOther,
    StatusOther,
    TypeINGO,
    TypeNNGO
  ) %>%
  mutate(
    in_2022 = replace_na(in_2022, TRUE),
    year_check = case_when(
      !in_2021 ~ "new addition in 2022",
      !in_2022 ~ "missing from 2022",
      TRUE ~ ""
    ),
    duplicate_check = ifelse(
      n() == 1 | year == 2021,
      "",
      "multiple rows for 2022"
    )
  ) %>%
  select(
    -starts_with("in_2")
  ) %>%
  arrange(
    IN_Operation,
    desc(year),
    Type,
    Name
  )

# save to a workbook

addWorksheet(
  wb_iccg,
  "HCTOrg"
)

writeData(
  wb_iccg,
  "HCTOrg",
  df_hct_wb
)

conditionalFormatting(
  wb = wb_iccg,
  sheet = "HCTOrg",
  rows = 1:nrow(df_hct_wb) + 1,
  cols = 12:13,
  rule = 'L2<>""'
)

##################################
#### ICCG SUBGROUP: LOCSubnat ####
##################################

# not covered because no data from 2022

##################################
#### ICCG SUBGROUP: SUBGroups ####
##################################

df_subgroups <- df_iccg %>%
  select(
    IN_Operation,
    submissionId,
    year,
    SUBGroups
  ) %>%
  unnest(
    SUBGroups
  ) %>%
  rename_with(
    .fn = \(x) str_remove(x, "^SUB_"),
    .cols = everything()
  ) %>%
  type_convert() %>%
  select(
    -contains("Chairs") # only used in 2020
  ) %>%
  pivot_longer(
    cols = matches("^Chair[0-9]{1}"),
    names_to = c("ChairNumber", "name"),
    names_pattern = "Chair([0-9]{1})(.*)"
  ) %>%
  pivot_wider(
    names_prefix = "Chair"
  ) %>%
  filter(
    year != "2020",
    !is.na(ChairType)
  ) %>%
  select(
    IN_Operation,
    year,
    submissionId,
    NumCalc,
    Theme,
    ThemeOther,
    Report,
    ReportOther,
    SecType,
    SecName,
    SecNameOther,
    ChairFullTime,
    ChairNumber,
    ChairType,
    ChairName,
    ChairNameOther
  )

df_subgroups_2021 <- filter(df_subgroups, year == 2021)
df_subgroups_2022 <- filter(df_subgroups, year == 2022)


# do some comparisons between 2022 and 2021
# looking at first at the general subgroups

df_subgroups_wb <- df_subgroups_2022 %>%
  left_join(
    df_subgroups_2021 %>%
      distinct(
        IN_Operation,
        Theme,
        Report,
        ReportOther
      ) %>% mutate(
      group_existed_2021 = TRUE
    ),
    by = c("IN_Operation", "Theme", "Report", "ReportOther"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    group_existed_2021 = replace_na(group_existed_2021, FALSE)
  ) %>%
  left_join( # find those chairs that exists in 2021
    df_subgroups_2021 %>%
      select(
        IN_Operation,
        Theme,
        Report,
        ReportOther,
        ChairType,
        ChairName,
        ChairNameOther
      ) %>%
      mutate(
        chair_existed_2021 = TRUE
      ),
    relationship = "many-to-many",
    by = c("IN_Operation", "Theme", "Report", "ReportOther", "ChairType", "ChairName", "ChairNameOther")
  ) %>%
  mutate(
    chair_existed_2021 = ifelse(
      group_existed_2021,
      replace_na(chair_existed_2021, FALSE),
      NA
    )
  ) %>%
  bind_rows( # find those groups dropped in 2022
    anti_join(
      df_subgroups_2021,
      df_subgroups_2022,
      by = c("IN_Operation", "Theme", "Report", "ReportOther")
    ) %>%
      mutate(
        group_dropped_2022 = TRUE
      )
  )  %>%
  bind_rows( # find chairs dropped in 2022
    anti_join(
      df_subgroups_2021,
      df_subgroups_2022,
      by = c("IN_Operation", "Theme", "Report", "ReportOther", "ChairType", "ChairName", "ChairNameOther")
    ) %>%
      semi_join( # only keep for groups kept in 2022
        df_subgroups_2022,
        by = c("IN_Operation", "Theme", "Report", "ReportOther")
      ) %>%
      mutate(
        chair_dropped_2022 = TRUE
      )
  ) %>%
  mutate(
    subgroup_check = case_when(
      group_dropped_2022 ~ "Subgroup was not listed in 2022",
      !group_existed_2021 ~ "Subgroup is new in 2022",
      TRUE ~ ""
    ),
    chair_check = case_when(
      group_dropped_2022 | !group_existed_2021  ~ "",
      chair_dropped_2022 ~ "Chair dropped in 2022",
      !chair_existed_2021 ~ "Chair is new in 2022",
      TRUE ~ ""
    )
  ) %>%
  select(
    -matches("[0-9]{4}$")
  ) %>%
  arrange(
    IN_Operation,
    year,
    Theme
  )

# save to a workbook

addWorksheet(
  wb_iccg,
  "SUBGroups"
)

writeData(
  wb_iccg,
  "SUBGroups",
  df_subgroups_wb
)

conditionalFormatting(
  wb = wb_iccg,
  sheet = "SUBGroups",
  rows = 1:nrow(df_subgroups_wb) + 1,
  cols = 17:18,
  rule = 'Q2<>""'
)

##################################
#### ICCG SUBGROUP: FRMRRMOrg ####
##################################

df_fmrrm <- df_iccg %>%
  select(
    IN_Operation,
    submissionId,
    year,
    FRMRRMOrg
  ) %>%
  unnest(
    FRMRRMOrg
  ) %>%
  rename_with(
    .fn = \(x) str_remove(x, "^FRM_RRMOrg"),
    .cols = everything()
  ) %>%
  type_convert() %>%
  filter(
    !is.na(Role)
  )

df_fmrrm_2021 <- filter(df_fmrrm, year == 2021)
df_fmrrm_2022 <- filter(df_fmrrm, year == 2022)

df_fmrrm_wb <- df_fmrrm_2022 %>%
  left_join(
    df_fmrrm_2021 %>%
      select(
        -year, -submissionId, -Calc
      ) %>%
      mutate(
        org_check = ""
      ),
    by = c("IN_Operation", "Role", "Type", "Name", "NameOther")
  ) %>%
  mutate(
    org_check = replace_na(org_check, "New Org in 2022")
  ) %>%
  bind_rows(
    anti_join(
      df_fmrrm_2021,
      df_fmrrm_2022,
      by = c("IN_Operation", "Role", "Type", "Name", "NameOther")
    )
  ) %>%
  mutate(
    org_check = replace_na(org_check, "Org not listed in 2022")
  ) %>%
  arrange(
    IN_Operation,
    year,
    Name
  )

# save to a workbook

addWorksheet(
  wb_iccg,
  "FMRRMOrg"
)

writeData(
  wb_iccg,
  "FMRRMOrg",
  df_fmrrm_wb
)

conditionalFormatting(
  wb = wb_iccg,
  sheet = "FMRRMOrg",
  rows = 1:nrow(df_fmrrm_wb) + 1,
  cols = 8:9,
  rule = 'H2<>""'
)

#####################################
#### ICCG SUBGROUP: HCTSubnatLoc ####
#####################################

df_hct_subnat <- df_iccg %>%
  select(
    IN_Operation,
    submissionId,
    year,
    HCTSubnatLoc
  ) %>%
  unnest(
    HCTSubnatLoc
  ) %>%
  rename_with(
    .fn = \(x) str_remove(x, "^HCT_SubnatLoc"),
    .cols = everything()
  ) %>%
  type_convert() %>%
  filter(
    !is.na(City)
  )

df_hct_subnat_2021 <- filter(df_hct_subnat, year == 2021)
df_hct_subnat_2022 <- filter(df_hct_subnat, year == 2022)

df_hct_subnat_wb <- df_hct_subnat_2022 %>%
  left_join(
    df_hct_subnat_2021 %>%
      distinct(
        IN_Operation,
        City,
        CityOther,
        Area
      ) %>%
      mutate(
        subnat_existed_2021 = TRUE
      ),
    by = c("IN_Operation", "City", "CityOther", "Area")
  ) %>%
  left_join(
    df_hct_subnat_2021 %>%
      select(
        -year, -submissionId, -Calc
      ) %>%
      mutate(
        subnat_details_matched_2021 = TRUE
      ),
    by = c("IN_Operation", "City", "CityOther", "Area", "Chair", "ChairOther", "NTA", "DNR")
  ) %>%
  mutate(
    subnat_check = case_when(
      subnat_details_matched_2021 ~ "",
      subnat_existed_2021 ~ "Subnat details differ in 2021",
      TRUE ~ "Subnat location is new in 2022"
    )
  ) %>%
  bind_rows(
    anti_join(
      df_hct_subnat_2021,
      df_hct_subnat_2022,
      by = c("IN_Operation", "City", "CityOther", "Area")
    ) %>%
      mutate(
        subnat_check = "Subnat location dropped in 2022"
      )
  ) %>%
  bind_rows(
    anti_join(
      df_hct_subnat_2021,
      df_hct_subnat_2022,
      by = c("IN_Operation", "City", "CityOther", "Area", "Chair", "ChairOther", "NTA", "DNR")
    ) %>%
      semi_join(
        df_hct_subnat_2022,
        by = c("IN_Operation", "City", "CityOther", "Area")
      ) %>%
      mutate(
        subnat_check = "Subnat details changed in 2022"
      )
  ) %>%
  select(
    -subnat_existed_2021,
    -subnat_details_matched_2021
  ) %>%
  arrange(
    IN_Operation,
    year,
    City,
    Area
  )

# save to a workbook

addWorksheet(
  wb_iccg,
  "HCTSubnatLoc"
)

writeData(
  wb_iccg,
  "HCTSubnatLoc",
  df_hct_subnat_wb
)

conditionalFormatting(
  wb = wb_iccg,
  sheet = "HCTSubnatLoc",
  rows = 1:nrow(df_hct_subnat_wb) + 1,
  cols = 12,
  rule = 'L2<>""'
)

#####################################
#### ICCG SUBGROUP: ICCSubnatLoc ####
#####################################

df_icc_subnat <- df_iccg %>%
  select(
    IN_Operation,
    submissionId,
    year,
    ICCSubnatLoc
  ) %>%
  unnest(
    ICCSubnatLoc
  ) %>%
  rename_with(
    .fn = \(x) str_remove(x, "^ICC_SubnatLoc"),
    .cols = everything()
  ) %>%
  type_convert() %>%
  filter(
    !is.na(City)
  )

df_icc_subnat_2021 <- filter(df_icc_subnat, year == 2021)
df_icc_subnat_2022 <- filter(df_icc_subnat, year == 2022)

df_icc_subnat_wb <- df_icc_subnat_2022 %>%
  left_join(
    df_icc_subnat_2021 %>%
      distinct(
        IN_Operation,
        City,
        CityOther,
        Area
      ) %>%
      mutate(
        subnat_existed_2021 = TRUE
      ),
    by = c("IN_Operation", "City", "CityOther", "Area")
  ) %>%
  left_join(
    df_icc_subnat_2021 %>%
      select(
        -year, -submissionId, -Calc
      ) %>%
      mutate(
        subnat_details_matched_2021 = TRUE
      ),
    by = c("IN_Operation", "City", "CityOther", "Area", "Chair", "ChairOther", "NTA", "DNR")
  ) %>%
  mutate(
    subnat_check = case_when(
      subnat_details_matched_2021 ~ "",
      subnat_existed_2021 ~ "Subnat details differ in 2021",
      TRUE ~ "Subnat location is new in 2022"
    )
  ) %>%
  bind_rows(
    anti_join(
      df_icc_subnat_2021,
      df_icc_subnat_2022,
      by = c("IN_Operation", "City", "CityOther", "Area")
    ) %>%
      mutate(
        subnat_check = "Subnat location dropped in 2022"
      )
  ) %>%
  bind_rows(
    anti_join(
      df_icc_subnat_2021,
      df_icc_subnat_2022,
      by = c("IN_Operation", "City", "CityOther", "Area", "Chair", "ChairOther", "NTA", "DNR")
    ) %>%
      semi_join(
        df_icc_subnat_2022,
        by = c("IN_Operation", "City", "CityOther", "Area")
      ) %>%
      mutate(
        subnat_check = "Subnat details changed in 2022"
      )
  ) %>%
  select(
    -subnat_existed_2021,
    -subnat_details_matched_2021
  ) %>%
  arrange(
    IN_Operation,
    year,
    City,
    Area
  )

# save to a workbook

addWorksheet(
  wb_iccg,
  "ICCSubnatLoc"
)

writeData(
  wb_iccg,
  "ICCSubnatLoc",
  df_icc_subnat_wb
)

conditionalFormatting(
  wb = wb_iccg,
  sheet = "ICCSubnatLoc",
  rows = 1:nrow(df_icc_subnat_wb) + 1,
  cols = 12,
  rule = 'L2<>""'
)

################################
#### SAVING ENTIRE WORKBOOK ####
################################

saveWorkbook(
  wb = wb_iccg,
  file = file.path(output_dir, "swaps_iccg_data.xlsx"),
  overwrite = TRUE
)
