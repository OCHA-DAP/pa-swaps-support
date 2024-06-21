source(
  file.path(
    "clean_data",
    "utils.R"
  )
)

library(httr)

#########################
#### LOAD INPUT JSON ####
#########################

df_iccg <- GET(
  url = "https://api.hpc.tools/v2/reportingwindows/assignments/export?type=operation",
  authenticate(
    "hid",
    password = Sys.getenv("HPC_TOOLS_TOKEN")
  )
) |>
  content(
    as = "text"
  ) |>
  jsonlite::fromJSON() |>
  pluck(
    "data"
  ) |>
  as_tibble() |>
  mutate(
    year = 2019 + reportingWindowId,
    IN_Operation_short = !(IN_Operation %in% c("SLV", "GTM", "PAK")) # other countries on short list
  ) |>
  filter(
    year > 2021,
    !(IN_Operation %in% c("SYR-NE", "SYR-RG", "PFC", "SDN"))# removing 3 operations never included for analysis
  )

###################
#### ICCG DATA ####
###################

# ICCG data has a single response for each operation, so use that as the
# unique identifier, we deal with repeat groups separately

wb_iccg <- createWorkbook()

df_iccg_wide <- df_iccg %>%
  select(
    starts_with("IN_Operation"),
    everything(),
    -reportingWindowId,
    -where(is.list)
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

write_swaps_data(
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
    starts_with("IN_Operation"),
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
# save to a workbook

addWorksheet(
  wb_iccg,
  "HCTOrg"
)

writeData(
  wb_iccg,
  "HCTOrg",
  df_hct
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
    starts_with("IN_Operation"),
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
    starts_with("IN_Operation"),
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
# save to a workbook

addWorksheet(
  wb_iccg,
  "SUBGroups"
)

writeData(
  wb_iccg,
  "SUBGroups",
  df_subgroups
)

##################################
#### ICCG SUBGROUP: FRMRRMOrg ####
##################################

df_fmrrm <- df_iccg %>%
  select(
    starts_with("IN_Operation"),
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

# save to a workbook

addWorksheet(
  wb_iccg,
  "FMRRMOrg"
)

writeData(
  wb_iccg,
  "FMRRMOrg",
  df_fmrrm
)

#####################################
#### ICCG SUBGROUP: HCTSubnatLoc ####
#####################################

df_hct_subnat <- df_iccg %>%
  select(
    starts_with("IN_Operation"),
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

# save to a workbook

addWorksheet(
  wb_iccg,
  "HCTSubnatLoc"
)

writeData(
  wb_iccg,
  "HCTSubnatLoc",
  df_hct_subnat
)

#####################################
#### ICCG SUBGROUP: ICCSubnatLoc ####
#####################################

df_icc_subnat <- df_iccg %>%
  select(
    starts_with("IN_Operation"),
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

# save to a workbook

addWorksheet(
  wb_iccg,
  "ICCSubnatLoc"
)

writeData(
  wb_iccg,
  "ICCSubnatLoc",
  df_icc_subnat
)

################################
#### SAVING ENTIRE WORKBOOK ####
################################

saveWorkbook(
  wb = wb_iccg,
  file = file.path(output_dir, "swaps_iccg_data.xlsx"),
  overwrite = TRUE
)
