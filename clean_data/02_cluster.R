source(
  file.path(
    "clean_data",
    "utils.R"
  )
)

##########################
#### LOAD INPUT JSONS ####
##########################

df_cluster <- read_json(
  file.path(
    input_dir,
    "Cluster data.json"
  ),
  simplifyVector = TRUE
) %>%
  pluck(1) %>%
  as_tibble() %>%
  mutate(
    year = 2019 + reportingWindowId,
    CL_SectorsID = ifelse(
      CL_Sectors == "FSC" & IN_Operation == "ETH" & str_detect(CL_Name, "Agriculture"),
      "FSC-AG",
      CL_Sectors
    )
  )

######################
#### CLUSTER DATA ####
######################

# Cluster data has a single response for each operation and cluster, so use that as the
# unique identifier, we deal with repeat groups separately

wb_clusters <- createWorkbook()

# we have to create a unique identifier for clusters because there is 1
# instances of clusters not uniquely identified by CL_Sectors and IN_Type
df_cluster_wide <- df_cluster %>%
  filter(
    !is.na(CL_Sectors)
  ) %>%
  select(
    IN_Operation,
    CL_SectorsID,
    CL_Sectors,
    CL_SectorsOther,
    IN_Type,
    everything(),
    -reportingWindowId,
    -where(is.list)
  ) %>%
  pivot_wider(
    id_cols = IN_Operation:IN_Type,
    names_from = year,
    values_from = -c(names(.)[1:5], "year"),
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    across(
      .cols = everything(),
      .fns = as.character
    )
  ) %>%
  arrange(
    IN_Operation,
    CL_SectorsID
  )

write_swaps_yearly_data(
  wb = wb_clusters,
  sheet = "Cluster",
  df = df_cluster_wide
)

# there are some empty responses but with submission IDs and sometimes even
# a bit more info (like respondent, but no actual info)
# so storing these separately

df_cluster_incomplete <- df_cluster %>%
  filter(
    is.na(CL_Sectors)
  ) %>%
  arrange(
    IN_Operation
  )

# save to a workbook

addWorksheet(
  wb_clusters,
  "ClusterIncomplete"
)

writeData(
  wb_clusters,
  "ClusterIncomplete",
  df_cluster_incomplete
)

#######################################
#### CLUSTER SUBGROUP: CLSub_count ####
#######################################

# unclear what this is but just using to count up total
# for each year and operation
df_clsub_count <- df_cluster %>%
  select(
    IN_Operation,
    submissionId,
    year,
    CLSub_count
  ) %>%
  unnest(
    CLSub_count
  ) %>%
  type_convert() %>%
  group_by(IN_Operation, year) %>%
  summarize(
    `0` = as.character(sum(`0`, na.rm = TRUE)),
    `1` = as.character(sum(`1`, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(`0`, `1`)
  ) %>%
  arrange(
    IN_Operation
  )

write_swaps_yearly_data(
  wb = wb_clusters,
  sheet = "CLSub_count",
  df = df_clsub_count
)

#################################
#### CLUSTER SUBGROUP: CLSub ####
#################################

# unclear what this is but just using to count up total
# for each year and operation
df_clsub <- df_cluster %>%
  transmute(
    IN_Operation,
    submissionId,
    year,
    CLSub = map(
      .x = CLSub,
      .f = \(x) {
        if (!is.null(x)) {
          mutate(x, across(.fns = as.character))
        } else {
          NULL
        }
      }
    )
  ) %>%
  unnest(
    CLSub
  ) %>%
  type_convert() %>%
  rename_with(
    .fn = \(x) str_remove(x, "^CL_Sub"),
    .cols = everything()
  ) %>%
  select(
    -starts_with("Leads"), # only used in 2020
    -LeadsNum
  ) %>%
  filter(
    year != "2020"
  ) %>%
  mutate(
    across(
      .fns = as.character
    )
  ) %>%
  pivot_longer(
    -c(IN_Operation, Calc, Loc, LocOther, Area, submissionId, year, OrgsNum, OrgsFootnote),
    names_pattern = "Org([1-3]{1})(.*)",
    names_to = c("Num", "name")
  ) %>%
  filter(
    !is.na(name),
    Num <= OrgsNum
  ) %>%
  pivot_wider() %>%
  arrange(
    IN_Operation,
    year,
    Loc,
    Area
  )

# save to a workbook

addWorksheet(
  wb_clusters,
  "CLSub"
)

writeData(
  wb_clusters,
  "CLSub",
  df_clsub
)

########################################
#### CLUSTER SUBGROUP: CLTech_count ####
########################################

# unclear what this is but just using to count up total
# for each year and operation
df_cltech_count <- df_cluster %>%
  select(
    IN_Operation,
    submissionId,
    year,
    CLTech_count
  ) %>%
  unnest(
    CLTech_count
  ) %>%
  type_convert() %>%
  group_by(IN_Operation, year) %>%
  summarize(
    `0` = as.character(sum(`0`, na.rm = TRUE)),
    `1` = as.character(sum(`1`, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(`0`, `1`)
  ) %>%
  arrange(
    IN_Operation
  )

write_swaps_yearly_data(
  wb = wb_clusters,
  sheet = "CLTech_count",
  df = df_cltech_count
)

##################################
#### CLUSTER SUBGROUP: CLTech ####
##################################

# unclear what this is but just using to count up total
# for each year and operation
df_cltech <- df_cluster %>%
  transmute(
    IN_Operation,
    submissionId,
    year,
    CLTech = map(
      .x = CLTech,
      .f = \(x) {
        if (!is.null(x)) {
          mutate(x, across(.fns = as.character))
        } else {
          NULL
        }
      }
    )
  ) %>%
  unnest(
    CLTech
  ) %>%
  type_convert() %>%
  rename_with(
    .fn = \(x) str_remove(x, "^CL_Tech"),
    .cols = everything()
  ) %>%
  rename(
    TechName = Name
  ) %>%
  select(
    -starts_with("Chair"), # only used in 2020
  ) %>%
  filter(
    year != "2020"
  ) %>%
  mutate(
    across(
      .fns = as.character
    )
  ) %>%
  pivot_longer(
    -c(IN_Operation, Calc, submissionId, year, OrgsNum, ToR, Desc, Ptcps, TechName),
    names_pattern = "Org([1-3]{1})(.*)",
    names_to = c("Num", "name")
  ) %>%
  filter(
    !is.na(name),
    Num <= OrgsNum
  ) %>%
  pivot_wider() %>%
  arrange(
    IN_Operation,
    year,
    TechName
  )

# no checking as based on names, so just giving back both sets of data

# save to a workbook

addWorksheet(
  wb_clusters,
  "CLTech"
)

writeData(
  wb_clusters,
  "CLTech",
  df_cltech
)

################################
#### SAVING ENTIRE WORKBOOK ####
################################

saveWorkbook(
  wb = wb_clusters,
  file = file.path(output_dir, "swaps_clusters_data.xlsx"),
  overwrite = TRUE
)
