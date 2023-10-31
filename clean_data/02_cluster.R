source(
  file.path(
    "clean_data",
    "utils.R"
  )
)

library(httr)

##########################
#### LOAD INPUT JSONS ####
##########################

df_cluster <- GET(
  url = "https://api.hpc.tools/v2/reportingwindows/assignments/export?type=operationCluster",
  authenticate(
    "hid",
    password = Sys.getenv("HPC_TOOLS_TOKEN")
  )
) |>
  content(
    as = "text"
  ) |>
  jsonlite::fromJSON() %>%
  pluck(1) %>%
  as_tibble() %>%
  mutate(
    year = 2019 + reportingWindowId,
    IN_Operation_short = !(IN_Operation %in% c("SLV", "GTM", "PAK")),
    CL_SectorsID = ifelse(
      CL_Sectors == "FSC" & IN_Operation == "ETH" & str_detect(CL_Name, "Agriculture"),
      "FSC-AG",
      CL_Sectors
    ),
    drop_response = case_when(
      IN_Operation == "BDI" & CL_Sectors == "PRO-HLP" ~ TRUE,
      IN_Operation == "BDI" & CL_Sectors == "PRO-GBV" ~ TRUE,
      IN_Operation == "CMR" & CL_Sectors == "ERY" ~ TRUE,
      IN_Operation == "ETH" & CL_Sectors == "PRO-CPN PRO-GBV" ~ TRUE,
      IN_Operation == "ETH" & CL_Sectors == "PRO PRO-GBV" ~ TRUE,
      IN_Operation == "ETH" & CL_Sectors == "PRO-CPN" ~ TRUE,
      IN_Operation == "ETH" & CL_Sectors == "TEL" ~ TRUE,
      IN_Operation == "ETH" & CL_Sectors == "PRO-HLP" ~ TRUE,
      IN_Operation == "HTI" & CL_Sectors == "TEL" ~ TRUE,
      IN_Operation == "HTI" & CL_Sectors == "SHL" ~ TRUE,
      IN_Operation == "LBY" & CL_Sectors == "LOG" ~ TRUE,
      IN_Operation == "LBY" & CL_Sectors == "HEA" ~ TRUE,
      IN_Operation == "LBY" & CL_Sectors == "SHL" ~ TRUE,
      IN_Operation == "PSE" & CL_Sectors == "LOG" ~ TRUE,
      IN_Operation == "UKR" & CL_Sectors == "HEA NUT" ~ TRUE,
      IN_Operation == "UKR" & CL_Sectors == "HEA" ~ TRUE,
      IN_Operation == "ZWE" & CL_Sectors == "HEA" ~ TRUE,
      is.na(CL_Sectors) & is.na(IN_Type) ~ TRUE,
      is.na(IN_TypeStrEn) ~ TRUE,
      .default = FALSE
    )
  ) %>%
  filter(
    year > 2021,
    !(IN_Operation %in% c("SYR-RG", "SYR-NE", "LBN", "PHL", "PFC", "SDN")),
    !drop_response
  ) |>
  arrange(
    IN_Operation
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
    IN_Operation_short,
    CL_SectorsID,
    CL_Sectors,
    CL_SectorsOther,
    IN_Type,
    everything(),
    -reportingWindowId,
    -where(is.list)
  ) |>
  arrange(
    IN_Operation,
    CL_SectorsID
  )

write_swaps_data(
  wb = wb_clusters,
  sheet = "Cluster",
  df = df_cluster_wide
)

#################################
#### CLUSTER LEADS: CL_leads ####
#################################

df_cluster_leadership <- df_cluster_wide |>
  select(
    IN_Operation,
    IN_Operation_short,
    IN_Type,
    CL_SectorsID,
    CL_OrgsNum,
    matches("CL_Org[0-9]{1}(Role|Type)")
  ) |>
  pivot_longer(
    cols = matches("CL_Org[0-9]{1}(Role|Type)"),
    names_to = c("OrgNum", "name"),
    names_pattern = "CL_Org([0-9]{1})(.*)"
  ) |>
  filter(
    !is.na(value)
  ) |>
  pivot_wider()

write_swaps_data(
  wb = wb_clusters,
  sheet = "CL_Leads",
  df = df_cluster_leadership
)

#################################
#### CLUSTER LEADS: CL_leads ####
#################################

df_cluster_staffing <- df_cluster_wide |>
  select(
    IN_Operation,
    IN_Operation_short,
    IN_Type,
    CL_SectorsID,
    CL_OrgsNum,
    matches("CL_Org[0-9]{1}(Staff)")
  ) |>
  pivot_longer(
    cols = matches("CL_Org[0-9]{1}(Staff)"),
    names_to = c("OrgNum", "Function", "name"),
    names_pattern = "CL_Org([0-9]{1})Staff([A-Z]{2})(.*)"
  ) |>
  filter(
    !is.na(value)
  ) |>
  pivot_wider() |>
  left_join(
    df_cluster_leadership,
    by = c("IN_Operation", "IN_Operation_short", "IN_Type", "CL_SectorsID", "CL_OrgsNum", "OrgNum")
  )

write_swaps_data(
  wb = wb_clusters,
  sheet = "CL_Staffing",
  df = df_cluster_staffing
)

##################################################
#### CLUSTER STAFFING NATIONAL CLASSIFICATION ####
##################################################

df_cluster_staffing_class <- df_cluster_staffing |>
  filter(
    !is.na(Full)
  ) |>
  mutate(
    across(
      .cols = c(Full, Dbl, Vac, NoP),
      .fns = as.numeric
    ),
    Role_analysis = ifelse(
      Role %in% c("LEAD", "COLEAD"),
      "Co-lead/lead",
      "Co-chair"
    )
  ) |>
  group_by(
    IN_Operation,
    IN_Operation_short,
    IN_Type,
    CL_SectorsID,
    Function,
    Role_analysis
  ) |>
  summarize(
    Staffing = case_when(
      any(Full >= 9) ~ "Dedicated",
      sum(Full >= 6) >= 2 ~ "Dedicated",
      any(Full >= 3) ~ "Partial",
      any(Dbl >= 9) ~ "Double",
      sum(Dbl >= 6) >= 2 ~ "Double",
      any(NoP + Vac >= 3) ~ "Vacant"
    ),
    .groups = "drop"
  )

write_swaps_data(
  wb = wb_clusters,
  sheet = "CL_Staffing_Classification",
  df = df_cluster_staffing_class
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
    `0` = sum(`0`, na.rm = TRUE),
    `1` = sum(`1`, na.rm = TRUE),
    .groups = "drop"
  )

write_swaps_data(
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
    IN_Operation_short,
    submissionId,
    year,
    CLSub = map(
      .x = CLSub,
      .f = \(x) {
        if (!is.null(x)) {
          mutate(x, across(.fns = as.character, .cols = everything()))
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
  mutate(
    across(
      .fns = as.character,
      .cols = everything()
    )
  ) %>%
  pivot_longer(
    -c(IN_Operation, IN_Operation_short, Calc, Loc, LocOther, Area, submissionId, year, OrgsNum, OrgsFootnote),
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
  ) %>%
  left_join(
    transmute(
      df_cluster_wide,
      submissionId = as.character(submissionId),
      IN_Type
    ),
    by = c("submissionId")
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

###################################
#### CLUSTER SUBGROUP STAFFING ####
###################################

df_clsub_staffing <- df_clsub |>
  select(
    IN_Operation,
    IN_Operation_short,
    IN_Type,
    submissionId,
    Calc,
    Num,
    Role,
    starts_with("Staff")
  ) |>
  pivot_longer(
    cols = starts_with("Staff"),
    names_to = c("Function", "name"),
    names_pattern = "Staff([A-Z]{2})(.*)"
  ) |>
  filter(
    !is.na(value),
    name != "Calc"
  ) |>
  pivot_wider()

write_swaps_data(
  wb = wb_clusters,
  sheet = "CLSub_Staffing",
  df = df_clsub_staffing
)

###################################
#### CLUSTER SUBGROUP STAFFING ####
###################################

df_clsub_staffing_class <- df_clsub_staffing |>
  mutate(
    across(
      .cols = c(Full, Dbl, Vac, NoP),
      .fns = as.numeric
    ),
    Role_analysis = ifelse(
      Role %in% c("LEAD", "COLEAD"),
      "Co-lead/lead",
      "Co-chair"
    )
  ) |>
  group_by(
    IN_Operation,
    IN_Operation_short,
    IN_Type,
    submissionId,
    Calc,
    Function,
    Role_analysis
  ) |>
  summarize(
    Staffing = case_when(
      any(Full >= 9) ~ "Dedicated",
      sum(Full >= 6) >= 2 ~ "Dedicated",
      any(Full >= 3) ~ "Partial",
      any(Dbl >= 9) ~ "Double",
      sum(Dbl >= 6) >= 2 ~ "Double",
      any(NoP + Vac >= 3) ~ "Vacant"
    ),
    .groups = "drop"
  )

write_swaps_data(
  wb = wb_clusters,
  sheet = "CLSub_Staffing_Class",
  df = df_clsub_staffing_class
)

########################################
#### CLUSTER SUBGROUP: CLTech_count ####
########################################

# unclear what this is but just using to count up total
# for each year and operation
df_cltech_count <- df_cluster %>%
  select(
    IN_Operation,
    IN_Operation_short,
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
  )

write_swaps_data(
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
    IN_Operation_short,
    IN_Type,
    CL_SectorsID,
    submissionId,
    year,
    CLTech = map(
      .x = CLTech,
      .f = \(x) {
        if (!is.null(x)) {
          mutate(x, across(.fns = as.character, .cols = everything()))
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
  mutate(
    across(
      .fns = as.character,
      .cols = everything()
    )
  ) %>%
  pivot_longer(
    -c(IN_Operation, IN_Operation_short, IN_Type, CL_SectorsID, Calc, submissionId, year, OrgsNum, ToR, Desc, Ptcps, TechName),
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
