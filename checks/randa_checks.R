source(
  file.path(
    "clean_data",
    "02_cluster.R"
  )
)

library(countrycode)

#################################
#### FILTERING TO SHORT LIST ####
#################################

df_cluster_wide <- filter(df_cluster_wide, IN_Operation_short)
df_clsub <- filter(df_clsub, as.logical(IN_Operation_short))
df_clsub_staffing <- filter(df_clsub_staffing, as.logical(IN_Operation_short))
df_clsub_staffing_class <- filter(df_clsub_staffing_class, as.logical(IN_Operation_short))
df_cluster_staffing <- filter(df_cluster_staffing, as.logical(IN_Operation_short))
df_cluster_staffing_class <- filter(df_cluster_staffing_class, as.logical(IN_Operation_short))
df_cluster_leadership <- filter(df_cluster_leadership, as.logical(IN_Operation_short))
df_cltech <- filter(df_cltech, as.logical(IN_Operation_short))

#####################
#### RANDAS DATA ####
#####################

df_clusters_randa <- readxl::read_excel(
  file.path(
    input_dir,
    "data_checks_randa",
    "List of clusters.xlsx"
  )
)

df_twg_randa <- readxl::read_excel(
  file.path(
    input_dir,
    "data_checks_randa",
    "Working group.xlsx"
  )
)

df_dedicated_randa <- readxl::read_excel(
  file.path(
    input_dir,
    "data_checks_randa",
    "RH dedicated analysis.xlsx"
  ),
  skip = 2
) |>
  transmute(
    Country = `Cntry...1`,
    IN_Operation = ifelse(
      Country != "oPt",
      countryname(Country, destination = "iso3c"),
      "PSE"
    ),
    CL_Sectors = Cluster
  )


# cluster check

df_clusters_randa |>
  anti_join(
    df_cluster_wide,
    by = c(
      "id" = "submissionId",
      "Type" = "IN_Type"
    )
  )

df_cluster_wide |>
  filter(
    IN_Type != "WKG"
  ) |>
  anti_join(
    df_clusters_randa,
    by = c(
      "submissionId" = "id"
    ),
    "T"
  )

# working group check

df_twg_randa |>
  mutate(
    id = as.character(id)
  ) |>
  anti_join(
    df_cltech |>
      filter(
        IN_Type != "WKG"
      ),
    by = c(
      "id" = "submissionId",
      "CL_TechCalc" = "Calc"
    )
  )

# dedication check

df_my <- df_cluster_staffing_class |>
  filter(
    Function == "CD",
    Staffing == "Dedicated",
    Role_analysis == "Co-lead/lead"
  ) |>
  group_by(
    IN_Operation
  ) |>
  summarize(
    n = n()
  ) |>
  arrange(
    desc(
      n
    )
  )

df_rand <- df_dedicated_randa |>
  group_by(
    IN_Operation
  ) |>
  summarize(
    n = n()
  ) |>
  arrange(
    desc(
      n
    )
  )

df_my |>
  left_join(
    df_rand |>
      rename(
        n_rand = n
      )
  ) |>
  View(

  )
