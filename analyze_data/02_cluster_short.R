source(
  file.path(
    "clean_data",
    "02_cluster.R"
  )
)

wb_cluster_analysis <- createWorkbook()

#' Joins data frames together ignoring column names and rows
join_list <- function(df_list) {
  df_list <- map(df_list, \(x) ungroup(x) |> mutate(joining_id = row_number()))
  df <- reduce(
    df_list,
    \(x, y) {
      full_join(x, y, by = "joining_id", suffix = c("@", "£"))
    }
  )
  df <- select(df, -joining_id)
  names(df) <- str_remove_all(names(df), "@|£")
  df
}

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

###############################
#### COORDINATION OVERVIEW ####
###############################

coord_overview <- list(
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `Clusters, sectors and AoRs at national level (not WKG)` = n(),
      `Clusters` = sum(IN_Type == "CLU"),
      `Sectors` = sum(IN_Type == "SEC"),
      `Sectors/Clusters` = sum(IN_Type == "CLU SEC"),
      `AOR` = sum(IN_Type == "AOR")
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Total # of:",
      values_to = "#"
    ),
  df_cltech |>
    filter(
      IN_Type != "WKG"
    ) |>
    distinct(
      IN_Operation, year, TechName
    ) |>
    summarize(
      `# technical working groups` = n()
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `#` = as.character(sum(CL_SAG == "Y")),
      `%` = scales::percent(sum(CL_SAG == "Y") / n())
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Clusters/Sectors/AOR with SAG"
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    select(
      starts_with("CL_SAGMembers"),
      -CL_SAGMembersOTHSpecify,
      -CL_SAGMembersTotalCalc
    ) |>
    type_convert() |>
    summarize(
      across(
        .cols = everything(),
        .fns = \(x) sum(x, na.rm = TRUE)
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "SAG Members",
      values_to = "%"
    ) |>
    arrange(
      desc(
        `%`
      )
    ) |>
    mutate(
      `SAG Members` = str_remove(`SAG Members`, "CL_SAGMembers"),
      `%` = scales::percent(`%` / sum(`%`))
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG",
      CL_SAG == "Y"
    ) |>
    summarize(
      `Average member # of SAG` = mean(as.numeric(CL_SAGMembersTotalCalc))
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    distinct(
      IN_Operation, submissionId, Calc
    ) |>
    summarize(
      `Total #clusters, sectors, AoRs at subnational level (not WKG)` = n()
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    distinct(
      IN_Operation,
      Loc
    ) |>
    summarize(
      `# of subnational locations` = n()
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    mutate(
      Area = strsplit(Area, " ")
    ) |>
    unnest(
      Area
    ) |>
    distinct(
      IN_Operation,
      Loc,
      Area
    ) |>
    summarize(
      `# of subnational locations and areas` = n()
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    distinct(
      IN_Operation,
      submissionId,
      Calc
    ) |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      `# of subnational clusters/sectors/AoRs` = n()
    ) |>
    arrange(
      desc(
        `# of subnational clusters/sectors/AoRs`
      )
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    left_join(
      distinct(
        df_clsub,
        submissionId = as.numeric(submissionId)
      ) |>
        mutate(
          subnational = TRUE
        ),
      by = "submissionId"
    ) |>
    mutate(
      subnational = replace_na(subnational, FALSE)
    ) |>
    summarize(
      `% clusters/sectors/AoRs with subnational presence` = scales::percent(mean(subnational))
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Coordination overview",
  join_list(coord_overview)
)

#####################################
#### CLUSTER LEADERSHIP NATIONAL ####
#####################################

cluster_leadership_national <- list(
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG",
      Role %in% c("LEAD", "COLEAD")
    ) |>
    group_by(
      `National lead/co-lead organizations` = Type
    ) |>
    summarize(
      `#` = n(),
      .groups = "drop"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ) |>
    arrange(
      desc(
        `#`
      )
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG",
      Role %in% c("LEAD", "COLEAD", "COCHAIR", "COFAC", "COCOORD")
    ) |>
    group_by(
      `National lead/co-lead/cochair organizations` = Type
    ) |>
    summarize(
      `#` = n(),
      .groups = "drop"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ) |>
    arrange(
      desc(
        `#`
      )
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG",
      Role %in% c("COCHAIR", "COFAC", "COCOORD")
    ) |>
    group_by(
      `National cochair organizations` = Type
    ) |>
    summarize(
      `#` = n(),
      .groups = "drop"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ) |>
    arrange(
      desc(
        `#`
      )
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      IN_Operation,
      CL_Sectors
    ) |>
    summarize(
      cochair = any(Role %in% c("COCHAIR", "COFAC", "COCOORD")),
      .groups = "drop"
    ) |>
    summarize(
      `% clusters/sectors/AoRs at national level having cochair` = scales::percent(mean(cochair))
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG"
    ) |>
    mutate(
      Sector = strsplit(CL_Sectors, " ")
    ) |>
    unnest(
      Sector
    ) |>
    group_by(
      Sector
    ) |>
    summarize(
      `# of co-chairs at national level` = sum(Role %in% c("COCHAIR", "COFAC", "COCOORD"))
    ) |>
    arrange(
      desc(
        `# of co-chairs at national level`
      )
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG",
      Role %in% c("COCHAIR", "COFAC", "COCOORD")
    ) |>
    summarize(
      `% of NGO co-chairs` = scales::percent(mean(Type %in% c("INGO", "NNGO")))
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      IN_Operation,
      CL_Sectors
    ) |>
    summarize(
      local = any(Type %in% c("NNGO", "NTA", "RCN", "LCA")),
      nngo = any(Type == "NNGO"),
      nta = any(Type %in% c("NTA", "LCA")),
      rcn = any(Type == "RCN"),
      .groups = "drop"
    ) |>
    summarize(
      `NNGO` = scales::percent(mean(nngo)),
      `Nat Auth` = scales::percent(mean(nta)),
      `RC-N` = scales::percent(mean(rcn)),
      `NNGO/Nat Auth/RC-N` = scales::percent(mean(local))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "National cluster lead/co-lead/chair",
      values_to = "% of clusters"
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `# of leadership roles held by NNGO/Govt/RC-N (global)` = sum(Type %in% c("NNGO", "NTA", "RCN", "LCA")),
      `% of leadership roles held by NNGO/Govt/RC-N (global)` = scales::percent(mean(Type %in% c("NNGO", "NTA", "RCN", "LCA")))
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG"
    ) |>
    mutate(
      Sector = strsplit(CL_Sectors, " ")
    ) |>
    unnest(
      Sector
    ) |>
    group_by(
      Sector
    ) |>
    summarize(
      `# of leadership roles held by NNGO/Govt/RC-N` = sum(Type %in% c("NNGO", "NTA", "RCN", "LCA")),
      `% of leadership roles held by NNGO/Govt/RC-N` = mean(Type %in% c("NNGO", "NTA", "RCN", "LCA"))
    ) |>
    arrange(
      desc(
        `% of leadership roles held by NNGO/Govt/RC-N`
      )
    ) |>
    mutate(
      `% of leadership roles held by NNGO/Govt/RC-N` = scales::percent(`% of leadership roles held by NNGO/Govt/RC-N`)
    ),
  df_cluster_leadership |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      IN_Operation,
      CL_SectorsID
    ) |>
    summarize(
      local = any(Type %in% c("NNGO", "NTA", "RCN", "LCA")),
      .groups = "drop"
    ) |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      `# national clusters with NNGO/Nat Auth/RCN as lead/colead/cochair` = sum(local),
      local_pct = mean(local),
      `% national clusters with NNGO/Nat Auth/RCN as lead/colead/cochair` = scales::percent(local_pct)
    ) |>
    arrange(
      desc(
        local_pct
      )
    ) |>
    select(
      -local_pct
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Cluster leadership (National)",
  join_list(cluster_leadership_national)
)

########################################
#### CLUSTER LEADERSHIP SUBNATIONAL ####
########################################

cluster_leadership_subnational <- list(
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    count(
      Type
    ) |>
    arrange(
      desc(n)
    ) |>
    mutate(
      `Leadership (lead/co-lead/co-chair) at subnational level` = scales::percent(n / sum(n))
    ) |>
    select(
      -n
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG",
      Role %in% c("LEAD", "COLEAD")
    ) |>
    count(
      Type
    ) |>
    arrange(
      desc(n)
    ) |>
    mutate(
      `Lead/co-lead only at subnational level` = scales::percent(n / sum(n))
    ) |>
    select(
      -n
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG",
      Role %in% c("COCHAIR", "COFAC", "COCOORD")
    ) |>
    count(
      Type
    ) |>
    arrange(
      desc(n)
    ) |>
    mutate(
      `Co-chair only at subnational level` = scales::percent(n / sum(n))
    ) |>
    select(
      -n
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      submissionId,
      Calc
    ) |>
    summarize(
      cochair = any(Role %in% c("COCHAIR", "COFAC", "COCOORD")),
      .groups = "drop"
    ) |>
    summarize(
      `% cluster/sector/AoR at subnational with co-chairs` = scales::percent(mean(cochair))
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      submissionId,
      Calc
    ) |>
    summarize(
      local = any(Type %in% c("NNGO", "NTA", "RCN", "LCA")),
      .groups = "drop"
    ) |>
    summarize(
      `% subnational clusters with NNGO/Nat Auth/RCN as lead/colead/cochair` = scales::percent(mean(local))
    ),
  df_clsub |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      IN_Operation,
      submissionId,
      Calc
    ) |>
    summarize(
      local = any(Type %in% c("NNGO", "NTA", "RCN", "LCA")),
      .groups = "drop"
    ) |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      `# subnational clusters with NNGO/Nat Auth/RCN as lead/colead/cochair` = sum(local),
      local_pct = mean(local),
      `% subnational clusters with NNGO/Nat Auth/RCN as lead/colead/cochair` = scales::percent(local_pct)
    ) |>
    arrange(
      desc(
        local_pct
      )
    ) |>
    select(
      -local_pct
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Cluster leadership (subnat)",
  join_list(cluster_leadership_subnational)
)

###################################
#### CLUSTER STAFFING NATIONAL ####
###################################

cluster_staffing_nat <- list(
  df_cluster_staffing_class |>
    filter(
      IN_Type != "WKG",
      Role_analysis == "Co-lead/lead",
      Function == "CD"
    ) |>
    count(
      `Coordinator coverage` = Staffing,
      name = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_cluster_staffing_class |>
    filter(
      IN_Type != "WKG",
      Function == "CD"
    ) |>
    group_by(
      Country = IN_Operation,
      CL_Sectors
    ) |>
    summarize(
      dedicated_coordinator = any(Role_analysis == "Co-lead/lead" & Staffing == "Dedicated"),
      .groups = "drop_last"
    ) |>
    summarize(
      `# of clusters with dedicated lead/co-lead nationally` = sum(dedicated_coordinator),
      `% of clusters with dedicated lead/co-lead nationally` = mean(dedicated_coordinator)
    ) |>
    arrange(
      desc(
        `% of clusters with dedicated lead/co-lead nationally`
      )
    ) |>
    mutate(
      `% of clusters with dedicated lead/co-lead nationally` = scales::percent(`% of clusters with dedicated lead/co-lead nationally`)
    ),
  df_cluster_staffing_class |>
    mutate(
      Sectors = strsplit(CL_Sectors, " ")
    ) |>
    unnest(
      Sectors
    ) |>
    filter(
      IN_Type != "WKG",
      Function == "CD"
    ) |>
    group_by(
      Sectors,
      IN_Operation
    ) |>
    summarize(
      dedicated_coordinator = any(Role_analysis == "Co-lead/lead" & Staffing == "Dedicated"),
      .groups = "drop_last"
    ) |>
    summarize(
      `# of clusters with dedicated lead/co-lead nationally` = sum(dedicated_coordinator),
      `% of clusters with dedicated lead/co-lead nationally` = mean(dedicated_coordinator)
    ) |>
    arrange(
      desc(
        `% of clusters with dedicated lead/co-lead nationally`
      )
    ) |>
    mutate(
      `% of clusters with dedicated lead/co-lead nationally` = scales::percent(`% of clusters with dedicated lead/co-lead nationally`)
    ),
  df_cluster_staffing_class |>
    filter(
      IN_Type != "WKG",
      Function == "CD"
    ) |>
    count(
      `Coordinator staffing all leads` = Staffing,
      name = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_cluster_staffing_class |>
    filter(
      IN_Type != "WKG",
      Role_analysis == "Co-lead/lead",
      Function == "IM"
    ) |>
    count(
      `IMO staffing (lead/co-lead)` = Staffing,
      name = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_cluster_staffing_class |>
    filter(
      IN_Type != "WKG",
      Function == "IM"
    ) |>
    group_by(
      Country = IN_Operation,
      CL_Sectors
    ) |>
    summarize(
      dedicated_imo = any(Role_analysis == "Co-lead/lead" & Staffing == "Dedicated"),
      .groups = "drop_last"
    ) |>
    summarize(
      `% of clusters with dedicated IMO nationally` = mean(dedicated_imo)
    ) |>
    arrange(
      desc(
        `% of clusters with dedicated IMO nationally`
      )
    ) |>
    mutate(
      `% of clusters with dedicated IMO nationally` = scales::percent(`% of clusters with dedicated IMO nationally`)
    ),
  df_cluster_staffing_class |>
    filter(
      IN_Type != "WKG",
      Function == "IM"
    ) |>
    count(
      `IMO staffing all leads` = Staffing,
      name = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Cluster staffing (national)",
  join_list(cluster_staffing_nat)
)


######################################
#### CLUSTER STAFFING SUBNATIONAL ####
######################################

cluster_staffing_subnat <- list(
  df_clsub_staffing_class |>
    filter(
      IN_Type != "WKG",
      Role_analysis == "Co-lead/lead",
      Function == "CD"
    ) |>
    count(
      `Coordinator coverage (subnational)` = Staffing,
      name = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_clsub_staffing_class |>
    filter(
      IN_Type != "WKG",
      Function == "CD"
    ) |>
    group_by(
      Country = IN_Operation,
      submissionId,
      Calc
    ) |>
    summarize(
      dedicated_coordinator = any(Role_analysis == "Co-lead/lead" & Staffing == "Dedicated"),
      .groups = "drop"
    ) |>
    group_by(
      Country
    ) |>
    summarize(
      `# of clusters with dedicated lead/co-lead subnationally` = sum(dedicated_coordinator),
      `% of clusters with dedicated lead/co-lead subnationally` = mean(dedicated_coordinator)
    ) |>
    arrange(
      desc(
        `% of clusters with dedicated lead/co-lead subnationally`
      )
    ) |>
    mutate(
      `% of clusters with dedicated lead/co-lead subnationally` = scales::percent(`% of clusters with dedicated lead/co-lead subnationally`)
    ),
  df_clsub_staffing_class |>
    mutate(
      Sectors = strsplit(CL_Sectors, " "),
    ) |>
    unnest(
      Sectors
    ) |>
    filter(
      IN_Type != "WKG",
      Function == "CD"
    ) |>
    group_by(
      Sectors,
      submissionId,
      Calc
    ) |>
    summarize(
      dedicated_coordinator = any(Role_analysis == "Co-lead/lead" & Staffing == "Dedicated"),
      .groups = "drop"
    ) |>
    group_by(
      Sectors
    ) |>
    summarize(
      `# of clusters with dedicated lead/co-lead subnationally` = sum(dedicated_coordinator),
      `% of clusters with dedicated lead/co-lead subnationally` = mean(dedicated_coordinator)
    ) |>
    arrange(
      desc(
        `% of clusters with dedicated lead/co-lead subnationally`
      )
    ) |>
    mutate(
      `% of clusters with dedicated lead/co-lead subnationally` = scales::percent(`% of clusters with dedicated lead/co-lead subnationally`)
    ),
  df_clsub_staffing_class |>
    filter(
      IN_Type != "WKG",
      Role_analysis == "Co-lead/lead",
      Function == "IM"
    ) |>
    count(
      `IM staffing (subnational)` = Staffing,
      name = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_clsub_staffing_class |>
    filter(
      IN_Type != "WKG",
      Function == "IM"
    ) |>
    group_by(
      Country = IN_Operation,
      submissionId,
      Calc
    ) |>
    summarize(
      dedicated_imo = any(Role_analysis == "Co-lead/lead" & Staffing == "Dedicated"),
      .groups = "drop"
    ) |>
    group_by(
      Country
    ) |>
    summarize(
      `% of clusters with dedicated IMO subnationally` = mean(dedicated_imo)
    ) |>
    arrange(
      desc(
        `% of clusters with dedicated IMO subnationally`
      )
    ) |>
    mutate(
      `% of clusters with dedicated IMO subnationally` = scales::percent(`% of clusters with dedicated IMO subnationally`)
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Cluster staffing (subnat)",
  join_list(cluster_staffing_subnat)
)

############################
#### CLUSTER MEMBERSHIP ####
############################

cluster_membership <- list(
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `Active members` = sum(as.numeric(CL_MembersActiveTotalCalc)),
      `Members` = sum(as.numeric(CL_MembersTotalCalc))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Cluster membership",
      values_to = "#"
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    select(
      starts_with("CL_Members"),
      -matches("Active|Total|Calc|Specify")
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "member",
      values_to = "#"
    ) |>
    mutate(
      member = str_remove(member, "CL_Members")
    ) |>
    group_by(
      `Cluster membership breakdown` = member
    ) |>
    summarize(
      `#` = sum(as.numeric(`#`), na.rm = TRUE)
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      `Active members` = sum(as.numeric(CL_MembersActiveTotalCalc)),
      `Members` = sum(as.numeric(CL_MembersTotalCalc))
    ) |>
    arrange(
      desc(
        Members
      )
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Cluster membership",
  join_list(cluster_membership)
)

##################################
#### TECHNICAL WORKING GROUPS ####
##################################

technical_working_group <- list(
  df_cltech |>
    filter(
      IN_Type != "WKG"
    ) |>
    distinct(
      IN_Operation,
      year,
      TechName
    ) |>
    summarize(
      `Total # TWG` = n()
    ),
  df_cltech |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      `Breakdown of chairs/FP` = Type
    ) |>
    summarize(
      `#` = n()
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_cltech |>
    filter(
      IN_Type != "WKG"
    ) |>
    mutate(
      Sector = strsplit(CL_Sectors, " ")
    ) |>
    unnest(
      Sector
    ) |>
    group_by(
      Sector
    ) |>
    summarize(
      `# of TWGs` = sum(length(unique(TechName)))
    ) |>
    arrange(
      desc(
        `# of TWGs`
      )
    ),
  df_cltech |>
    filter(
      IN_Type != "WKG"
    ) |>
    group_by(
      submissionId,
      TechName
    ) |>
    summarize(
      local = any(Type %in% c("NNGO", "NTA", "LCA", "RCN")),
      .groups = "drop"
    ) |>
    summarize(
      `% TWG with NNGO/Nat Auth/RC-N as lead/colead/cochair` = scales::percent(mean(local))
    )
)


write_swaps_data(
  wb_cluster_analysis,
  "Technical working group",
  join_list(technical_working_group)
)

##################################
#### CLUSTER RESPONSIBILITIES ####
##################################

cluster_responsibilities <- list(
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `% with Cluster Strategy` = scales::percent(sum(CL_Strat == "Y") / n())
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `% fully completed CCPM` = scales::percent(sum(CL_CCPM == "FULL") / n())
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `% with Transition Plans` = scales::percent(sum(CL_Trans == "Y") / n())
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `% with ToRs` = scales::percent(sum(CL_ToR == "Y") / n())
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    ) |>
    summarize(
      `% ToRs updated/developed 2020` = scales::percent(sum(as.Date(CL_ToRYear) >= "2020-01-01", na.rm = TRUE) / sum(!is.na(CL_ToRYear)))
    )
)


write_swaps_data(
  wb_cluster_analysis,
  "Cluster responsibilities",
  join_list(cluster_responsibilities)
)

##################
#### LANGUAGE ####
##################

language <- list(
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    )|>
    summarize(
      `National level` = scales::percent(sum(CL_LangOfficialNat != "NONE", na.rm = TRUE) / sum(!is.na(CL_LangOfficialNat))),
      `Subnational level` = scales::percent(sum(CL_LangOfficialSubnat != "NONE", na.rm = TRUE) / sum(!is.na(CL_LangOfficialSubnat)))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Clusters/sectors/AoRs reporting using official/local language",
      values_to = "%"
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG",
    ) |>
    summarize(
      `National level` = scales::percent(sum(CL_LangTransNat %in% c("3", "4"), na.rm = TRUE) / sum(!is.na(CL_LangTransNat))),
      `Subnational level` = scales::percent(sum(CL_LangTransSubnat %in% c("3", "4"), na.rm = TRUE) / sum(!is.na(CL_LangTransSubnat)))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Clusters/sectors/AoRs reporting translation available if necessary (3 - 4)",
      values_to = "%"
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG",
    ) |>
    summarize(
      `National level` = scales::percent(sum(CL_LangTransNat %in% c("4"), na.rm = TRUE) / sum(!is.na(CL_LangTransNat))),
      `Subnational level` = scales::percent(sum(CL_LangTransSubnat %in% c("4"), na.rm = TRUE) / sum(!is.na(CL_LangTransSubnat)))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Clusters/sectors/AoRs reporting translation available if necessary (4 only)",
      values_to = "%"
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG",
    ) |>
    summarize(
      `National level` = scales::percent(sum(CL_LangTransNat != "0", na.rm = TRUE) / sum(!is.na(CL_LangTransNat))),
      `Subnational level` = scales::percent(sum(CL_LangTransSubnat != "0", na.rm = TRUE) / sum(!is.na(CL_LangTransSubnat)))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Clusters/sectors/AoRs reporting translation available if necessary (1, 2, 3, and 4)",
      values_to = "%"
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG"
    )|>
    summarize(
      `% SAG AoRs at national using official/local language` = scales::percent(sum(CL_LangOfficialSAG != "NONE", na.rm = TRUE) / sum(!is.na(CL_LangOfficialSAG)))
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG",
      !is.na(CL_LangOfficialNat)
    ) |>
    transmute(
      French = str_detect(CL_LangOfficialNat, "French"),
      English = str_detect(CL_LangOfficialNat, "English"),
      Spanish = str_detect(CL_LangOfficialNat, "Spanish"),
      Arabic = str_detect(CL_LangOfficialNat, "Arabic"),
      Portuguese = str_detect(CL_LangOfficialNat, "Portuguese"),
      Ukrainian = str_detect(CL_LangOfficialNat, "Ukrainian"),
      Burmese = str_detect(CL_LangOfficialNat, "Burmese"),
      Malagasy = str_detect(CL_LangOfficialNat, "Malagasy"),
      Urdu = str_detect(CL_LangOfficialNat, "Urdu"),
      HaitianCreole = str_detect(CL_LangOfficialNat, "HaitianCreole"),
      Amharic = str_detect(CL_LangOfficialNat, "Amharic"),
      Somali = str_detect(CL_LangOfficialNat, "Somali"),
      Dari = str_detect(CL_LangOfficialNat, "Dari"),
      Pashto = str_detect(CL_LangOfficialNat, "Pashto")
    ) |>
    summarize(
      across(
        .cols = everything(),
        .fns = mean
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Language used at national level",
      values_to = "% of clusters"
    ) |>
    arrange(
      desc(
        `% of clusters`
      )
    ) |>
    mutate(
      `% of clusters` = scales::percent(`% of clusters`)
    ),
  df_cluster_wide |>
    filter(
      IN_Type != "WKG",
      !is.na(CL_LangOfficialSubnat)
    ) |>
    transmute(
      French = str_detect(CL_LangOfficialSubnat, "French"),
      English = str_detect(CL_LangOfficialSubnat, "English"),
      Spanish = str_detect(CL_LangOfficialSubnat, "Spanish"),
      Arabic = str_detect(CL_LangOfficialSubnat, "Arabic"),
      Portuguese = str_detect(CL_LangOfficialSubnat, "Portuguese"),
      Ukrainian = str_detect(CL_LangOfficialSubnat, "Ukrainian"),
      Burmese = str_detect(CL_LangOfficialSubnat, "Burmese"),
      Malagasy = str_detect(CL_LangOfficialSubnat, "Malagasy"),
      Urdu = str_detect(CL_LangOfficialSubnat, "Urdu"),
      HaitianCreole = str_detect(CL_LangOfficialSubnat, "HaitianCreole"),
      Amharic = str_detect(CL_LangOfficialSubnat, "Amharic"),
      Somali = str_detect(CL_LangOfficialSubnat, "Somali"),
      Dari = str_detect(CL_LangOfficialSubnat, "Dari"),
      Pashto = str_detect(CL_LangOfficialSubnat, "Pashto")
    ) |>
    summarize(
      across(
        .cols = everything(),
        .fns = mean
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Language used at subnational level",
      values_to = "% of clusters"
    ) |>
    arrange(
      desc(
        `% of clusters`
      )
    ) |>
    mutate(
      `% of clusters` = scales::percent(`% of clusters`)
    )
)

write_swaps_data(
  wb_cluster_analysis,
  "Language",
  join_list(language)
)

#################################
#### SAVING OUT THE WORKBOOK ####
#################################

saveWorkbook(
  wb = wb_cluster_analysis,
  file = file.path(output_dir, "swaps_clusters_analysis_short.xlsx"),
  overwrite = TRUE
)
