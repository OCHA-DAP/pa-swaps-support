source(
  file.path(
    "clean_data",
    "01_iccg.R"
  )
)

wb_iccg_analysis <- createWorkbook()

#' Joins data frames together ignoring column names and rows
join_list <- function(df_list) {
  df_list <- map(df_list, \(x) ungroup(x) |> mutate(joining_id = row_number()))
  df <- reduce(
    df_list,
    \(x, y) {
      full_join(x, y, by = "joining_id", suffix = c("", ""))
    }
  )
  select(df, -joining_id)
}

#################################
#### FILTERING TO SHORT LIST ####
#################################

df_iccg_wide <- filter(df_iccg_wide, as.logical(IN_Operation_short))
df_hct <- filter(df_hct, as.logical(IN_Operation_short))
df_iccg <- filter(df_iccg, as.logical(IN_Operation_short))
df_icc_subnat <- filter(df_icc_subnat, as.logical(IN_Operation_short))
df_hct_subnat <- filter(df_hct_subnat, as.logical(IN_Operation_short))
df_fmrrm <- filter(df_fmrrm, as.logical(IN_Operation_short))
df_subgroups <- filter(df_subgroups, as.logical(IN_Operation_short))

###################################
#### HUMANITARIAN COUNTRY TEAM ####
###################################

hct <- list(
  df_iccg_wide |>
    group_by(
      `Frequency of meetings` = HCT_MtgFreq
    ) |>
    summarize(
      Countries = paste(IN_Operation, collapse = ", "),
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
  df_iccg_wide |>
    filter(
      !is.na(HCT_MtgFreqOther)
    ) |>
    distinct(
      Country = IN_Operation,
      `Frequency of meetings - other` = HCT_MtgFreqOther
    ),
  df_iccg_wide |>
    summarize(
      `Average HC attedance of HCT meetings` = scales::percent(mean(as.numeric(HCT_ChairAttHC)), scale = 1)
    ),
  df_iccg_wide |>
    summarize(
      `Average % of HCT members women` = scales::percent(mean(as.numeric(HCT_MembersGender)), scale = 1)
    ),
  df_iccg_wide |>
    distinct(
      IN_Operation,
      `% of HCT members women` = HCT_MembersGender
    ) |>
    arrange(
      desc(
        as.numeric(
          `% of HCT members women`
        )
      )
    ) |>
    mutate(
      `% of HCT members women` = paste0(`% of HCT members women`, "%")
    ),
  df_hct |>
    count(
      `Breakdown of organizational members` = Type,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_hct |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      hct_members = n()
    ) |>
    summarize(
      `Average HCT size` = mean(as.numeric(hct_members))
    ),
  df_hct |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      `# of HCT members` = n()
    ) |>
    arrange(
      desc(
        `# of HCT members`
      )
    ),
  df_hct |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      NNGO = "NNGO" %in% Type,
      NNGOF = "NNGOF" %in% Type,
      RCN = "RCN" %in% Type,
      NTA = "NTA" %in% Type
    ) |>
    mutate(
      ANY_LOCAL = NNGO | NNGOF | RCN | NTA
    ) |>
    summarize(
      across(
        NNGO:ANY_LOCAL,
        \(x) scales::percent(mean(x))
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Local/national orgs",
      values_to = "% of HCTs with this org as member"
    ),
  df_hct |>
    mutate(
      local = Type %in% c("NNGO", "NNGOF", "RCN", "NTA")
    ) |>
    summarize(
      `% of HCT members are LNA` = scales::percent(mean(local))
    ),
  df_hct |>
    summarize(
      `# of HCT members globally` = n()
    ),
  df_hct |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      nngo_rep = any(Type %in% c("NNGO", "NNGOF"))
    ) |>
    summarize(
      `% of HCTs with NNGO representation` = scales::percent(mean(nngo_rep))
    ),
  df_hct |>
    group_by(
      `HCTs without NNGOs` = IN_Operation
    ) |>
    filter(
      !any(Type %in% c("NNGO", "NNGOF"))
    ) |>
    distinct(
      `HCTs without NNGOs`
    ),
  df_hct |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      ifi = any(Type == "IFI"),
      pvt = any(Type == "PRV"),
      donor = any(Type == "DNR")
    ) |>
    summarize(
      `# HCTs with IFIs` = sum(ifi),
      `HCTs with IFIs` = paste(IN_Operation[ifi], collapse = ", "),
      `# HCTs with private sector actors` = sum(pvt),
      `HCTs with private sector actors` = paste(IN_Operation[pvt], collapse = ", "),
      `# HCTs with donors` = sum(donor)
    ),
  df_hct |>
    summarize(
      `Total donor seats` = sum(Type == "DNR")
    ),
  df_hct |>
    filter(
      Type == "DNR"
    ) |>
    count(
      `Donor on HCTs` = Name,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    filter(
      !is.na(
        HCT_INGOSelect
      )
    ) |>
    count(
      `Breakdown how INGOs selected` = HCT_INGOSelect,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    summarize(
      `% HCTs discussed representation of NNGOs` = scales::percent(sum(HCT_NNGORep == "Y") / n())
    ),
  df_iccg_wide |>
    filter(
      !is.na(
        HCT_NNGOSelect
      )
    ) |>
    count(
      `Breakdown how NNGOs selected` = HCT_NNGOSelect,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    summarize(
      `% of HCTs with separate donor group` = scales::percent(sum(str_detect(HCT_Donor, "SEP")) / n())
    ),
  df_iccg_wide |>
    mutate(
      ICC = str_detect(HCT_ICCGRep, "ICC"),
      CL = str_detect(HCT_ICCGRep, "CL"),
      OTHER = str_detect(HCT_ICCGRep, "OTHER"),
    ) |>
    summarize(
      across(
        ICC:OTHER,
        sum
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "ICCG Representation on HCT",
      values_to = "#"
    ) |>
    mutate(
      `%` = scales::percent(`#` / nrow(df_iccg_wide))
    ),
  df_iccg_wide |>
    summarize(
      `% of HCTs with ToRs` = scales::percent(sum(str_detect(HCT_ToRPresent, "Y")) / n())
    ),
  df_iccg_wide |>
    summarize(
      `% ToRs revised in last 3 years` = scales::percent(sum(as.Date(HCT_ToRDate) >= "2020-01-01", na.rm = TRUE) / sum(!is.na(HCT_ToRDate)))
    ),
  df_iccg_wide |>
    summarize(
      `% of HCTs with compacts` = scales::percent(sum(str_detect(HCT_CompactPresent, "Y")) / n())
    ),
  df_iccg_wide |>
    summarize(
      `% HCTs revised compacts in last 3 years` = scales::percent(sum(as.Date(HCT_CompactDate) >= "2020-01-01" & !is.na(HCT_CompactDate)) / n())
    ),
  df_iccg_wide |>
    summarize(
      `% of HCTs with workplan` = scales::percent(sum(str_detect(HCT_WPlanPresent, "Y")) / n())
    ),
  df_iccg_wide |>
    summarize(
      `% of HCTs conducted coord arch review` = scales::percent(sum(str_detect(HCT_ArcRev, "Y")) / n())
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "HCT",
  join_list(hct)
)

############################################################
#### HCT MANDATORY RESPONSIBILITIES AND THEMATIC ISSUES ####
############################################################

hct_responsibilities <- list(
  df_iccg_wide |>
    count(
      `HCT prot strategy or equivalent` = HCT_ProtStrat,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    filter(
      !is.na(HCT_ProtProgress)
    ) |>
    count(
      `HCT measure progress against its action plan` = HCT_ProtProgress,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT standing item on protection` = HCT_ProtStdItem,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT standing item on protection` = HCT_ProtDialogue,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT dialogue with relevant actors` = HCT_ProtDialogue,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT AAP action plan or strategy` = HCT_AAP,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `Full time PSEA coordinator` = HCT_PSEANetCoord,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    filter(
      !is.na(HCT_PSEANetCoordReport)
    ) |>
    count(
      `PSEA coordinator reports to the HCT` = HCT_PSEANetCoordReport,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCTs with joint HCT/UNCT action plan` = HCT_PSEAActionPlan,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCTs with IA feedback mechanism in place` = HCT_CFM,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    filter(
      !is.na(HCT_CFMPSEA)
    ) |>
    count(
      `CFM able to handle complaints on sensitive issues` = HCT_CFMPSEA,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT with dedicated gender adviser` = HCT_GenderAdvsr,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT gender engagement` = HCT_GenderEngmt,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT gender in workplan` = HCT_GenderWPlan,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT GBV strategy in place` = HCT_GBVStrat,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT disability mainstreaming` = HCT_Disability,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `HCT disability focal point` = HCT_DisabilityFocPt,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `CVA considered by HCT in 2022` = HCT_CashIncrease,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    mutate(
      cash_issues = strsplit(HCT_CashIssues, " ")
    ) |>
    unnest(
      cash_issues
    ) |>
    group_by(
      `HCTs discussed these cash issues` = cash_issues
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
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "HCT Responsibilities",
  join_list(hct_responsibilities)
)

################################################
#### HCT NATIONAL AND SUBNATIONAL LOCATIONS ####
################################################

hct_locations <- list(
  df_iccg_wide |>
    summarize(
      `# of operations with HCTs in 2 locations` = sum(!is.na(HCT_NatLoc2)),
      `Operations with HCTs in 2 locations` = paste(IN_Operation[!is.na(HCT_NatLoc2)])
    ),
  df_iccg_wide |>
    count(
      `HCT has subnational locations` = HCT_SubnatLocsPresent,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_hct_subnat |>
    summarize(
      `Total subnational HCTs` = n()
    ),
  df_hct_subnat |>
    count(
      `Countries with subnational HCTs` = IN_Operation,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ),
  df_hct_subnat |>
    count(
      `HCT subnational chairs` = Chair,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_hct_subnat |>
    summarize(
      across(
        c(NTA, DNR),
        \(x) scales::percent(sum(x == "Y" | is.na(x)) / n())
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = "HCT participation",
      values_to = "% subnational HCT"
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "HCT Locations",
  join_list(hct_locations)
)

##############
#### ICCG ####
##############

iccg <- list(
  df_iccg_wide |>
    count(
      `ICCG frequency of meetings` = ICC_MtgFreq,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    count(
      `ICCG chair majority of time` = ICC_Chair,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    select(
      starts_with("ICC_WkArea"),
      -matches("Note|Specify|Context|Ops")
    ) |>
    summarize(
      across(
        everything(),
        \(x) scales::percent(mean(as.numeric(x)), scale = 1)
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = "ICCG Area of Work",
      values_to = "Average %"
    ) |>
    mutate(
      `ICCG Area of Work` = str_remove(`ICCG Area of Work`, "ICC_WkArea")
    ),
  df_iccg_wide |>
    select(
      starts_with("ICC_Members"),
      -matches("Specify|TotalCalc|CoordNNGO|CofacNNGO|NGOColeads")
    ) |>
    pivot_longer(
      everything(),
      names_to = "ICC composition (global)",
      values_to = "#"
    ) |>
    mutate(
      `ICC composition (global)` = str_remove(`ICC composition (global)`, "ICC_Members")
    ) |>
    group_by(
      `ICC composition (global)`
    ) |>
    summarize(
      `#` = sum(as.numeric(`#`))
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_iccg_wide |>
    mutate(
      lna_member_no_coord = as.numeric(ICC_MembersNLA) + as.numeric(ICC_MembersNNGO),
      lna_member = as.numeric(ICC_MembersNLA) + as.numeric(ICC_MembersNNGO) + replace_na(as.numeric(ICC_MembersCofacNNGO), 0) + replace_na(as.numeric(ICC_MembersCoordNNGO), 0)
    ) |>
    summarize(
      `# of ICCG members that are LNA (not including NNGO cluster coordinators/chairs)` = sum(lna_member_no_coord),
      `% of ICCG members that are LNA (not including NNGO cluster coordinators/chairs)` = scales::percent(`# of ICCG members that are LNA (not including NNGO cluster coordinators/chairs)` / sum(as.numeric(ICC_MembersTotalCalc))),
      `# of ICCG members that are LNA (including NNGO cluster coordinators/chairs)` = sum(lna_member),
      `% of ICCG members that are LNA (including NNGO cluster coordinators/chairs)` = scales::percent(`# of ICCG members that are LNA (including NNGO cluster coordinators/chairs)` / sum(as.numeric(ICC_MembersTotalCalc)))
    ),
  df_iccg_wide |>
    mutate(
      lna_members_no_coord = as.numeric(ICC_MembersNLA) + as.numeric(ICC_MembersNNGO) > 0,
      lna_members = as.numeric(ICC_MembersNLA) + as.numeric(ICC_MembersNNGO) + replace_na(as.numeric(ICC_MembersCofacNNGO), 0) + replace_na(as.numeric(ICC_MembersCoordNNGO), 0) > 0
    ) |>
    summarize(
      `% of ICCGS with LNA members (not including NNGO cluster coordinators/chairs)` = scales::percent(sum(lna_members_no_coord) / n()),
      `% of ICCGs with LNA members (including NNGO cluster coordinators/chairs)` = scales::percent(sum(lna_members) / n())
    ),
  df_iccg_wide |>
    summarize(
      `% of ICCGs with ToRs` = scales::percent(sum(ICC_ToRPresent == "Y") / n()),
      `% ToRs revised in last 3 years` = scales::percent(sum(as.Date(ICC_ToRDate) >= "2020-01-01", na.rm = TRUE) / sum(!is.na(ICC_ToRDate)))
    ),
  df_iccg_wide |>
    summarize(
      `% of ICCGs with workplans` = scales::percent(sum(ICC_WPlanPresent == "Y") / n())
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "ICCG",
  join_list(iccg)
)

#################################################
#### ICCG NATIONAL AND SUBNATIONAL LOCATIONS ####
#################################################

iccg_locations <- list(
  df_iccg_wide |>
    summarize(
      `# of operations with subnational ICCGs` = sum(ICC_SubnatLocsPresent == "Y"),
      `% of operations with subnational ICCGs` = scales::percent(`# of operations with subnational ICCGs` / n())
    ),
  df_icc_subnat |>
    summarize(
      `# of subnational ICCGs globally` = n()
    ),
  df_icc_subnat |>
    group_by(
      Country = IN_Operation
    ) |>
    summarize(
      `# of subnational ICCGs` = n()
    ) |>
    arrange(
      desc(
        `# of subnational ICCGs`
      )
    ),
  df_icc_subnat |>
    group_by(
      Country = IN_Operation
    ) |>
    summarize(
      `# of subnational ICCGs` = n()
    ) |>
    summarize(
      `Average # of subnational ICCGs per country` = mean(`# of subnational ICCGs`)
    ),
  df_icc_subnat |>
    mutate(
      Chair = strsplit(Chair, " ")
    ) |>
    unnest(
      Chair
    ) |>
    count(
      `Chairs of subnational ICCGs` = Chair,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / nrow(df_icc_subnat))
    ),
  df_icc_subnat |>
    summarize(
      across(
        c(NTA, DNR),
        \(x) scales::percent(sum(x == "Y" | is.na(x)) / n())
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = "ICCG participation",
      values_to = "% of subnational ICCG"
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "ICCG Locations",
  join_list(iccg_locations)
)

###################
#### SUBGROUPS ####
###################

subgroups <- list(
  df_subgroups |>
    distinct(
      submissionId, NumCalc
    ) |>
    summarize(
      `# of total subgroups` = n()
    ),
  df_subgroups |>
    distinct(
      submissionId, NumCalc, Report
    ) |>
    count(
      `Subgroups reporting line` = Report,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_subgroups |>
    distinct(
      submissionId, NumCalc, Report, IN_Operation
    ) |>
    filter(Report %in% c("HCT", "ICCG")) |>
    group_by(
      `Subgroups reporting to:` = Report,
      IN_Operation
    ) |>
    summarize(
      num_subgroups = n(),
      .groups = "drop_last"
    ) |>
    summarize(
      `Average # of subgroups` = mean(num_subgroups)
    ),
  df_subgroups |>
    distinct(
      submissionId, NumCalc, Theme
    ) |>
    count(
      `Most common type of subgroup` = Theme,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "Subgroups",
  join_list(subgroups)
)

############################
#### OTHER COORDINATION ####
############################

other_coordination <- list(
  df_iccg_wide |>
    mutate(
      OTH_DeepField = strsplit(OTH_DeepField, " ")
    ) |>
    unnest(
      OTH_DeepField
    ) |>
    count(
      `Describe deep field locations: # / % countries` = OTH_DeepField,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / nrow(df_iccg_wide))
    ),
  df_iccg_wide |>
    summarize(
      `Total # of NGOF` = sum(as.numeric(FRM_NGONum)),
      `# operations with NGOF` = sum(as.numeric(FRM_NGONum) > 0)
    ),
  df_iccg_wide |>
    select(
      starts_with("FRM_NGOType")
    ) |>
    pivot_longer(everything()) |>
    filter(!is.na(value)) |>
    count(
      `NGO Forum types` = value,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ),
  df_iccg_wide |>
    summarize(
      `# of operations with RRM (same as # of RRM)` = sum(FRM_RRM == "Y"),
      `% of operations with RRM` = scales::percent(`# of operations with RRM (same as # of RRM)` / n())
    ),
  df_fmrrm |>
    filter(
      str_detect(Role, "LEAD")
    ) |>
    count(
      `RRM lead/managing organizations` = Type,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_fmrrm |>
    group_by(
      IN_Operation
    ) |>
    summarize(
      `# of operations with RRM comanaged by LTA/NTA (NTA and NNGO)` = any(str_detect(Role, "LEAD") & Type %in% c("NNGO", "NTA")),
      `# of operations with RRM comanaged by LTA/NTA (NTA only)` = any(str_detect(Role, "LEAD") & Type %in% c("NTA")),
    ),
  df_iccg_wide |>
    summarize(
      `# of countries with subnational RRM` = sum(FRM_RRMSubnat == "Y", na.rm = TRUE),
      `% of RRM that have subnational presence` = scales::percent(`# of countries with subnational RRM` / sum(!is.na(FRM_RRMSubnat)))
    ),
  df_iccg_wide |>
    filter(
      !is.na(FRM_RRMReport)
    ) |>
    count(
      `RRM reporting lines` = FRM_RRMReport,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ) |>
    mutate(
      `%` = scales::percent(`#` / sum(`#`))
    ),
  df_fmrrm |>
    count(
      `RRM role` = Role,
      `RRM org` = Type,
      name = "#"
    ) |>
    arrange(
      desc(
        `#`
      )
    ),
  df_iccg_wide |>
    summarize(
      `% of operations with joint development-humanitarian coord group` = scales::percent(sum(FRM_HDN == "Y") / n())
    )
)

write_swaps_data(
  wb_iccg_analysis,
  "Other Coordination",
  join_list(other_coordination)
)


#################################
#### SAVING OUT THE WORKBOOK ####
#################################

saveWorkbook(
  wb = wb_iccg_analysis,
  file = file.path(output_dir, "swaps_iccg_analysis_short.xlsx"),
  overwrite = TRUE
)

