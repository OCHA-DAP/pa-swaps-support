library(targets)
library(janitor)
library(tidyverse)

tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)
options(clustermq.scheduler = "multicore")
tar_source()

list(

# Track input files -------------------------------------------------------
# this way if the files change targets will get re-run
  tar_target(
    name = iccg_file,
    command = file.path(
      Sys.getenv("SWAPS_SUPPORT"),
      "output",
      "swaps_iccg_data.xlsx"
    ),
    format = "file"
  ),
  tar_target(
    name = clusters_data_file,
    command = file.path(
      Sys.getenv("SWAPS_SUPPORT"),
      "output",
      "swaps_clusters_data.xlsx"
    ),
    format = "file"
  ),
  tar_target(
    name = iccg_tool_file,
    command = file.path(Sys.getenv("SWAPS_SUPPORT"),"CDM2022-HCTICCG_XLSForms_V1.0.xlsx"),
    format = "file"
  ),
  tar_target(
    name = cluster_tool_file,
    command = file.path(Sys.getenv("SWAPS_SUPPORT"),"CDM2022-ClusterSector_XLSForms_V1.0.xlsx"),
    format = "file"
  ),

# Read Data ---------------------------------------------------------------
## Data sets to Analyze ####
  tar_target(
    name = iccg_clean,
    command = load_swaps(fp = iccg_file)
  ),
  # load clusters workbooks with names cleaned up
  tar_target(
    name = clusters_clean,
    command = load_swaps(fp = clusters_data_file)
  ),
## Tools ####
  tar_target(
    name = tool_iccg,
    command= read_all_tabs(fp = iccg_tool_file,
                           clean_names = F,
                           skip=0,
                           sheet_names = c("survey","choices")
                           )
    ),
  tar_target(
    name = tool_clusters,
    command= read_all_tabs(fp = cluster_tool_file,
                           clean_names = F,
                           skip=0,
                           sheet_names = c("survey","choices")
                           )
    )
)
