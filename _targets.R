# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(janitor)
library(tidyverse)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Define file paths
iccg_data_fp <- file.path(
  Sys.getenv("SWAPS_SUPPORT"),
  "output",
  "swaps_iccg_data.xlsx"
)
clusters_data_fp <- file.path(
  Sys.getenv("SWAPS_SUPPORT"),
  "output",
  "swaps_clusters_data.xlsx"
)

# Replace the target list below with your own:
list(

  # track input files -- downstream targets don't need to change unless file changes or operations change
  tar_target(
    name = iccg_file,
    command = iccg_data_fp,
    format = "file"
  ),
  tar_target(
    name = clusters_data_file,
    command = clusters_data_fp,
    format = "file"
  ),

  # load iccg workbooks with names cleaned up
  tar_target(
    name = iccg_clean,
    command = load_swaps(fp = iccg_file)
  ),
  # load clusters workbooks with names cleaned up
  tar_target(
    name = clusters_clean,
    command = load_swaps(fp = clusters_data_file)
  )
)
