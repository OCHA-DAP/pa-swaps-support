#!/bin/bash

# Runs the analysis
Rscript analyze_data/01_iccg.R
Rscript analyze_data/01_iccg_short.R
Rscript analyze_data/02_cluster.R
Rscript analyze_data/02_cluster_short.R
Rscript analyze_data/02_cluster_short_wkg.R
