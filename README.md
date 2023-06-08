# Introduction

This repository holds the analysis done by the UN OCHA Centre for Humanitarian
Data on the SWAPS 2022 CDM surveys.

## Data checking

The first step is to help with the cleaning process by checking the current data
against data collected in the previous year. 

Data was collected using the
ODK system with repeat groups, with two primary surveys, one for ICCGs and HCTs
and another for cluster coordination bodies. The data is available in two
separate JSON files, and are processed into data frames for each primary survey
and repreat group within.

These are saved out as two Excel files respectively: `swaps_iccg_data.xlsx` and
`swaps_clusters_data.xlsx`.

### Checks

For each file and sheet (representative of repeat groups from the survey),
cleaning is done slightly differently. 

- *Full:* For sheets where each row and column can
be uniquely identified, such as the main sheets for both surveys, the data is
presented with all questions for 2020, 2021, and 2022 next to each other, and
comparisons done directly between the 2021 and 2022. 2022 values are highlighted
if they differ from 2021
- *Partial:* For sheets without uniquely identified rows and columns,
such as where the respondent could list 3 leads of a working group, data is 
presented for 2022, and columns indicating a mismatch with 2021 data are added
next to it. This could be if a particular working group was not present in 2021,
for instance, or if a member has been removed. These checking columns are colored
if any check has been identified.
- *Incomplete:* This data is not cleaned, it is simply the responses for a
particular sheet that were not completed although a survey has been submited.
These are excluded from the main sheet and only included in the incomplete sheet.

- *None:* In a couple of instances, data changed too much between 2021 and 2022
for comparisons

**swaps_iccg_data.xlsx**

| Sheet | Cleaning |
| ----- | ------- |
| ICCG | Full |
| HCTOrg | Partial |
| SUBGroups | Partial |
| FMRRMOrg | Partial |
| HCTSubnatLoc | Partial |
| ICCSubnatLoc | Partial |

**swaps_clusters_data.xlsx**

| Sheet | Cleaning |
| ----- | ------- |
| Cluster| Full |
| ClusterIncomplete | Incomplete |
| CLSub_count | Full |
| CLSub| None |
| CLTech_count | Full |
| CLTech | None |

### Manual cleaning

These sheets are designed to be cleaned by the end user, and returned in the same format, after which analysis will be done direct from these sheets for 2022.
