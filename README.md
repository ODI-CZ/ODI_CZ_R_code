ODI-CZ validation study — analysis code
This repository contains the R analysis code for the ODI-CZ validation study.
Patient-level data are not included in this public repository to reduce the risk of re-identification. The script is intended to be run locally with the private analysis dataset.
Files
`ODI_CZ_validation_analysis.R` — main R analysis script.
`.gitignore` — prevents accidental upload of local data and generated outputs.
Required local data file
To reproduce the analysis locally, place the private dataset in the repository root as:
```text
ODI_data_github.csv
```
The expected file format is semicolon-separated CSV with decimal point (`.`) and the following columns:
```text
Patient_id, DG_group3, Age, Sex, Pain_duration_years,
History_of_surgery, NRS_PMD, ODI1, ODI2, ODI3, ODI4, ODI5,
ODI6, ODI7, ODI8, ODI9, ODI10
```
Expected coding:
`Sex`: `male`, `female`
`History_of_surgery`: `YES`, `NO`
`ODI1`–`ODI10`: integer values 0–5, missing values allowed
`NRS_PMD`: 0–5, missing values allowed
Install packages
```r
install.packages(c(
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "psych",
  "lavaan",
  "semPlot"
))
```
