# ============================================================
# ODI-CZ VALIDATION STUDY
# Descriptive statistics, floor/ceiling effects,
# concurrent validity, and internal consistency
# confirmatory factor analysis, and path diagrams
# ============================================================

# This script:
# 1. Loads the cleaned ODI-CZ dataset
# 2. Computes ODI index scores adjusted for missing ODI items
# 3. Creates descriptive statistics tables
# 4. Calculates floor and ceiling effects
# 5. Assesses concurrent validity with NRS-PMD
# 6. Estimates internal consistency using Cronbach's alpha
# 7. Performs confirmatory factor analysis
# 8. Creates CFA model fit table
# 9. Exports path diagrams of modified CFA models
# 10. Extracts supplementary CFA outputs

# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

# readr  : reading CSV files
# dplyr  : data manipulation
# tidyr  : reshaping tables
# tibble : clean table output
# psych  : Cronbach's alpha and psychometric statistics
# lavaan  : confirmatory factor analysis
# semPlot : CFA path diagrams

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(psych)
library(lavaan)
library(semPlot)

# ============================================================
# 2. LOAD DATA
# ============================================================

# Load the cleaned ODI-CZ dataset.
# read_csv2() is used because the file is semicolon-separated.
# Empty strings and "NA" are treated as missing values.

data <- read_csv2(
  "ODI_data_github.csv",
  na = c("", "NA")
)


# ============================================================
# 3. PREPARE VARIABLES
# ============================================================

# Convert pain duration to numeric.
# This is needed if the variable is imported as character.

data <- data %>%
  mutate(
    Pain_duration_years = as.numeric(Pain_duration_years)
  )


# Define ODI item names.
# ODI consists of 10 items, each scored from 0 to 5.

odi_items <- paste0("ODI", 1:10)


# Compute ODI index score on a 0–100 scale.
#
# The score is adjusted for missing ODI items:
#
# ODI_index = observed item sum /
#             maximum possible score for observed items * 100
#
# Example:
# - if all 10 items are answered, denominator = 10 * 5 = 50
# - if 9 items are answered, denominator = 9 * 5 = 45

data <- data %>%
  mutate(
    ODI_index = rowSums(across(all_of(odi_items)), na.rm = TRUE) /
      (rowSums(!is.na(across(all_of(odi_items)))) * 5) * 100,
    ODI_index = round(ODI_index)
  )


# Create ODI item-only dataset for later analyses.

odi_item_data <- data %>%
  select(all_of(odi_items))


# Item labels for manuscript-readable outputs.

item_labels <- c(
  ODI1  = "Pain intensity",
  ODI2  = "Self-care",
  ODI3  = "Lifting",
  ODI4  = "Walking",
  ODI5  = "Sitting",
  ODI6  = "Standing",
  ODI7  = "Sleeping",
  ODI8  = "Sex life",
  ODI9  = "Social life",
  ODI10 = "Travelling"
)


# ============================================================
# 4. DESCRIPTIVE STATISTICS
# ============================================================

# Helper function: format continuous variables as mean (SD),
# rounded to one decimal place.

mean_sd <- function(x) {
  paste0(
    round(mean(x, na.rm = TRUE), 1),
    " (",
    round(sd(x, na.rm = TRUE), 1),
    ")"
  )
}


# Helper function: count missing values.

n_missing <- function(x) {
  sum(is.na(x))
}


# ------------------------------------------------------------
# Table: Sample characteristics
# ------------------------------------------------------------

# Descriptive statistics are reported for the total sample
# and separately for men and women.

table_sample_characteristics <- tibble(
  Variable = c(
    "Age, years (SD)",
    "Duration of LBP, years (SD)",
    "History of lumbar spine surgery, n",
    "NRS-PMD (SD)",
    "ODI, 0–100 (SD)"
  ),
  
  Men = c(
    mean_sd(data$Age[data$Sex == "male"]),
    mean_sd(data$Pain_duration_years[data$Sex == "male"]),
    sum(data$History_of_surgery[data$Sex == "male"] == "YES", na.rm = TRUE),
    mean_sd(data$NRS_PMD[data$Sex == "male"]),
    mean_sd(data$ODI_index[data$Sex == "male"])
  ),
  
  Women = c(
    mean_sd(data$Age[data$Sex == "female"]),
    mean_sd(data$Pain_duration_years[data$Sex == "female"]),
    sum(data$History_of_surgery[data$Sex == "female"] == "YES", na.rm = TRUE),
    mean_sd(data$NRS_PMD[data$Sex == "female"]),
    mean_sd(data$ODI_index[data$Sex == "female"])
  ),
  
  Total = c(
    mean_sd(data$Age),
    mean_sd(data$Pain_duration_years),
    sum(data$History_of_surgery == "YES", na.rm = TRUE),
    mean_sd(data$NRS_PMD),
    mean_sd(data$ODI_index)
  ),
  
  Missing = c(
    n_missing(data$Age),
    n_missing(data$Pain_duration_years),
    n_missing(data$History_of_surgery),
    n_missing(data$NRS_PMD),
    n_missing(data$ODI_index)
  )
)

print(table_sample_characteristics, width = Inf)


# ------------------------------------------------------------
# Table: Diagnosis groups
# ------------------------------------------------------------

# Diagnosis groups are derived from ICD-10 diagnosis codes.
# Diagnosis codes are converted to uppercase to avoid
# case-sensitivity issues.

table_diagnosis <- data %>%
  mutate(
    DG_group3 = toupper(DG_group3),
    
    Diagnosis = case_when(
      DG_group3 %in% c("M511", "M541") ~ "Radiculopathy",
      DG_group3 %in% c("M539", "M545", "M549", "M479") ~ "Non-specific LBP",
      DG_group3 == "M961" ~ "Post-laminectomy syndrome",
      DG_group3 == "M439" ~ "Deforming dorsopathy",
      DG_group3 == "M480" ~ "Spinal stenosis",
      TRUE ~ NA_character_
    )
  ) %>%
  count(Diagnosis, Sex) %>%
  pivot_wider(
    names_from = Sex,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(
    Men = male,
    Women = female
  ) %>%
  mutate(
    Total = Men + Women
  ) %>%
  select(Diagnosis, Men, Women, Total)

print(table_diagnosis, width = Inf)


# Optional: number of participants without a diagnosis classification.

missing_diagnosis_n <- data %>%
  mutate(
    DG_group3 = toupper(DG_group3),
    Diagnosis = case_when(
      DG_group3 %in% c("M511", "M541") ~ "Radiculopathy",
      DG_group3 %in% c("M539", "M545", "M549", "M479") ~ "Non-specific LBP",
      DG_group3 == "M961" ~ "Post-laminectomy syndrome",
      DG_group3 == "M439" ~ "Deforming dorsopathy",
      DG_group3 == "M480" ~ "Spinal stenosis",
      TRUE ~ NA_character_
    )
  ) %>%
  summarise(Missing_diagnosis = sum(is.na(Diagnosis)))

print(missing_diagnosis_n)


# ============================================================
# 5. FLOOR AND CEILING EFFECTS
# ============================================================

# Floor and ceiling effects were calculated at:
# 1. Questionnaire level
# 2. Item level
#
# At questionnaire level, missing ODI items are allowed.
# Participants are included if they answered at least one ODI item.
#
# Floor effect:
# all available ODI item responses are equal to 0.
#
# Ceiling effect:
# all available ODI item responses are equal to 5.


# ------------------------------------------------------------
# Questionnaire-level floor and ceiling effects
# ------------------------------------------------------------

has_any_odi_response <- rowSums(!is.na(odi_item_data)) > 0

floor_cases <- apply(
  odi_item_data[has_any_odi_response, ],
  1,
  function(x) all(x[!is.na(x)] == 0)
)

ceiling_cases <- apply(
  odi_item_data[has_any_odi_response, ],
  1,
  function(x) all(x[!is.na(x)] == 5)
)

floor_ceiling_questionnaire <- tibble(
  Included_n = sum(has_any_odi_response),
  Floor_n = sum(floor_cases),
  Floor_percent = round(mean(floor_cases) * 100, 1),
  Ceiling_n = sum(ceiling_cases),
  Ceiling_percent = round(mean(ceiling_cases) * 100, 1)
)

print(floor_ceiling_questionnaire)


# ------------------------------------------------------------
# Item-level floor and ceiling effects
# ------------------------------------------------------------

# Missing item responses are excluded from the denominator
# for each individual item and reported separately.

floor_ceiling_items <- tibble(
  Item = names(item_labels),
  Label = unname(item_labels),
  
  N_answered = sapply(odi_item_data, function(x) sum(!is.na(x))),
  
  Floor_n = sapply(odi_item_data, function(x) sum(x == 0, na.rm = TRUE)),
  Floor_percent = sapply(odi_item_data, function(x) mean(x == 0, na.rm = TRUE) * 100),
  
  Ceiling_n = sapply(odi_item_data, function(x) sum(x == 5, na.rm = TRUE)),
  Ceiling_percent = sapply(odi_item_data, function(x) mean(x == 5, na.rm = TRUE) * 100),
  
  Missing = sapply(odi_item_data, function(x) sum(is.na(x)))
) %>%
  mutate(
    Floor_percent = round(Floor_percent, 1),
    Ceiling_percent = round(Ceiling_percent, 1)
  )

print(floor_ceiling_items, width = Inf)


# Optional: list items with floor effects of at least 15%.

floor_effect_items_15 <- floor_ceiling_items %>%
  filter(Floor_percent >= 15) %>%
  select(Item, Label, Floor_n, Floor_percent)

print(floor_effect_items_15, width = Inf)


# ============================================================
# 6. CONCURRENT VALIDITY
# ============================================================

# Concurrent validity was examined as the association between:
#
# ODI_index : ODI-CZ disability score, transformed to 0–100 scale
# NRS_PMD   : pain intensity score
#
# Pearson correlation was used as the primary analysis.
# Spearman correlation was used as a non-parametric sensitivity analysis.


# ------------------------------------------------------------
# Prepare complete cases for concurrent validity
# ------------------------------------------------------------

concurrent_data <- data %>%
  select(ODI_index, NRS_PMD) %>%
  filter(
    !is.na(ODI_index),
    !is.na(NRS_PMD)
  )

n_concurrent <- nrow(concurrent_data)


# ------------------------------------------------------------
# Pearson correlation
# ------------------------------------------------------------

pearson_validity <- cor.test(
  concurrent_data$ODI_index,
  concurrent_data$NRS_PMD,
  method = "pearson"
)

pearson_results <- tibble(
  Method = "Pearson",
  n = n_concurrent,
  Correlation = unname(pearson_validity$estimate),
  CI_lower = pearson_validity$conf.int[1],
  CI_upper = pearson_validity$conf.int[2],
  p_value = pearson_validity$p.value
) %>%
  mutate(
    Correlation = round(Correlation, 3),
    CI_lower = round(CI_lower, 3),
    CI_upper = round(CI_upper, 3),
    p_value = signif(p_value, 3)
  )

print(pearson_results)


# ------------------------------------------------------------
# Spearman correlation
# ------------------------------------------------------------

spearman_validity <- cor.test(
  concurrent_data$ODI_index,
  concurrent_data$NRS_PMD,
  method = "spearman",
  exact = FALSE
)

spearman_results <- tibble(
  Method = "Spearman",
  n = n_concurrent,
  Correlation = unname(spearman_validity$estimate),
  CI_lower = NA_real_,
  CI_upper = NA_real_,
  p_value = spearman_validity$p.value
) %>%
  mutate(
    Correlation = round(Correlation, 3),
    p_value = signif(p_value, 3)
  )

print(spearman_results)


# ------------------------------------------------------------
# Combined concurrent validity table
# ------------------------------------------------------------

concurrent_validity_results <- bind_rows(
  pearson_results,
  spearman_results
)

print(concurrent_validity_results, width = Inf)


# ============================================================
# 7. INTERNAL CONSISTENCY - CRONBACH'S ALPHA
# ============================================================

# Internal consistency of the ODI-CZ was assessed using Cronbach's alpha.
#
# Missing ODI item responses were handled using pairwise available observations.
# This allows participants with partially completed ODI questionnaires to
# contribute to the analysis, instead of excluding them completely.


# ------------------------------------------------------------
# Cronbach's alpha
# ------------------------------------------------------------

alpha_odi <- psych::alpha(
  odi_item_data,
  use = "pairwise"
)

# Full output
alpha_odi

# Raw Cronbach's alpha
alpha_odi$total$raw_alpha

# Standardized Cronbach's alpha
alpha_odi$total$std.alpha

# Approximate standard error of alpha
alpha_odi$total$ase


# ------------------------------------------------------------
# 95% confidence interval for Cronbach's alpha
# ------------------------------------------------------------

# The confidence interval is calculated using the asymptotic standard error
# returned by psych::alpha():
#
# CI = alpha ± 1.96 * SE

alpha_lower <- alpha_odi$total$raw_alpha - 1.96 * alpha_odi$total$ase
alpha_upper <- alpha_odi$total$raw_alpha + 1.96 * alpha_odi$total$ase

alpha_results <- tibble(
  Cronbach_alpha = round(alpha_odi$total$raw_alpha, 3),
  CI_lower = round(alpha_lower, 3),
  CI_upper = round(alpha_upper, 3),
  Standardized_alpha = round(alpha_odi$total$std.alpha, 3)
)

print(alpha_results)


# ------------------------------------------------------------
# Item-level internal consistency indices
# ------------------------------------------------------------

# Corrected item-total correlations indicate how strongly each item
# correlates with the total score computed from the remaining items.
#
# Alpha if item deleted estimates Cronbach's alpha after removing
# the given item from the scale.

item_consistency <- tibble(
  Item = names(item_labels),
  Label = unname(item_labels),
  Corrected_item_total_correlation = round(alpha_odi$item.stats$r.drop, 3),
  Alpha_if_item_deleted = round(alpha_odi$alpha.drop$raw_alpha, 3),
  N_valid = sapply(odi_item_data, function(x) sum(!is.na(x))),
  Missing = sapply(odi_item_data, function(x) sum(is.na(x)))
)

print(item_consistency, width = Inf)


# ------------------------------------------------------------
# Summary values for manuscript text
# ------------------------------------------------------------

min_item_total <- item_consistency %>%
  filter(
    Corrected_item_total_correlation ==
      min(Corrected_item_total_correlation, na.rm = TRUE)
  )

max_item_total <- item_consistency %>%
  filter(
    Corrected_item_total_correlation ==
      max(Corrected_item_total_correlation, na.rm = TRUE)
  )

min_alpha_drop <- item_consistency %>%
  filter(
    Alpha_if_item_deleted ==
      min(Alpha_if_item_deleted, na.rm = TRUE)
  )

max_alpha_drop <- item_consistency %>%
  filter(
    Alpha_if_item_deleted ==
      max(Alpha_if_item_deleted, na.rm = TRUE)
  )

internal_consistency_summary <- list(
  alpha = alpha_results,
  min_corrected_item_total = min_item_total,
  max_corrected_item_total = max_item_total,
  min_alpha_if_item_deleted = min_alpha_drop,
  max_alpha_if_item_deleted = max_alpha_drop
)

internal_consistency_summary


# ============================================================
# 8. CONFIRMATORY FACTOR ANALYSIS
# ============================================================

# CFA was performed using robust maximum likelihood estimation (MLR)
# with full information maximum likelihood (FIML) for missing ODI item data.

# ------------------------------------------------------------
# Prepare ODI item data for CFA
# ------------------------------------------------------------

cfa_data <- data %>%
  select(all_of(odi_items))


# ------------------------------------------------------------
# Specify baseline CFA models
# ------------------------------------------------------------

# Model 1: one-factor model
cfa_model_1 <- '
Disability =~ ODI1 + ODI2 + ODI3 + ODI4 + ODI5 +
       ODI6 + ODI7 + ODI8 + ODI9 + ODI10
'

# Model 2: two-factor model, loaded vs unloaded activities
cfa_model_2 <- '
Loaded =~ ODI3 + ODI4 + ODI6 + ODI8 + ODI9
Unloaded =~ ODI1 + ODI2 + ODI5 + ODI7 + ODI10
'

# Model 3: two-factor model, static vs dynamic activities
cfa_model_3 <- '
Dynamic =~ ODI2 + ODI3 + ODI4 + ODI8 + ODI9
Static =~ ODI1 + ODI5 + ODI6 + ODI7 + ODI10
'


# ------------------------------------------------------------
# Fit baseline CFA models
# ------------------------------------------------------------

fit_1 <- cfa(
  cfa_model_1,
  data = cfa_data,
  estimator = "MLR",
  missing = "fiml"
)

fit_2 <- cfa(
  cfa_model_2,
  data = cfa_data,
  estimator = "MLR",
  missing = "fiml"
)

fit_3 <- cfa(
  cfa_model_3,
  data = cfa_data,
  estimator = "MLR",
  missing = "fiml"
)


# ------------------------------------------------------------
# Inspect baseline model summaries
# ------------------------------------------------------------

summary(fit_1, fit.measures = TRUE, standardized = TRUE)
summary(fit_2, fit.measures = TRUE, standardized = TRUE)
summary(fit_3, fit.measures = TRUE, standardized = TRUE)

# ------------------------------------------------------------
# Specify modified CFA models
# ------------------------------------------------------------

# Modified Model 1: one-factor model
# Residual correlations allowed:
# - ODI1 ~~ ODI7: pain intensity and sleeping
# - ODI4 ~~ ODI6: walking and standing

cfa_model_1_mod <- '
Disability =~ ODI1 + ODI2 + ODI3 + ODI4 + ODI5 +
       ODI6 + ODI7 + ODI8 + ODI9 + ODI10

ODI1 ~~ ODI7
ODI4 ~~ ODI6
'


# Modified Model 2: two-factor model, loaded vs unloaded activities

cfa_model_2_mod <- '
Loaded =~ ODI3 + ODI4 + ODI6 + ODI8 + ODI9
Unloaded =~ ODI1 + ODI2 + ODI5 + ODI7 + ODI10

ODI1 ~~ ODI7
ODI4 ~~ ODI6
'


# Modified Model 3: two-factor model, static vs dynamic activities

cfa_model_3_mod <- '
Dynamic =~ ODI2 + ODI3 + ODI4 + ODI8 + ODI9
Static =~ ODI1 + ODI5 + ODI6 + ODI7 + ODI10

ODI1 ~~ ODI7
ODI4 ~~ ODI6
'


# ------------------------------------------------------------
# Fit modified CFA models
# ------------------------------------------------------------

fit_1_mod <- cfa(
  cfa_model_1_mod,
  data = cfa_data,
  estimator = "MLR",
  missing = "fiml"
)

fit_2_mod <- cfa(
  cfa_model_2_mod,
  data = cfa_data,
  estimator = "MLR",
  missing = "fiml"
)

fit_3_mod <- cfa(
  cfa_model_3_mod,
  data = cfa_data,
  estimator = "MLR",
  missing = "fiml"
)


# ------------------------------------------------------------
# Inspect modified model summaries
# ------------------------------------------------------------

summary(fit_1_mod, fit.measures = TRUE, standardized = TRUE)
summary(fit_2_mod, fit.measures = TRUE, standardized = TRUE)
summary(fit_3_mod, fit.measures = TRUE, standardized = TRUE)

# ------------------------------------------------------------
# Table: Model fit indices
# ------------------------------------------------------------

cfa_models <- list(
  "One-factor" = fit_1,
  "Two-factor (loaded/unloaded)" = fit_2,
  "Two-factor (dynamic/static)" = fit_3,
  "One-factor (modified)" = fit_1_mod,
  "Two-factor (loaded/unloaded, modified)" = fit_2_mod,
  "Two-factor (dynamic/static, modified)" = fit_3_mod
)

fit_indices <- lapply(names(cfa_models), function(model_name) {
  
  fit <- cfa_models[[model_name]]
  
  fm <- fitMeasures(
    fit,
    c(
      "chisq.scaled",
      "df.scaled",
      "pvalue.scaled",
      "cfi.robust",
      "rmsea.robust",
      "rmsea.ci.lower.robust",
      "rmsea.ci.upper.robust"
    )
  )
  
  tibble(
    Model = model_name,
    Chi_square = round(fm["chisq.scaled"], 3),
    df = unname(fm["df.scaled"]),
    p = round(fm["pvalue.scaled"], 3),
    Robust_CFI = round(fm["cfi.robust"], 3),
    Robust_RMSEA = round(fm["rmsea.robust"], 3),
    RMSEA_90CI = paste0(
      round(fm["rmsea.ci.lower.robust"], 3),
      " - ",
      round(fm["rmsea.ci.upper.robust"], 3)
    )
  )
  
})

fit_indices <- bind_rows(fit_indices)

print(fit_indices, width = Inf)

# ============================================================
# 9. PATH DIAGRAMS OF MODIFIED CFA MODELS
# ============================================================

# Path diagrams are exported as SVG files.
# SVG is a vector format, so figures remain sharp when resized.

# ------------------------------------------------------------
# Create output folder
# ------------------------------------------------------------

if (!dir.exists("figures")) {
  dir.create("figures", recursive = TRUE)
}


# ------------------------------------------------------------
# Helper function: create node labels in the correct semPlot order
# ------------------------------------------------------------

get_node_labels <- function(fit, label_map) {
  
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  
  labels <- label_map[node_names]
  labels[is.na(labels)] <- node_names[is.na(labels)]
  
  unname(labels)
}


# ------------------------------------------------------------
# Helper function: save CFA path diagram as SVG
# ------------------------------------------------------------

save_cfa_diagram_svg <- function(fit,
                                 file,
                                 label_map,
                                 width = 8,
                                 height = 8,
                                 sizeMan = 8,
                                 sizeLat = 9,
                                 edge_label_cex = 0.9) {
  
  # Create the output folder if it does not exist
  output_dir <- dirname(file)
  
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create node labels in the order expected by semPaths()
  node_labels <- get_node_labels(fit, label_map)
  
  # Open SVG device
  svg(
    filename = file,
    width = width,
    height = height
  )
  
  # Ensure the graphics device is closed even if plotting fails
  on.exit(dev.off(), add = TRUE)
  
  # Draw path diagram
  semPaths(
    fit,
    what = "std",
    whatLabels = "std",
    style = "lisrel",
    layout = "tree2",
    rotation = 2,
    residuals = TRUE,
    intercepts = FALSE,
    thresholds = FALSE,
    nCharNodes = 0,
    nodeLabels = node_labels,
    curvePivot = TRUE,
    edge.color = "black",
    color = list(
      lat = "white",
      man = "white"
    ),
    border.color = "black",
    fade = FALSE,
    sizeMan = sizeMan,
    sizeLat = sizeLat,
    edge.label.cex = edge_label_cex,
    mar = c(5, 5, 5, 5)
  )
}


# ------------------------------------------------------------
# Figure 1: Modified one-factor model
# ------------------------------------------------------------

label_map_1 <- c(
  "Disability"   = "Disability",
  "ODI1"  = "Pain",
  "ODI2"  = "Self-care",
  "ODI3"  = "Lift",
  "ODI4"  = "Walking",
  "ODI5"  = "Sitting",
  "ODI6"  = "Standing",
  "ODI7"  = "Sleeping",
  "ODI8"  = "Sex life",
  "ODI9"  = "Social life",
  "ODI10" = "Travel"
)

save_cfa_diagram_svg(
  fit = fit_1_mod,
  file = "figures/Figure_1_one_factor_modified.svg",
  label_map = label_map_1
)


# ------------------------------------------------------------
# Figure 2: Modified two-factor loaded/unloaded model
# ------------------------------------------------------------

label_map_2 <- c(
  "Loaded"   = "Loaded",
  "Unloaded" = "Unloaded",
  "ODI1"  = "Pain",
  "ODI2"  = "Self-care",
  "ODI3"  = "Lifting",
  "ODI4"  = "Walking",
  "ODI5"  = "Sitting",
  "ODI6"  = "Standing",
  "ODI7"  = "Sleeping",
  "ODI8"  = "Sex life",
  "ODI9"  = "Social life",
  "ODI10" = "Travelling"
)

save_cfa_diagram_svg(
  fit = fit_2_mod,
  file = "figures/Figure_2_loaded_unloaded_modified.svg",
  label_map = label_map_2
)


# ------------------------------------------------------------
# Figure 3: Modified two-factor dynamic/static model
# ------------------------------------------------------------

label_map_3 <- c(
  "Dynamic" = "Dynamic",
  "Static"  = "Static",
  "ODI1"  = "Pain",
  "ODI2"  = "Self-care",
  "ODI3"  = "Lifting",
  "ODI4"  = "Walking",
  "ODI5"  = "Sitting",
  "ODI6"  = "Standing",
  "ODI7"  = "Sleeping",
  "ODI8"  = "Sex life",
  "ODI9"  = "Social life",
  "ODI10" = "Travelling"
)

save_cfa_diagram_svg(
  fit = fit_3_mod,
  file = "figures/Figure_3_dynamic_static_modified.svg",
  label_map = label_map_3
)


# ============================================================
# 10. SUPPLEMENTARY CFA OUTPUTS
# ============================================================

# This section extracts supplementary CFA information:
# 1. Standardized factor loadings
# 2. Inter-factor correlations for two-factor models
# 3. Modification indices for baseline models


# ------------------------------------------------------------
# Helper function: extract standardized factor loadings
# ------------------------------------------------------------

print_wide <- function(x) {
  print(
    as_tibble(x),
    n = Inf,
    width = 200
  )
}

extract_factor_loadings <- function(fit, model_name) {
  
  standardizedSolution(fit) %>%
    filter(op == "=~") %>%
    mutate(
      Model = model_name,
      Item_label = item_labels[rhs]
    ) %>%
    select(
      Model,
      Factor = lhs,
      Item = rhs,
      Item_label,
      Std_loading = est.std,
      SE = se,
      z = z,
      p_value = pvalue
    ) %>%
    mutate(
      Std_loading = round(Std_loading, 3),
      SE = round(SE, 3),
      z = round(z, 3),
      p_value = signif(p_value, 3)
    )
}


# ------------------------------------------------------------
# Standardized factor loadings: all CFA models
# ------------------------------------------------------------

cfa_factor_loadings <- bind_rows(
  extract_factor_loadings(fit_1, "One-factor"),
  extract_factor_loadings(fit_2, "Two-factor (loaded/unloaded)"),
  extract_factor_loadings(fit_3, "Two-factor (dynamic/static)"),
  extract_factor_loadings(fit_1_mod, "One-factor (modified)"),
  extract_factor_loadings(fit_2_mod, "Two-factor (loaded/unloaded, modified)"),
  extract_factor_loadings(fit_3_mod, "Two-factor (dynamic/static, modified)")
)

print_wide(cfa_factor_loadings)


# ------------------------------------------------------------
# Standardized factor loadings: modified models only
# ------------------------------------------------------------

cfa_factor_loadings_modified <- cfa_factor_loadings %>%
  filter(grepl("modified", Model, ignore.case = TRUE))

print_wide(cfa_factor_loadings_modified)


# ------------------------------------------------------------
# Helper function: extract inter-factor correlations
# ------------------------------------------------------------

extract_factor_correlations <- function(fit, model_name, factors) {
  
  standardizedSolution(fit) %>%
    filter(
      op == "~~",
      lhs %in% factors,
      rhs %in% factors,
      lhs != rhs
    ) %>%
    mutate(
      Model = model_name
    ) %>%
    select(
      Model,
      Factor_1 = lhs,
      Factor_2 = rhs,
      Correlation = est.std,
      SE = se,
      z = z,
      p_value = pvalue
    ) %>%
    mutate(
      Correlation = round(Correlation, 3),
      SE = round(SE, 3),
      z = round(z, 3),
      p_value = signif(p_value, 3)
    )
}


# ------------------------------------------------------------
# Inter-factor correlations for two-factor models
# ------------------------------------------------------------

cfa_factor_correlations <- bind_rows(
  extract_factor_correlations(
    fit = fit_2,
    model_name = "Two-factor (loaded/unloaded)",
    factors = c("Loaded", "Unloaded")
  ),
  extract_factor_correlations(
    fit = fit_3,
    model_name = "Two-factor (dynamic/static)",
    factors = c("Dynamic", "Static")
  ),
  extract_factor_correlations(
    fit = fit_2_mod,
    model_name = "Two-factor (loaded/unloaded, modified)",
    factors = c("Loaded", "Unloaded")
  ),
  extract_factor_correlations(
    fit = fit_3_mod,
    model_name = "Two-factor (dynamic/static, modified)",
    factors = c("Dynamic", "Static")
  )
)

print_wide(cfa_factor_correlations)


# ------------------------------------------------------------
# Helper function: extract modification indices for residual correlations
# ------------------------------------------------------------

extract_residual_mi <- function(fit, model_name, minimum_mi = 3, top_n = 10) {
  
  modindices(fit, sort. = TRUE) %>%
    filter(
      op == "~~",
      lhs %in% odi_items,
      rhs %in% odi_items,
      lhs != rhs,
      mi >= minimum_mi
    ) %>%
    mutate(
      Model = model_name,
      lhs_label = item_labels[lhs],
      rhs_label = item_labels[rhs]
    ) %>%
    select(
      Model,
      lhs,
      lhs_label,
      op,
      rhs,
      rhs_label,
      MI = mi,
      EPC = epc,
      Std_EPC = sepc.all
    ) %>%
    mutate(
      MI = round(MI, 3),
      EPC = round(EPC, 3),
      Std_EPC = round(Std_EPC, 3)
    ) %>%
    slice_head(n = top_n)
}


# ------------------------------------------------------------
# Modification indices: baseline models
# ------------------------------------------------------------

# Modification indices are inspected for baseline CFA models only.
# The focus is on residual correlations among ODI items.

cfa_modification_indices <- bind_rows(
  extract_residual_mi(
    fit = fit_1,
    model_name = "One-factor"
  ),
  extract_residual_mi(
    fit = fit_2,
    model_name = "Two-factor (loaded/unloaded)"
  ),
  extract_residual_mi(
    fit = fit_3,
    model_name = "Two-factor (dynamic/static)"
  )
)

print_wide(cfa_modification_indices)


# ------------------------------------------------------------
# Modification indices specifically used in modified models
# ------------------------------------------------------------

# These are the residual correlations allowed in the modified models:
# ODI1 ~~ ODI7: pain intensity and sleeping
# ODI4 ~~ ODI6: walking and standing

selected_residual_mi <- cfa_modification_indices %>%
  filter(
    (lhs == "ODI1" & rhs == "ODI7") |
      (lhs == "ODI7" & rhs == "ODI1") |
      (lhs == "ODI4" & rhs == "ODI6") |
      (lhs == "ODI6" & rhs == "ODI4")
  )

print_wide(selected_residual_mi)
