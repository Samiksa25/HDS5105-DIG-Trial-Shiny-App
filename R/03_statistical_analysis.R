
# ============================================================
# 03_STATISTICAL_ANALYSIS.R
# DIG Trial Explorer
# ============================================================

library(dplyr)

# 1. Load cleaned dataset
DIG_clean <- readRDS("Data/DIG_clean.rds")

# 2. Chi-square test: Treatment and hospitalisation
hospital_table <- table(
  DIG_clean$TRTMT_label,
  DIG_clean$HOSP_label
)

hospital_test <- chisq.test(hospital_table)

cat("\nCHI-SQUARE TEST: HOSPITALISATION\n")
print(hospital_table)
print(hospital_test)

# 3. Chi-square test: Treatment and mortality
mortality_table <- table(
  DIG_clean$TRTMT_label,
  DIG_clean$DEATH_label
)

mortality_test <- chisq.test(mortality_table)

cat("\nCHI-SQUARE TEST: MORTALITY\n")
print(mortality_table)
print(mortality_test)

# 4. Independent t-test: Age
age_test <- t.test(
  AGE ~ TRTMT_label,
  data = DIG_clean
)

cat("\nT-TEST: AGE\n")
print(age_test)

# 5. Independent t-test: BMI
bmi_test <- t.test(
  BMI ~ TRTMT_label,
  data = DIG_clean
)

cat("\nT-TEST: BMI\n")
print(bmi_test)

# 6. Calculate hospitalisation risk by group
hospital_risk <- DIG_clean %>%
  group_by(TRTMT_label) %>%
  summarise(
    Total = sum(!is.na(HOSP)),
    Events = sum(HOSP == 1, na.rm = TRUE),
    Risk_Percentage = round(
      mean(HOSP == 1, na.rm = TRUE) * 100, 2
    ),
    .groups = "drop"
  )

cat("\nHOSPITALISATION RISK\n")
print(hospital_risk)

# 7. Calculate mortality risk by group
mortality_risk <- DIG_clean %>%
  group_by(TRTMT_label) %>%
  summarise(
    Total = sum(!is.na(DEATH)),
    Events = sum(DEATH == 1, na.rm = TRUE),
    Risk_Percentage = round(
      mean(DEATH == 1, na.rm = TRUE) * 100, 2
    ),
    .groups = "drop"
  )

cat("\nMORTALITY RISK\n")
print(mortality_risk)

# 8. Absolute risk difference
hospital_rates <- prop.table(hospital_table, 1)[, "Yes"]
mortality_rates <- prop.table(mortality_table, 1)[, "Death"]

hospital_risk_difference <- (
  hospital_rates["Digoxin"] - hospital_rates["Placebo"]
) * 100

mortality_risk_difference <- (
  mortality_rates["Digoxin"] - mortality_rates["Placebo"]
) * 100

cat("\nABSOLUTE RISK DIFFERENCE (percentage points)\n")
cat("Hospitalisation:", hospital_risk_difference, "\n")
cat("Mortality:", mortality_risk_difference, "\n")

# 9. Save statistical results
statistical_results <- list(
  hospital_table = hospital_table,
  hospital_test = hospital_test,
  mortality_table = mortality_table,
  mortality_test = mortality_test,
  age_test = age_test,
  bmi_test = bmi_test,
  hospital_risk = hospital_risk,
  mortality_risk = mortality_risk,
  hospital_risk_difference = hospital_risk_difference,
  mortality_risk_difference = mortality_risk_difference
)

saveRDS(
  statistical_results,
  "Data/statistical_results.rds"
)

cat("\nStatistical analysis completed successfully.\n")
