
# ============================================================
# 02_EXPLORATORY_ANALYSIS.R
# DIG Trial Explorer
# ============================================================

library(dplyr)
library(ggplot2)

# 1. Load the cleaned dataset
DIG_clean <- readRDS("Data/DIG_clean.rds")

# 2. Basic dataset information
cat("Dataset dimensions:\n")
print(dim(DIG_clean))

cat("\nDataset structure:\n")
str(DIG_clean)

# 3. Patient distribution by treatment group
treatment_summary <- DIG_clean %>%
  count(TRTMT_label, name = "Patients")

cat("\nPatients by treatment group:\n")
print(treatment_summary)

# 4. Age summary by treatment group
age_summary <- DIG_clean %>%
  group_by(TRTMT_label) %>%
  summarise(
    Patients = sum(!is.na(AGE)),
    Mean_Age = round(mean(AGE, na.rm = TRUE), 2),
    SD_Age = round(sd(AGE, na.rm = TRUE), 2),
    Median_Age = median(AGE, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nAge summary:\n")
print(age_summary)

# 5. Sex distribution by treatment group
sex_summary <- DIG_clean %>%
  count(TRTMT_label, SEX_label, name = "Patients")

cat("\nSex distribution:\n")
print(sex_summary)

# 6. BMI summary by treatment group
bmi_summary <- DIG_clean %>%
  group_by(TRTMT_label) %>%
  summarise(
    Mean_BMI = round(mean(BMI, na.rm = TRUE), 2),
    SD_BMI = round(sd(BMI, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nBMI summary:\n")
print(bmi_summary)

# 7. Mortality by treatment group
mortality_summary <- DIG_clean %>%
  group_by(TRTMT_label) %>%
  summarise(
    Total_Patients = n(),
    Deaths = sum(DEATH == 1, na.rm = TRUE),
    Death_Percentage = round(
      mean(DEATH == 1, na.rm = TRUE) * 100, 2
    ),
    .groups = "drop"
  )

cat("\nMortality summary:\n")
print(mortality_summary)

# 8. Hospitalisation by treatment group
hospital_summary <- DIG_clean %>%
  group_by(TRTMT_label) %>%
  summarise(
    Total_Patients = n(),
    Hospitalised = sum(HOSP == 1, na.rm = TRUE),
    Hospitalisation_Percentage = round(
      mean(HOSP == 1, na.rm = TRUE) * 100, 2
    ),
    .groups = "drop"
  )

cat("\nHospitalisation summary:\n")
print(hospital_summary)

# 9. Missing values
missing_summary <- data.frame(
  Variable = names(DIG_clean),
  Missing_Count = colSums(is.na(DIG_clean)),
  Missing_Percentage = round(
    colMeans(is.na(DIG_clean)) * 100, 2
  )
)

cat("\nMissing values:\n")
print(missing_summary)

# 10. Age distribution plot
age_plot <- ggplot(
  DIG_clean,
  aes(x = AGE, fill = TRTMT_label)
) +
  geom_histogram(
    bins = 30,
    alpha = 0.7,
    position = "identity"
  ) +
  labs(
    title = "Age Distribution by Treatment Group",
    x = "Age",
    y = "Number of Patients",
    fill = "Treatment Group"
  ) +
  theme_minimal()

print(age_plot)

# 11. BMI distribution plot
bmi_plot <- ggplot(
  DIG_clean,
  aes(x = TRTMT_label, y = BMI, fill = TRTMT_label)
) +
  geom_boxplot() +
  labs(
    title = "BMI Distribution by Treatment Group",
    x = "Treatment Group",
    y = "BMI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(bmi_plot)

# 12. Save exploratory analysis results
saveRDS(
  list(
    treatment_summary = treatment_summary,
    age_summary = age_summary,
    sex_summary = sex_summary,
    bmi_summary = bmi_summary,
    mortality_summary = mortality_summary,
    hospital_summary = hospital_summary,
    missing_summary = missing_summary
  ),
  "Data/exploratory_results.rds"
)

cat("\nExploratory analysis completed successfully.\n")

