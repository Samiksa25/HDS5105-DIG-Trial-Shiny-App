
# ============================================================
# 01_DATA_PREPROCESSING.R
# DIG Trial Explorer
# ============================================================

library(dplyr)

# 1. Load the original dataset
DIG <- read.csv("Data/DIG-1.csv")

# 2. Check the original dataset
cat("Original dataset dimensions:\n")
print(dim(DIG))

cat("\nColumn names:\n")
print(names(DIG))

# 3. Create a cleaned copy
DIG_clean <- DIG %>%
  mutate(
    TRTMT_label = factor(
      TRTMT,
      levels = c(0, 1),
      labels = c("Placebo", "Digoxin")
    ),
    
    SEX_label = factor(
      SEX,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    DEATH_label = factor(
      DEATH,
      levels = c(0, 1),
      labels = c("Alive", "Death")
    ),
    
    HOSP_label = factor(
      HOSP,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# 4. Check the cleaned dataset
cat("\nFirst six rows:\n")
print(head(DIG_clean))

cat("\nCleaned dataset dimensions:\n")
print(dim(DIG_clean))

# 5. Check treatment group distribution
cat("\nPatients in each treatment group:\n")
print(table(DIG_clean$TRTMT_label, useNA = "ifany"))

# 6. Check patient demographics
cat("\nSex distribution:\n")
print(table(DIG_clean$SEX_label, useNA = "ifany"))

cat("\nMortality distribution:\n")
print(table(DIG_clean$DEATH_label, useNA = "ifany"))

# 7. Check hospitalisation by treatment group
cat("\nHospitalisation by treatment group:\n")
print(
  table(
    DIG_clean$TRTMT_label,
    DIG_clean$HOSP_label,
    useNA = "ifany"
  )
)

# 8. Missing values by variable
missing_summary <- data.frame(
  Variable = names(DIG_clean),
  Missing_Count = colSums(is.na(DIG_clean)),
  Missing_Percentage = round(
    colMeans(is.na(DIG_clean)) * 100,
    2
  )
)

cat("\nMissing value summary:\n")
print(missing_summary)

# 9. Save the cleaned dataset
saveRDS(
  DIG_clean,
  "Data/DIG_clean.rds"
)

# 10. Save missing value summary for later use
write.csv(
  missing_summary,
  "Data/missing_summary.csv",
  row.names = FALSE
)

cat("\nPreprocessing completed successfully.\n")

