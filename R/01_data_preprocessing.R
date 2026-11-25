library(dplyr)

# Load the original dataset
DIG <- read.csv("Data/DIG-1.csv")

# Create a cleaned copy
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

# Check the cleaned data
head(DIG_clean)

# Check treatment groups
table(DIG_clean$TRTMT_label)

# Save the cleaned dataset
saveRDS(DIG_clean, "Data/DIG_clean.rds")

# Check dataset dimensions
dim(DIG_clean)

# Check missing values
colSums(is.na(DIG_clean))

# Check patient demographics
table(DIG_clean$SEX_label)
table(DIG_clean$DEATH_label)

# Check treatment and hospitalisation
table(DIG_clean$TRTMT_label, DIG_clean$HOSP_label)
