DIG <- read.csv("Data/DIG-1.csv")
dim(DIG)

# Display column names
names(DIG)

# Display first six rows
head(DIG)

# Check missing values
colSums(is.na(DIG))

table(DIG$TRTMT)
table(DIG$SEX)
table(DIG$RACE)
table(DIG$DEATH)
table(DIG$HOSP)
range(DIG$AGE, na.rm = TRUE)