library(ggplot2)
library(dplyr)
library(hrbrthemes)

dig <- read.csv("Data/DIG-1.csv")
# head(dig)
# summary(dig$AGE)
dig$group <- factor(dig$TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin"))

Age <- dig %>%
  ggplot( aes(x=AGE, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
  theme_ipsum() +
  labs(title = "How old were the patients?",
       x = "Age (years)",
       y = "Number of patients",
       fill = "")
Age

Bmi <- dig %>%
   ggplot( aes(x=BMI, fill=group)) +
   geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 2, na.rm = TRUE) +
   scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
   theme_ipsum() +
   labs(title = "Body weight of the patients",
        x = "BMI (kg/m²)",
        y = "Number of patients",
        fill = "")
Bmi

HR <- dig %>%
  ggplot( aes(x=HEARTRTE, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5, na.rm = TRUE) +
  scale_fill_manual(values=c("#6e03ab", "#0ac219")) +
  theme_ipsum() +
  labs(title = "What were the patients' heart rates?",
       x = "Heart rate (beats/min)",
       y = "Number of patients",
       fill = "")
HR

BP <- ggplot(dig) +
  geom_histogram( aes(x = SYSBP, y = after_stat(count), fill= group), color="#e9ecef",alpha = 0.6, position = 'identity', binwidth = 10, na.rm = TRUE) +
  geom_histogram( aes(x = DIABP, y = -after_stat(count), fill= group), color="#e9ecef", alpha = 0.6, position = 'identity', binwidth = 10, na.rm = TRUE) +
  theme_ipsum() +
  labs(title = "Blood pressure was almost identical in both groups",
       x = "Blood pressure(mmHg)",
       y = "Number of patients",
       fill = "") +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  annotate("label", x = 190, y = 300, label = "Systolic") +
  annotate("label", x = 40, y = -300, label = "Diastolic") +
  scale_y_continuous(labels = abs)
BP

dig$race <- factor(dig$RACE, levels = c(1, 2), labels = c("White", "Non-white"))
Race <- ggplot(dig, aes(x = race, fill = group)) +
  geom_bar(position = "dodge", color = "#e9ecef", width = 0.4) +
  coord_flip() +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  theme_ipsum() + theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Ethnic background of the patients",
       x = "",
       y = "Number of patients",
       fill = "")
Race

library(tidyr)
history <- dig %>%
  group_by(group) %>%
  summarise(
    `Prior heart attack`  = mean(PREVMI,   na.rm = TRUE) * 100,
    `Angina`              = mean(ANGINA,   na.rm = TRUE) * 100,
    `Diabetes`            = mean(DIABETES, na.rm = TRUE) * 100,
    `High blood pressure` = mean(HYPERTEN, na.rm = TRUE) * 100
  )
history_long <- history %>%
  pivot_longer(cols = -group, names_to = "condition", values_to = "percent")

History <- ggplot(history_long, aes(x = reorder(condition, percent), y = percent, fill = group)) +
  geom_col(position = "dodge", color = "#e9ecef", width = 0.4) +
  coord_flip() +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Medical history of the patients",
       x = "",
       y = "Percentage of patients (%)",
       fill = "")
History