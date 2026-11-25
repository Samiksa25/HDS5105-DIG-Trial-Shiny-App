library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)

dig <- read.csv("Data/DIG-1.csv")
dig$group <- factor(dig$TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin"))
dig$age_group <- cut(dig$AGE, breaks = c(0, 60, 70, 100), labels = c("Under 60", "60–70", "Over 70"), right = FALSE)
dig$diabetes <- factor(dig$DIABETES, levels = c(0, 1), labels = c("No diabetes", "Diabetes"))

benefit <- dig %>%
  group_by(age_group, group) %>%
  summarise(percent = mean(DWHF, na.rm = TRUE) * 100, .groups = "drop")
Benefit <- ggplot(benefit, aes(x = age_group, y = percent, fill = group)) +
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(title = "Who benefited most?",
       subtitle = "Died or hospitalised for heart failure, by age group",
       x = "",
       y = "Percentage of patients (%)",
       fill = "")
Benefit

benefit_diab <- dig %>%
  group_by(diabetes, group) %>%
  summarise(percent = mean(DWHF, na.rm = TRUE) * 100, .groups = "drop")

Benefit_diab <- ggplot(benefit_diab, aes(x = diabetes, y = percent, fill = group)) +
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(title = "Who benefited most?",
       subtitle = "Died or hospitalised for heart failure, by diabetes",
       x = "",
       y = "Percentage of patients (%)",
       fill = "")
Benefit_diab
