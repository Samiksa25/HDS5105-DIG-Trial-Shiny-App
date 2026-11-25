library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)

dig <- read.csv("Data/DIG-1.csv")
dig$group <- factor(dig$TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin"))
effect <- dig %>%
  group_by(group) %>%
  summarise(
    `Died` = mean(DEATH, na.rm = TRUE) * 100,
    `Died or hospitalised for heart failure` = mean(DWHF, na.rm = TRUE) * 100
  )

effect_long <- effect %>%
  pivot_longer(cols = -group, names_to = "outcome", values_to = "percent")
Outcome <- ggplot(effect_long, aes(x = outcome, y = percent, fill = group)) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.4) +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Same survival, fewer hospital stays",
       x = "",
       y = "Percentage of patients (%)",
       fill = "")
Outcome

by_year <- dig %>%
  group_by(group) %>%
  summarise(
    `After 1 year`  = mean(DWHF == 1 & DWHFDAYS <= 365, na.rm = TRUE) * 100,
    `After 2 years` = mean(DWHF == 1 & DWHFDAYS <= 730, na.rm = TRUE) * 100
  )

by_year_long <- by_year %>%
  pivot_longer(cols = -group, names_to = "time", values_to = "percent")

Time <- ggplot(by_year_long, aes(x = time, y = percent, fill = group)) +
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", color = "#e9ecef", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5) +
  scale_fill_manual(values = c("#6e03ab", "#0ac219")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(title = "Did digoxin help?",
       subtitle = "Died or hospitalised for heart failure, over time",
       x = "",
       y = "Percentage of patients (%)",
       fill = "")
Time