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