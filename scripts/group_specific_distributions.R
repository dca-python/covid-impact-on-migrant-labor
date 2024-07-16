# Packages
pacman::p_load(dplyr, haven, ggplot2)

# Data
ppathl  <- read_dta("./data/raw_data/ppathl.dta", col_select = c("pid", "syear", "piyear", "migback", "arefback"))
pgen   <- read_dta("./data/raw_data/pgen.dta", col_select = c("pid", "syear", "pgmonth"))

# Calculate
visualize_workset_3 <- ppathl %>% inner_join(pgen) %>% filter(syear >= 2019 & piyear <= 2021) %>%
  mutate(miggrp_bruecker = case_when(
    migback == 1 ~ 1,
    migback != 1 & arefback != 2 ~ 2,
    arefback == 2 ~ 3,
    TRUE ~ NA_integer_
    )) %>%
  group_by(piyear, miggrp_bruecker, pgmonth) %>%
  summarize(year_group_month_sum = n()) %>%
  mutate(year_group_sum = sum(year_group_month_sum), fraction = year_group_month_sum/year_group_sum) %>% ungroup() %>%
  mutate(interview_date = as.Date(paste(piyear, "-", pgmonth, "-01", sep=""))) %>%
  select(interview_date, miggrp_bruecker, fraction) %>% mutate(miggrp_bruecker = as.factor(miggrp_bruecker))

# Palette
miggrp_bruecker_palette <- c("#0072B2", "#009E73", "#D55E00")

# Plot
group_specific_distributions <- ggplot(data = visualize_workset_3, aes(x = interview_date, y = fraction, color = miggrp_bruecker)) +
  geom_line(size = 0.75) + geom_point(size = 1.5) +
  scale_x_date(date_breaks = "2 month", labels = scales::date_format("%b '%y")) +
  scale_color_manual(
    values = miggrp_bruecker_palette,
    breaks = c(1, 2, 3),
    labels = c("Natives", "Non-Ref. Migrants", "Refugees")) +
  labs(
    title = "Group-Specific Distributions of Interview Months",
    x = "Months",
    y = "Fraction of Yearly Interviews",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(vjust = -1, size = 14),
    axis.title.y = element_text(vjust = 1, size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black", linetype = "solid", size = 0.2),
    legend.text = element_text(size = 14),
  )
    legend.text = element_text(size = 14)

ggsave("./images/group_specific_distributions.jpeg", plot = group_specific_distributions, width = 10, height = 6)
