## Measure and Labor Market Effect Timeline

# Packages
pacman::p_load(dplyr, haven, ggplot2, survey, tidyr, labelled, srvyr, readxl, stargazer, gridExtra, openxlsx, clipr)

# Data Import
stw_ue_data <- read_excel("./data/raw_data/short_time_work_unemployment.xlsx") %>% mutate(year = as.integer(year))
stringency_data <- read_excel("./data/raw_data/stringency_index.xlsx")

# Data Prep
stringency_data_monthly <- stringency_data %>%
  group_by(year, month) %>%
  summarize(stringency_index_monthly = mean(stringency_index))

# Merge
timeline <- stringency_data_monthly %>% left_join(stw_ue_data %>% select(-ue_rate), join_by(year, month)) %>%
  mutate(date = as.Date(paste(year, "-", month, "-01", sep=""))) %>% ungroup() %>% select(-c(year, month)) %>%
  rename(stringency = stringency_index_monthly) %>%
  mutate(stringency = stringency * 60000)

## Visualize

# Palette
miggrp_bruecker_palette <- c("#0072B2", "#009E73", "#D55E00")

# Plot
measure_timeline <- ggplot(timeline, aes(x = date)) +
  geom_line(aes(y = ue_ind, color = "Unemployed Individuals"), linewidth = 1.2) +
  geom_line(aes(y = stw_ind, color = "Short-Time Workers"), linewidth = 1.2) +
  geom_line(aes(y = stringency, color = "Stringency Index"), linewidth = 1.2) +
  scale_x_date(date_breaks = "2 month", labels = scales::date_format("%b '%y")) +
  labs(title = "Labor Market & Measure Timeline",
       x = "Months",
       y = "Values",
       color = "") +
  scale_color_manual(values = miggrp_bruecker_palette) +
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
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(
    name = "Unemployed / Short-Time (in Mil.)",
    breaks = seq(0, 6000000, by = 1500000),
    labels = scales::comma_format(scale = 1e-6, accuracy = 0.1),
    sec.axis = sec_axis(~./60000, name = "Stringency Index")
  )

ggsave("./images/measure_timeline.jpeg", plot = measure_timeline, width = 10, height = 6)
