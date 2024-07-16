# Packages
pacman::p_load(dplyr, haven, ggplot2, survey, tidyr, labelled, srvyr, readxl, stargazer, gridExtra, openxlsx, clipr)

# Imports
ppathl <- read_dta("./data/raw_data/ppathl.dta", col_select = c("pid", "syear", "piyear"))
pgen <- read_dta("./data/raw_data/pgen.dta", col_select = c("pid", "syear", "pgmonth"))

# Calculation Of The Individual Intervalls Between Interviews Over The Years
visualize_workset_1 <- ppathl %>%
  inner_join(pgen) %>%
  filter(syear >= 2018) %>%
  mutate(
    month_counter = case_when( # 2018 or later
      piyear == syear ~ pgmonth,
      piyear == syear + 1 ~ pgmonth + 12,
      piyear == syear + 2 ~ pgmonth + 24,
      TRUE ~ NA_integer_)
  ) %>% # month counter
  mutate_all(as.integer) %>%
  group_by(pid) %>%
  mutate(years_participated = n()) %>%
  # filtering out indiv. who did not participate every year
  filter(years_participated == 4) %>%
  mutate(months_passed = 12 + month_counter - lag(month_counter, default = NA)) %>%
  filter(!is.na(months_passed))

# Visualization With A Stacked Histogramm
alpha_factor = 0.25
palette_19_21 <- c("#d9749a", "#fcbc67", "#825f6f")
months_passed = ggplot(data = visualize_workset_1, aes(x = months_passed, fill = as.factor(syear), color = as.factor(syear))) +
  geom_histogram(alpha = alpha_factor, binwidth = 1, boundary = 0.5, position = "stack") +
  labs(x = "Months Passed", y = "Interviews") +
  scale_x_continuous(breaks = 3:23) +
  scale_fill_manual(values = palette_19_21) +
  scale_color_manual(values = palette_19_21) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_text(vjust = -1, size = 14),
    axis.title.y = element_text(vjust = 1, size = 14),
    axis.text = element_text(size = 14),
    legend.box.background = element_rect(color = "black", linetype = "solid", size = 0.2),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  labs(title = "Interview Intervalls", size = 18) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("./images/intervalls_between_interviews.jpeg", plot = months_passed, width = 10, height = 6)

# Numeric Examination
# Counting Absolute Months Since Last Interview
xtabs(~ syear + month_counter, visualize_workset_1)

# Fractions Of Interviews Where The Year Of Interview Equals The Survey Year Plus One
visualize_workset_1 %>%
  mutate(piyear_greater_than_syear = piyear - syear) %>%
  group_by(syear) %>%
  summarize(round(mean(piyear_greater_than_syear) * 100, digits = 2))

# Calculation Of Months Passed Since The Start Of The Survey Year And The Interview
ppathl  <- read_dta("./data/raw_data/ppathl.dta", col_select = c("pid", "syear", "piyear"))
pgen   <- read_dta("./data/raw_data/pgen.dta", col_select = c("pid", "syear", "pgmonth"))
visualize_workset_2 <- ppathl %>%
  inner_join(pgen) %>%
  filter(syear >= 2018) %>%
  mutate(month_counter = case_when(
    piyear == syear ~ pgmonth,
    piyear == syear + 1 ~ pgmonth + 12,
    piyear == syear + 2 ~ pgmonth + 24,
    TRUE ~ NA_integer_
  ))

# Visualization with Density Plots
adjustment_factor = 1.75
alpha_factor = 0.20
palette_18_21 <- c("#8f41be", "#d9749a", "#fcbc67", "#825f6f")
months_passed_since_start_of_survey_year <- ggplot(data = visualize_workset_2, aes(x = month_counter, fill = as.factor(syear), color = as.factor(syear))) +
  geom_density(alpha = alpha_factor, adjust = adjustment_factor) +
  labs(x = "Months Passed Since Start of Survey Year", y = "Density") +
  scale_x_continuous(breaks = 1:15) +
  scale_fill_manual(values = palette_18_21) +
  scale_color_manual(values = palette_18_21) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_text(vjust = -1, size = 14),  # TOGGLE 13
    axis.title.y = element_text(vjust = 1, size = 14),  # TOGGLE 13
    axis.text = element_text(size = 14),
    legend.box.background = element_rect(color = "black", linetype = "solid", size = 0.2),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)  # TOGGLE 16
  ) +
  labs(title = "Interview Months", size = 18) + # TOGGLE 16
  theme(plot.title = element_text(hjust = 0.5))
ggsave("./images/months_passed_since_start_of_survey_year.jpeg", plot = months_passed_since_start_of_survey_year, width = 10, height = 6)

# Numeric Examination
# counting absolute months since start of survey year
xtabs(~ syear + month_counter, visualize_workset_2)
# Fractions Of Interviews Where The Year Of Interview Equals The Survey Year Plus One
visualize_workset_2 %>%
  mutate(piyear_greater_than_syear = piyear - syear) %>%
  group_by(syear) %>%
  summarize(round(mean(piyear_greater_than_syear) * 100, digits = 2))
