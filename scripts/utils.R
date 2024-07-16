# EXPLORING DISTRIBUTIONS AND LABELS

tab = function(df, col) {df %>% group_by({{col}}) %>% summarize(n_rel = n() / nrow(df) * 100, n_abs = n()) %>% ungroup()}

tabv = function(df, col) {
  data <- df %>% group_by({{col}}) %>% summarize(n_rel = n() / nrow(df) * 100, n_abs = n()) %>% ungroup()
  plot <- ggplot(data, aes(x = factor({{col}}), y = n_rel)) + geom_col()
  print(plot)
  tab(df, {{col}})}

tabp <- function(data, vars) {
  formula <- as.formula(paste("~", paste(vars, collapse = " + ")))
  result <- round(prop.table(xtabs(formula, data = data)), digits = 4) * 100
  return(result)}

lbl_search = function(df, keyword) {View(look_for(df, keyword))}

# FUNCTION TO CHECK IF FACTOR VARIABLES HAVE LESS THAN TWO LEVELS, CAUSING PROBLEMS IN THE REGRESSIONS

check_levels <- function(df) {
  vars_with_less_than_2_levels <- character(0)

  for (col in names(df)) {
    if (is.factor(df[[col]]) && nlevels(df[[col]]) < 2) {
      vars_with_less_than_2_levels <- c(vars_with_less_than_2_levels, col)
    }
  }

  return(vars_with_less_than_2_levels)
}

# VISUALIZING REGRESSION RESULTS

stargazer2 <- function(model, odd.ratio = FALSE, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(stats::coefficients(x)))
    seOR2 <- lapply(model, function(x) exp(stats::coefficients(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer::stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
  } else {
    stargazer::stargazer(model, ...)
  }
}

KEEP_VARIABLES_LABELS <- c(
  "Indirect Migrant",
  "EU Migrant",
  "Non-EU Migrant",
  "Refugees",
  "Non-EU Migrant",
  "Refugees",
  "Length of Stay",
  "Woman",
  "Child under 16 in Household",
  "Woman x Child",
  "Mid-tier Education",
  "High-tier Education",
  "Vocational Training",
  "Marginal Employment",
  "Employed by Employment Agency",
  "Permanent Work Contract",
  "Large Company",
  "1-3 Years with Firm",
  "3+ Years with Firm",
  "Critical Relevance",
  "Mid-level Teleworkability",
  "High-level Teleworkability",
  "Mid-tier Requirement Level",
  "High-tier Requirement Level",
  "Interactive Non-Routine Tasks",
  "Cognitive Routine Tasks",
  "Manual Routine Tasks",
  "Manual Non-Routine Tasks"
)

create_regression_table_file <- function(model_list, table_title, output_file_name) {
  stargazer2(
    model = model_list,
    title = table_title,
    odd.ratio=TRUE,
    type = "text",
    align = TRUE,
    dep.var.labels.include = FALSE,
    column.labels = NULL,
    report = "vc*",
    style = "qje",
    keep.stat = c("n", "ll"),
    digits = 2,
    font.size = "normalsize",
    no.space = TRUE,
    dep.var.caption = "",
    keep = c(1:29),
    notes = "Notes: ***, **, * denote significance at the 1, 5 and 10 percent level.",
    notes.label = "",
    notes.append = FALSE,
    notes.align = "l",
    covariate.labels = KEEP_VARIABLES_LABELS,
    out = paste("./tables/", output_file_name, ".txt", sep = "")
  )
}
