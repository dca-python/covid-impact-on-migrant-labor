# Loading Packages
pacman::p_load(dplyr, haven, ggplot2, survey, tidyr, labelled, srvyr, readxl, stargazer, gridExtra, openxlsx, clipr)
source("./scripts/utils.R")

print("Loading SOEP data…")
# Loading SOEP Data
ppathl <- read_dta("./data/raw_data/ppathl.dta", col_select = c("pid", "hid", "syear", "piyear", "migback", "arefback", "corigin", "sex", "gebjahr", "gebmonat", "immiyear", "phrf"))  %>% remove_labels()
pgen   <- read_dta("./data/raw_data/pgen.dta", col_select = c("pid", "syear", "pgstib", "pgisced11", "pgemplst", "pgbetr", "pgerwzeit", "pgkldb2010", "pgmonth", "pgnace2")) %>% remove_labels()
hgen   <- read_dta("./data/raw_data/hgen.dta", col_select = c("hid", "syear", "hgtyp1hh", "hgtyp2hh", "hgnuts1")) %>% remove_labels()
pl_18  <- read_dta("./data/raw_data/pl.dta", skip = 652141, n_max = 30306, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()
pl_19  <- read_dta("./data/raw_data/pl.dta", skip = 682447, n_max = 29905, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()
pl_20  <- read_dta("./data/raw_data/pl.dta", skip = 712352, n_max = 30470, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()
pl_21  <- read_dta("./data/raw_data/pl.dta", skip = 742822, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()

print("Joining SOEP data…")
# Joining Characteristics Across Sets
inner_join_18 <- ppathl %>% inner_join(pl_18, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))
inner_join_19 <- ppathl %>% inner_join(pl_19, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))
inner_join_20 <- ppathl %>% inner_join(pl_20, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))
inner_join_21 <- ppathl %>% inner_join(pl_21, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))

# Individuals Employed In A Given Year
empl_in_2018 <- pgen %>% inner_join(ppathl %>% select(pid, syear, piyear)) %>% filter(syear == 2018) %>% filter(pgemplst != 5)
empl_in_2019 <- pgen %>% inner_join(ppathl %>% select(pid, syear, piyear)) %>% filter(syear == 2019) %>% filter(pgemplst != 5)
empl_in_2020 <- pgen %>% inner_join(ppathl %>% select(pid, syear, piyear)) %>% filter((syear == 2020 & piyear == 2020 & pgmonth %in% c(3:12)) | (syear == 2020 & piyear == 2021)) %>% filter(pgemplst != 5)

print("Loading external data…")
# Getting the kldb2010 numbers that refer to occupations of the critical infrastructure according to Burstedde
occupations_critical_relevance <- read_excel("./data/raw_data/critical_infrastructure_jobs.xlsx") %>% as_tibble()

# Getting the teleworkability of occupations (kldb2010 two digit numbers) according to Alexandra
teleworkability <- read_excel("./data/raw_data/homeoffice.xlsx") %>% as_tibble()
colnames(teleworkability) <- c("kldb2010_two_digit", "homeoffice_access")
teleworkability$kldb2010_two_digit <- factor(teleworkability$kldb2010_two_digit)

# Getting the main_task_of_occupation of occupations (kldb2010 two digit numbers) according to Dengler
occupation_main_task <- read_dta("./data/raw_data/main_task_of_occupation.dta") %>% select(kldb2010_2, haupttask)
occupation_main_task$haupttask <- factor(occupation_main_task$haupttask)
occupation_main_task$kldb2010_2 <- factor(occupation_main_task$kldb2010_2)
colnames(occupation_main_task) <- c("kldb2010_two_digit", "occupation_main_task")
occupation_main_task <- occupation_main_task %>% as_tibble() %>% filter(!duplicated(kldb2010_two_digit))

# PARAMETERS
year_pair <- c("19_21")
inner_join_fy = inner_join_19
empl_in_fy = empl_in_2019
inner_join_sy = inner_join_21

# 231027_source_code_start_v1
print("Defining processing functions…")
# sys_mutation
sys_mutation <- function(inner_join_sy, empl_in_fy) {
  result <- inner_join_sy %>%
    filter(pid %in% empl_in_fy$pid) %>% # those employed in first year observated in second year
    # LABOR MARKET OUTCOME
    mutate(not_employed = case_when(
      pgemplst == 5 ~ 1,
      pgemplst < 0 ~ NA_integer_,
      TRUE ~ 0
    )) %>%
    mutate(reg_unempl = case_when(
      pgstib == 12 ~ 1,
      pgstib <  0  ~ NA_integer_,
      TRUE ~ 0
    )) %>%
    mutate(in_stw = case_when(
      pgemplst == 7 ~ 1,
      pgemplst < 0 ~ NA_integer_,
      TRUE ~ 0
    )) %>%
    # LENGTH OF STAY
    mutate(immiyear = case_when(immiyear < 0 ~ NA, TRUE ~ immiyear)) %>%
    mutate(length_of_stay_years = case_when(
      immiyear > 0 ~ syear - immiyear,
      TRUE ~ NA_integer_
    )) %>%
    mutate(native_migback_EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee = case_when(
      # native
      migback == 1 ~ 1,
      # indirect migration background
      migback == 3 ~ 2,
      # migration experience from EU
      corigin %in% c(40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826) ~ 3,
      # migration experience from Non-EU but no refugee
      arefback != 2 & corigin > 0 & !corigin %in% c(276, 40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826) ~ 4,
      # migration experience from Non-EU and refugee
      arefback == 2 & corigin > 0 & !corigin %in% c(276, 40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826) ~ 5,
    )) %>%
    mutate(native_migback_EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee =  relevel(as.factor(native_migback_EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee), ref = "1")) %>%
    mutate(EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee = case_when(
      # migration experience from EU
      corigin %in% c(40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826) ~ 1,
      # migration experience from Non-EU but no refugee
      arefback != 2 & corigin > 0 & !corigin %in% c(276, 40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826) ~ 2,
      # migration experience from Non-EU and refugee
      arefback == 2 & corigin > 0 & !corigin %in% c(276, 40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826) ~ 3,
    )) %>%
    mutate(EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee =  relevel(as.factor(EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee), ref = "1")) %>%
    # CONTROL VARIABLES
    mutate(woman = case_when(
      sex == 1 ~ 0, # man
      sex == 2 ~ 1, # woman
      TRUE ~ NA_integer_
    )) %>%
    mutate(child_u16_in_hh = case_when(
      hgtyp2hh %in% c(31, 32, 35, 36, 41, 42, 43, 61, 62, 73, 82) ~ 1,
      hgtyp2hh %in% c(11, 12, 13, 21, 33, 34, 51, 52, 53, 71, 72, 81) ~ 0,
      TRUE ~ NA_integer_
    )) %>%
    mutate(woman_x_child = woman * child_u16_in_hh) %>%
    mutate(highest_education = case_when(
      pgisced11 %in% c(1, 2) ~ 1, # “Low-Level Education”: Primary edu., lower secondary edu.
      pgisced11 %in% c(3, 4, 5) ~ 2, # “Mid-Level Education”: Upper secondary edu., Post-secondary, non-tertiary edu, short-cycle tertiary education
      pgisced11 %in% c(6, 7, 8) ~ 3,# “High-Level Education” bachelors, masters, doctoral
      TRUE ~NA_integer_
    )) %>%
    mutate(highest_education =  relevel(as.factor(highest_education), ref = "1")) %>%
    mutate(january = case_when(pgmonth == 1 ~ 1, pgmonth %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(february = case_when(pgmonth == 2 ~ 1, pgmonth %in% c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(march = case_when(pgmonth == 3 ~ 1, pgmonth %in% c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(april = case_when(pgmonth == 4 ~ 1, pgmonth %in% c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(may = case_when(pgmonth == 5 ~ 1, pgmonth %in% c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(june = case_when(pgmonth == 6 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(july = case_when(pgmonth == 7 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(august = case_when(pgmonth == 8 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(september = case_when(pgmonth == 9 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(october = case_when(pgmonth == 10 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(november = case_when(pgmonth == 11 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(december = case_when(pgmonth == 12 ~ 1, pgmonth %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 0, TRUE ~NA_integer_)) %>%
    mutate(age = case_when(
      pgmonth <  gebmonat ~ syear - gebjahr - 1, # Assumption: If individuals have the interview and their birthday in the same month, we assume the birthday to have happened later than the interview
      pgmonth >= gebmonat ~ syear - gebjahr,
      TRUE ~ NA_integer_
    )) %>% # Quartiles: # 18.00   36.00     47.00    55.00     91.00
    filter(age > 17 & age < 65) %>% # in the second year
    mutate(age_q2 = case_when((age > 36 & age <= 47) ~ 1, TRUE ~ 0)) %>%
    mutate(age_q3 = case_when((age > 47 & age <= 55) ~ 1, TRUE ~ 0)) %>%
    mutate(age_q4 = case_when(age > 55 ~ 1, TRUE ~ 0)) %>%
    mutate(feb_20_fedstate_unempl_rate = case_when(
      hgnuts1 == 1 ~ 3.5, hgnuts1 == 2 ~ 3.2, hgnuts1 == 3 ~ 8.0, hgnuts1 == 4 ~ 5.9, hgnuts1 == 5 ~ 10.2,
      hgnuts1 == 6 ~ 6.4, hgnuts1 == 7 ~ 4.6, hgnuts1 == 8 ~ 7.6, hgnuts1 == 9 ~ 5.3, hgnuts1 == 10 ~ 6.7,
      hgnuts1 == 11 ~ 4.7, hgnuts1 == 12 ~ 6.5, hgnuts1 == 13 ~ 5.6, hgnuts1 == 14 ~ 7.4, hgnuts1 == 15 ~ 5.4,
      hgnuts1 == 16 ~ 5.6, TRUE ~ NA_integer_
    )) %>%  # Quartiles:  3.200   3.500   5.400    6.700  10.200 # not sure if that makes sense
    mutate(pre_cov_unempl_q2 = case_when((feb_20_fedstate_unempl_rate > 3.5 & feb_20_fedstate_unempl_rate <= 5.4) ~ 1, TRUE ~ 0)) %>%
    mutate(pre_cov_unempl_q3 = case_when((feb_20_fedstate_unempl_rate > 5.4 & feb_20_fedstate_unempl_rate <= 6.7) ~ 1, TRUE ~ 0)) %>%
    mutate(pre_cov_unempl_q4 = case_when((feb_20_fedstate_unempl_rate > 6.7) ~ 1, TRUE ~ 0)) %>%
    filter(!is.na(phrf)) %>%
    select(-pgstib, -plb0041_v1, -plb0041_v2) %>%
  return(result)
}

# fys_mutation
fys_mutation = function(inner_join_fy) {
  result <- inner_join_fy %>%
    # "Individuals who were […] self-employed […] were filtered out."
    mutate(self_employed = case_when(
      pgstib %in% c(410:433) ~ 1, # pgstib %/% 100 == 4 ~ 1
      pgstib > 0 & !pgstib %in% c(410:433) ~ 0,
      TRUE ~ NA_integer_
    )) %>% select(-pgstib) %>%
    filter(self_employed == 0) %>%
    mutate(fixed_term_contract =  case_when(
      plb0037_h == 2 ~ 1,
      plb0037_h %in% c(1, 3, 4) ~ 0,
      TRUE ~ NA_integer_
    )) %>%
    mutate(permanent_work_contract =  case_when(
      plb0037_h == 1 ~ 1,
      plb0037_h %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_integer_
    )) %>%
    mutate(empl_by_empl_agency = case_when(
      (syear < 2021 & plb0041_v1 == 1) | (syear == 2021 & plb0041_v2 == 1) ~ 1,
      (syear < 2021 & plb0041_v1 == 2) | (syear == 2021 & plb0041_v2 == 2) ~ 0,
      TRUE ~ NA_integer_ # Answer [3] "I don't know" included here; but it does not matter anyway
    )) %>% select(-plb0041_v1, -plb0041_v2) %>%
    mutate(empl_status_pre_covid = case_when(
      pgemplst %in% c(1, 2) ~ 1, # Voll- oder Teilzeitbeschäftigung
      pgemplst %in% c(3) ~ 2, # Ausbildung, Lehre
      pgemplst %in% c(4) ~ 3, # Geringfügige Beschäftigung
      TRUE ~NA_integer_ # Praktikum, Freiwilliger Wehrdienst, FSJ/FOeJ/BFD, Werkstatt fuer Behinderte, Nicht erwerbstaetig, Wehrpflicht/Zivildienst, Kurzarbeit, Missing Values
    )) %>% select(-pgemplst) %>%
    mutate(empl_status_pre_covid =  relevel(as.factor(empl_status_pre_covid), ref = "1")) %>% # could use pgbetr, too
    mutate(company_size = case_when(
      pgbetr %in% c(1:2)  ~ 1, # 1-10
      pgbetr %in% c(3:6)  ~ 2, # 11-99
      pgbetr %in% c(7:10) ~ 3, # 100+
      TRUE ~NA_integer_ # [11] Self-Employed Without Coworkers coded as NA
    )) %>%
    mutate(large_company = case_when(
      pgbetr %in% c(1:6) ~ 1, # 1-99
      pgbetr %in% c(7:10) ~ 2, # 100+
      TRUE ~NA_integer_ # [11] Self-Employed Without Coworkers coded as NA
    )) %>%
    mutate(company_size =  relevel(as.factor(company_size), ref = "1")) %>%
    mutate(years_with_firm = case_when(
      pgerwzeit <= 1 ~ 1, # 0-1 years
      (pgerwzeit > 1 & pgerwzeit <= 3)  ~ 2, # 1-3 years
      pgerwzeit > 3 ~ 3, # 3+ years
      TRUE ~ NA_integer_
    )) %>% select(-pgerwzeit) %>%
    mutate(years_with_firm =  relevel(as.factor(years_with_firm), ref = "1")) %>%
    mutate(critical_relevance = case_when(
      pgkldb2010 %in% occupations_critical_relevance$A ~ 1, # according to Burstedde
      TRUE ~ 0
    )) %>%
    mutate(kldb2010_two_digit = pgkldb2010 %/% 1000) %>%
    mutate(kldb2010_two_digit = factor(kldb2010_two_digit)) %>%
    left_join(teleworkability) %>%
    mutate(teleworkability = case_when( # according to Alexandra
      homeoffice_access > 0 & homeoffice_access <= 30 ~ 1, # 0-30 %
      homeoffice_access > 30 & homeoffice_access <= 70 ~ 2, # 30-70 %
      homeoffice_access > 70 & homeoffice_access <= 100 ~ 3, # 70-100 %
      TRUE ~ NA_integer_
    )) %>%
    mutate(teleworkability =  relevel(as.factor(teleworkability), ref = "1")) %>%
    mutate(requirement_level = case_when(
      pgkldb2010 %% 10 == 1 ~ 1, # Helfer- und Anlerntätigkeiten
      pgkldb2010 %% 10 == 2 ~ 2, # fachlich ausgerichtete Tätigkeiten
      ((pgkldb2010 %% 10 == 3) | (pgkldb2010 %% 10 == 4))  ~ 3, # komplexe Spezialistentätigkeiten oder hoch komplexe Tätigkeiten
      TRUE ~ NA_integer_
    )) %>%
    mutate(requirement_level =  relevel(as.factor(requirement_level), ref = "1")) %>%
    left_join(occupation_main_task) %>%
    mutate(occupation_main_task =  relevel(as.factor(occupation_main_task), ref = "1")) %>% # according to Dengler
    mutate(sna_a10 = case_when( # Base category: Agriculture, forestry and fishing
      pgnace2 %in% c(1:3) ~ 1,  # Mining and quarrying and other industry
      pgnace2 %in% c(5:39) ~ 2,  # Manufacturing
      pgnace2 %in% c(41:43) ~ 3,  # Construction
      pgnace2 %in% c(45:56) ~ 4,  # Wholesale and retail trade, transportation and storage, accommodation and food service activities
      pgnace2 %in% c(58:63) ~ 5,  # Information and communication
      pgnace2 %in% c(64:66) ~ 6,  # Financial and insurance activities
      pgnace2 %in% c(68) ~ 7,  # Real estate activities*
      pgnace2 %in% c(69:82) ~ 8,  # Professional, scientific, technical, administration and support service activities
      pgnace2 %in% c(84:88) ~ 9,  # Public administration, defence, education, human health and social work activities
      pgnace2 %in% c(90:99) ~ 10,  # Other services
      TRUE ~ NA_integer_
    )) %>%
    mutate(sna_a10_02 =  case_when(sna_a10 == 2  ~ 1, sna_a10 %in% c(1, 3, 4, 5, 6, 7, 8, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_03 =  case_when(sna_a10 == 3  ~ 1, sna_a10 %in% c(1, 2, 4, 5, 6, 7, 8, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_04 =  case_when(sna_a10 == 4  ~ 1, sna_a10 %in% c(1, 2, 3, 5, 6, 7, 8, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_05 =  case_when(sna_a10 == 5  ~ 1, sna_a10 %in% c(1, 2, 3, 4, 6, 7, 8, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_06 =  case_when(sna_a10 == 6  ~ 1, sna_a10 %in% c(1, 2, 3, 4, 5, 7, 8, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_07 =  case_when(sna_a10 == 7  ~ 1, sna_a10 %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_08 =  case_when(sna_a10 == 8  ~ 1, sna_a10 %in% c(1, 2, 3, 4, 5, 6, 7, 9, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_09 =  case_when(sna_a10 == 9  ~ 1, sna_a10 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 10) ~ 0, TRUE ~ NA_integer_)) %>%
    mutate(sna_a10_10 =  case_when(sna_a10 == 10 ~ 1, sna_a10 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9)  ~ 0, TRUE ~ NA_integer_)) %>%
    # this is necessary because I get these ".x" and ".y" problems otherwise. The more flexible appraoch would be to just
    select(pid,
    empl_by_empl_agency,
    permanent_work_contract,
    fixed_term_contract,
    empl_status_pre_covid,
    large_company,
    company_size,
    years_with_firm,
    critical_relevance,
    teleworkability,
    requirement_level,
    occupation_main_task,
    sna_a10_02, sna_a10_03, sna_a10_04, sna_a10_05, sna_a10_06, sna_a10_07, sna_a10_08, sna_a10_09, sna_a10_10)
  return(result)
}

# For 2020: filtering away the months in which the pandemic has not yet hit.
jan_feb_20_filter <- function(dataset) {
  result <- dataset %>%
    filter((syear == 2020 & piyear == 2020 & pgmonth %in% c(3:12)) | (syear == 2020 & piyear == 2021))
  return(result)
}

# MODEL SPECIFICATIONS
general_migrant_grouping <- c("native_migback_EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee + ")
direct_migrant_grouping <- c("EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee + length_of_stay_years + ")
control_variables <- c(
  "woman + child_u16_in_hh + woman_x_child + highest_education + empl_status_pre_covid + empl_by_empl_agency +
  permanent_work_contract + large_company + years_with_firm + critical_relevance + teleworkability + requirement_level + occupation_main_task +
  age_q2 + age_q3 + age_q4 +
  sna_a10_02 + sna_a10_03 + sna_a10_04 + sna_a10_05 + sna_a10_06 + sna_a10_07 + sna_a10_08 + sna_a10_09 + sna_a10_10 +
  pre_cov_unempl_q2 + pre_cov_unempl_q3 + pre_cov_unempl_q4 +"
)
general_migrant_variable <- c("native_migback_EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee")
direct_migrant_variable <- c("EUmigexp_NonEUmigexpNonRefugee_NonEUmigexpRefugee")
not_employed <- c("not_employed")
reg_unempl <- c("reg_unempl")
in_stw <- c("in_stw")


print("Assembling workset…")
# GENERALIZEABLE OPERATIONS
sys <- sys_mutation(inner_join_sy, empl_in_fy)
fys <- fys_mutation(inner_join_fy)

# IF 2020 is SY: SPECIFIC OPERATION
if (year_pair %in% c("19_20")) {
  sys <- sys %>% jan_feb_20_filter()
}

# MERGE THE TWO YEARS
workset <- sys %>% left_join(fys, by = "pid")
if (year_pair %in% c("19_20", "18_19")) {
  workset <- workset %>% select(-in_stw)
}

# CHANGE CATEGORICAL VARIABLES TO FACTORS
categorical_columns <- c(colnames(workset %>% select(-c(hid, pid, syear, immiyear, gebjahr, phrf, pgerwzeit, length_of_stay_years))))
workset[categorical_columns] <- lapply(workset[categorical_columns], as.factor)

# ADJUST MONTH DUMMIES WITH REGARD TO MISSING OBSERVATIONS
pgmonth_counts <- xtabs(~ pgmonth, workset)
months_with_observations <- as.numeric(rownames(as.matrix(pgmonth_counts)))[-1]
months_of_a_year <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
months <- paste(months_of_a_year[months_with_observations], collapse = " + ")

not_employed_general_migrant_grouping <- svyglm(
  formula = as.formula(paste0(not_employed, " ~ ", general_migrant_grouping, control_variables, months)),
  family = binomial(link = "logit"),
  design = workset %>% as_survey_design(weights = phrf)
)
not_employed_direct_migrant_grouping <- svyglm(
  formula = as.formula(paste0(not_employed, " ~ ", direct_migrant_grouping, control_variables, months)),
  family = binomial(link = "logit"),
  design = workset %>% filter(migback == 2) %>% as_survey_design(weights = phrf)
)
reg_unempl_general_migrant_grouping <- svyglm(
  formula = as.formula(paste0(reg_unempl, " ~ ", general_migrant_grouping, control_variables, months)),
  family = binomial(link = "logit"),
  design = workset %>% as_survey_design(weights = phrf)
)
reg_unempl_direct_migrant_grouping <- svyglm(
  formula = as.formula(paste0(reg_unempl, " ~ ", direct_migrant_grouping, control_variables, months)),
  family = binomial(link = "logit"),
  design = workset %>% filter(migback == 2) %>% as_survey_design(weights = phrf)
)
if (year_pair %in% c("19_20_cov", "19_21", "20_21", "21_22")) {
  in_stw_general_migrant_grouping <- svyglm(
    formula = as.formula(paste0(in_stw, " ~ ", general_migrant_grouping, control_variables, months)),
    family = binomial(link = "logit"),
    design = workset %>% as_survey_design(weights = phrf)
  )
  in_stw_direct_migrant_grouping <- svyglm(
    formula = as.formula(paste0(in_stw, " ~ ", direct_migrant_grouping, control_variables, months)),
    family = binomial(link = "logit"),
    design = workset %>% filter(migback == 2) %>% as_survey_design(weights = phrf)
  )
}

create_regression_table_file(
  model_list = list(
    reg_unempl_general_migrant_grouping,
    reg_unempl_direct_migrant_grouping
  ),
  table_title = "2019 to 2021, Employed to Unemployed",
  output_file_name = "employed_to_unemployed_2019_to_2021"
)
create_regression_table_file(
  model_list = list(
    not_employed_general_migrant_grouping,
    not_employed_direct_migrant_grouping
  ),
  table_title = "2019 to 2021, Employed to Not-Employed",
  output_file_name = "employed_to_not_employed_2019_to_2021"
)
if (year_pair %in% c("19_20_cov", "19_21", "20_21", "21_22")) {  # TODO: Check if correct and if yes, generalize to other files.
  create_regression_table_file(
    model_list = list(
      in_stw_general_migrant_grouping,
      in_stw_direct_migrant_grouping
    ),
    table_title = "2019 to 2021, Employed to Short-Time Work",
    output_file_name = "employed_to_short_time_work_2019_to_2021"
  )
}
