# Loading Packages
# pacman::p_load(dplyr, haven)
pacman::p_load(dplyr, haven, ggplot2, survey, tidyr, labelled, srvyr, readxl, stargazer, gridExtra, openxlsx, clipr)

# Loading SOEP Data
ppathl <- read_dta("./data/raw_data/ppathl.dta", col_select = c("pid", "hid", "syear", "piyear", "migback", "arefback", "corigin", "sex", "gebjahr", "gebmonat", "immiyear", "phrf"))  %>% remove_labels()
pgen   <- read_dta("./data/raw_data/pgen.dta", col_select = c("pid", "syear", "pgstib", "pgisced11", "pgemplst", "pgbetr", "pgerwzeit", "pgkldb2010", "pgmonth", "pgnace2")) %>% remove_labels()
hgen   <- read_dta("./data/raw_data/hgen.dta", col_select = c("hid", "syear", "hgtyp1hh", "hgtyp2hh", "hgnuts1")) %>% remove_labels()
pl_18  <- read_dta("./data/raw_data/pl.dta", skip = 652141, n_max = 30306, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()
pl_19  <- read_dta("./data/raw_data/pl.dta", skip = 682447, n_max = 29905, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()
pl_20  <- read_dta("./data/raw_data/pl.dta", skip = 712352, n_max = 30470, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()
pl_21  <- read_dta("./data/raw_data/pl.dta", skip = 742822, col_select = c("pid", "syear", "plb0037_h", "plb0049_v6", "plb0041_v1", "plb0041_v2"))  %>% remove_labels()

# Joining Characteristics Across Sets
inner_join_18 <- ppathl %>% inner_join(pl_18, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))
inner_join_19 <- ppathl %>% inner_join(pl_19, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))
inner_join_20 <- ppathl %>% inner_join(pl_20, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))
inner_join_21 <- ppathl %>% inner_join(pl_21, by = c("pid", "syear")) %>% inner_join(pgen, by = c("pid", "syear")) %>% inner_join(hgen, by = c("hid", "syear"))

# Individuals Employed In A Given Year
empl_in_2018 <- pgen %>% inner_join(ppathl %>% select(pid, syear, piyear)) %>% filter(syear == 2018) %>% filter(pgemplst != 5)
empl_in_2019 <- pgen %>% inner_join(ppathl %>% select(pid, syear, piyear)) %>% filter(syear == 2019) %>% filter(pgemplst != 5)
empl_in_2020 <- pgen %>% inner_join(ppathl %>% select(pid, syear, piyear)) %>% filter((syear == 2020 & piyear == 2020 & pgmonth %in% c(3:12)) | (syear == 2020 & piyear == 2021)) %>% filter(pgemplst != 5)

# Getting the kldb2010 numbers that refer to occupations of the critical infrastructure according to Burstedde
occupations_critical_relevance <- read_excel("./data/raw_data/critical_infrastructure_jobs.xlsx")$A %>% as_tibble()

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
