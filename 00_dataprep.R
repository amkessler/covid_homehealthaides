library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(writexl)


#### BLS OCCUPATIONAL DATA ############ -------------------------------------

# Data source: BLS Occupational Employment Statistics (2018) 
# https://www.bls.gov/oes/tables.htm


#import data for all states, all occupations
rawstatesdata <- read_excel("raw_data/oesm18st/state_M2018_dl.xlsx")

#clean names
states_allrecs <- rawstatesdata %>% 
  clean_names()

#cut down to only the measures we'll use here:

# -- tot_emp: Estimated total employment rounded to the nearest 10 (excludes self-employed)
# -- jobs_1000: The number of jobs (employment) in the given occupation per 1,000 jobs in the given area 
# -- h_mean: Mean hourly wage
# -- a_mean: Mean annual wage 

states_allrecs <- states_allrecs %>% 
  select(
    area,
    st,
    state,
    occ_code,
    occ_title,
    occ_group,
    tot_emp,
    jobs_1000,
    h_mean,
    a_mean
  )


glimpse(states_allrecs)

#format columns
states_allrecs <- states_allrecs %>% 
  mutate(
    tot_emp = as.numeric(tot_emp),
    jobs_1000 = as.numeric(jobs_1000),
    h_mean = as.numeric(h_mean),
    a_mean = as.numeric(a_mean)
  )


## we'll create a new column that moves the decimal over one place from jobs_1000
## to be jobs per 100, a more readable "share of workforce
states_allrecs <- states_allrecs %>% 
  mutate(
    share_of_workforce = jobs_1000 / 10
  ) 


#save for use in analysis steps
saveRDS(states_allrecs, "processed_data/states_allrecs.rds")




##### Now the same thing but for MSA-level records #######

rawmsadata <- read_excel("raw_data/oesm18ma/MSA_M2018_dl.xlsx")

#clean names
msa_allrecs <- rawmsadata %>% 
  clean_names()

names(msa_allrecs)

#cut down to only the measures we'll use:
msa_allrecs <- msa_allrecs %>% 
  select(
    prim_state,
    area,
    area_name,
    occ_code,
    occ_title,
    occ_group,
    tot_emp,
    jobs_1000,
    h_mean,
    a_mean
  )

#format columns
msa_allrecs <- msa_allrecs %>% 
  mutate(
    tot_emp = as.numeric(tot_emp),
    jobs_1000 = as.numeric(jobs_1000),
    h_mean = as.numeric(h_mean),
    a_mean = as.numeric(a_mean)
  )


## we'll create a new column that moves the decimal over one place from jobs_1000
## to be jobs per 100, a more readable "share of workforce
msa_allrecs <- msa_allrecs %>% 
  mutate(
    share_of_workforce = jobs_1000 / 10
  ) 


glimpse(msa_allrecs)

#save for use in analysis steps
saveRDS(msa_allrecs, "processed_data/msa_allrecs.rds")



#### Now the the national-level data #####

rawnationaldata <- read_excel("raw_data/oesm18nat/national_M2018_dl.xlsx")

#clean names
national_allrecs <- rawnationaldata %>% 
  clean_names()

names(national_allrecs)

#cut down to only the measures we'll use:
national_allrecs <- national_allrecs %>% 
  mutate(
    data_scope = "national"
  ) %>% 
  select(
    data_scope,
    occ_code,
    occ_title,
    occ_group,
    tot_emp,
    h_mean,
    a_mean
  ) 

#format columns
national_allrecs <- national_allrecs %>% 
  mutate(
    tot_emp = as.numeric(tot_emp),
    h_mean = as.numeric(h_mean),
    a_mean = as.numeric(a_mean)
  )

glimpse(national_allrecs)

#save for use in analysis steps
saveRDS(national_allrecs, "processed_data/national_allrecs.rds")





##### CENSUS POPULATIONS ###### --------------------------------------------

#We'll use the tidycensus package here to pull down census data

# 2018 ACS 1 year estimates ####
census_statepops2018 <- tidycensus::get_acs(geography = "state",
                                            variables = c(totalpop_2018 = "B01003_001"),
                                            survey = "acs1")

#clean names, remove PR and state names to lowercase
census_statepops2018 <- census_statepops2018 %>% 
  clean_names() %>% 
  mutate(name = str_trim(str_to_lower(name))) %>% 
  filter(name != "puerto rico") %>% 
  select(geoid, 
         name, 
         censuspop2018 = estimate)

census_statepops2018

#add abbreviations
statelist <- tidycensus::fips_codes %>% 
  select(state, state_name) %>% 
  distinct() %>% 
  mutate(state_name = str_to_lower(state_name)) 

statelist

statepops2018 <- inner_join(statelist, census_statepops2018, by = c("state_name" = "name")) %>% 
                    select(geoid, everything())

#save as RDS for later use
saveRDS(statepops2018, "processed_data/statepops2018.rds")



##### CORONAVIRUS CASE COUNTS ####### -----------------------------------------------

#import case counts from downloaded excel file and pare down columns
casecounts <- read_excel("raw_data/COVID_COUNTS_APR20.xlsx", 
                                  sheet = "cases")

casecounts <- casecounts %>% 
  clean_names() %>% 
  mutate(region_2 = str_trim(str_to_lower(region_2)))

names(casecounts)

term_cases <- casecounts %>% 
  select(
    state_name = region_2,
    term_case_counts = latest
  )

#save as RDS for later use
saveRDS(term_cases, "processed_data/term_cases.rds")
write_xlsx(term_cases, "processed_data/term_cases.xlsx")




##### JOINING ###### ----------------
#join population and case counts together

joined <- inner_join(term_cases, statepops2018, by = "state_name")

#reorder columns
joined <- joined %>% 
  select(
    geoid,
    state,
    state_name,
    casecount = term_case_counts,
    censuspop2018
  )

#calculate cases per 100,000 people
joined <- joined %>% 
  mutate(
    cases_per_100k = round_half_up(casecount / censuspop2018 * 100000)
  ) 


#save joined results for next step
saveRDS(joined, "processed_data/joined_state_pops_cases.rds")

