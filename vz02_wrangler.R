#-------------------------------------------------------------------------------
# Description: Wrangling the Main Dataset for Regressions
#-------------------------------------------------------------------------------
#
# Author: H. Sant'Anna
# Date: 2023
# Institution: University of Georgia
# Version: 1.0
#
#-------------------------------------------------------------------------------

# Loading required libraries
#-------------------------------------------------------------------------------

library(tidyverse)
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader

# Loading the dataset
# Truncating occupation and activity variable for 2 and 3 digits, respectively
dt <- read_fst("./data/parsed_data_vz.fst") %>%
  mutate(
    activity = substr(activity, 1, 2),
    occupation = substr(occupation, 1, 3)
  )


# Filtering the data to the desired geographical locations
# Roraima: 14
# Amazonas: 13
# Acre: 12
# Rondônia: 11
# Pará: 15
# Amapá: 16
# Tocantins: 17

# We may use some of these for the regs and for the SCM analysis
dt <- dt %>% 
  filter(
    substr(municipality, 1, 2) %in% c("11", "12", "13", "14", "15", "16", "17")
  ) %>% 
  # Getting rid of the missing values
  filter(nominal_wage > 0) %>% 

# We only want Brazilians
  filter(nationality == 10) %>% 

# Only private workers
  filter(substr(nature, 1, 1) != "1")


# Here I remove multi-job entries, keeping only the main employment of each individual
#-------------------------------------------------------------------------------

# Getting the most conservative employment spell for each individual
# Jobs with longest tenure and largest wage only
ind_year_spell <- function(data, yr) {
  data <- data %>%
    filter(year == yr) %>%
    group_by(pis) %>%
    filter(hire_date == min(hire_date)) %>%
    filter(nominal_wage == max(nominal_wage)) %>%
    ungroup()

  return(data)
}

# Applying the function
dt <- map_dfr(
  unique(dt$year),
  ~ ind_year_spell(data = dt, yr = .x)
)

# Individuals that worked in all pre-treatment years.
#-------------------------------------------------------------------------------

max_yr <- length(2008:2014)

individuals <- dt %>%
  filter(year %in% 2008:2014) %>%
  group_by(pis) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years == max_yr) %>%
  pull(pis)


# join back with original data
dt <- dt %>%
  filter(pis %in% individuals)

# removing remaining outliers (individuals that for some 
#    reason keep appearing multiple times)
outliers <- dt %>%
  count(pis) %>%
  filter(n > 11) %>%
  pull(pis)

dt <- dt %>%
  filter(!pis %in% outliers)

# Making the outcome and state variable 
# Not worried with nominal x real wage here since we control for time period.
#-------------------------------------------------------------------------------
dt <- dt %>%
  mutate(
    log_wage = log(nominal_wage),
  ) %>%
  mutate(state = case_when(
    substr(municipality, 1, 2) == "11" ~ "Rondônia",
    substr(municipality, 1, 2) == "12" ~ "Acre",
    substr(municipality, 1, 2) == "13" ~ "Amazonas",
    substr(municipality, 1, 2) == "14" ~ "Roraima",
    substr(municipality, 1, 2) == "15" ~ "Pará",
    substr(municipality, 1, 2) == "16" ~ "Amapá",
    substr(municipality, 1, 2) == "17" ~ "Tocantins"
  ))



# Creating the continuous treatment variable foreign ratio
# As we show in the paper, foriengers in Roraima are a good proxy for
# the Venezuelan migration shock.
#-------------------------------------------------------------------------------
contreat <- read_fst("./data/parsed_data_vz.fst") %>%
  mutate(foreign = ifelse(nationality == 10, 0, 1)) %>%
  group_by(year, municipality) %>%
  count(foreign) %>%
  mutate(vz_ratio = n / sum(n)) %>%
  filter(foreign == 1) %>%
  select(year, municipality, vz_ratio)


# Joining the data
dt <- dt %>%
  left_join(contreat, by = c("year", "municipality")) %>%
  mutate(
    vz_ratio = ifelse(is.na(vz_ratio), 0, vz_ratio),
    vz_ratio = vz_ratio * 100
  ) %>%
  mutate(
    treated = ifelse(state == "Roraima", 1, 0),
    treat = ifelse(state == "Roraima" & year >= 2014, 1, 0)
  )

write_fst(dt, "data/data_wrangled_final.fst")

# END OF SCRIPT
