
#-------------------------------------------------------------------------------
# Some nice tables for descriptive statistics
# ---
# @Author:       Hugo Sant'Anna
# @Organization: University of Georgia
# @Github:       github.com/hsantanna88
#-------------------------------------------------------------------------------

# Library
library(tidyverse)
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader
library(ggsci)
library(modelsummary)
library(kableExtra)

# Aux functions
source("scripts/vz_aux0_20231203.R")

# Name the Nationality variable in the dataset
#-------------------------------------------------------------------------------

nationality <- tibble( # These are all the nationalities in the data
  nationcode = c(
    10, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
    31, 32, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44,
    45, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 59, 60,
    61, 62, 63, 64, 65, 70, 80
  ),
  nationality = c(
    "Brazilian", "Naturalized Brazilian", "Argentinian", "Bolivian",
    "Chilean", "Paraguayan", "Uruguayan", "Venezuelan",
    "Colombian", "Peruvian", "Ecuadorian", "German", "Belgian",
    "British", "Canadian", "Spanish", "North-American",
    "French", "Swiss", "Italian", "Haitian", "Japanese", "Chinese",
    "Russian", "Portuguese", "Pakistani", "Indian", "Other Latin Americans",
    "Other Asians", "Other Europeans", "Bissau-Guinean", "Moroccan",
    "Cuban", "Syrian", "South Korean", "Bengali", "Angolan", "Congolese",
    "South African", "Ghanaian", "Senegalese",
    "North Korean", "Other Africans", "Others"
  )
)

# Statistics for the 2013 data
#------------------------------------------------------------

rais <- read_fst("data/data_wrangled_final.fst") %>%
  mutate(
    educ = case_when(
      as.numeric(education) < 7 ~ "low",
      as.numeric(education) %in% c(7, 8) ~ "hschool",
      as.numeric(education) > 8 ~ "college"
    )
  )

stat_vars <- rais %>%
  filter(year %in% 2013) %>%
  filter(nationality %in% c(10)) %>%
  filter(state %in% c("Roraima", "Acre", "Amapá")) %>%
  transmute(
    Municipality = municipality,
    Nationality = ifelse(nationality == 10, "Brazilian", "Venezuelan"),
    Unit = ifelse(state == "Roraima", "Roraima", "Control States"),
    Unit = factor(Unit, levels = c("Roraima", "Control States")),
    Race = case_when(
      as.numeric(race) == 2 ~ "White",
      as.numeric(race) == 4 ~ "Black",
      as.numeric(race) == 1 ~ "Indigenous",
      as.numeric(race) == 8 ~ "Mixed",
      TRUE ~ "Not Declared",
    ),
    Race = factor(Race, levels = c("White", "Black", "Indigenous", "Mixed", "Not Declared")),
    Sex = ifelse(male == 1, "Male", "Female"),
    Education = case_when(
      educ == "low" ~ "No High School",
      educ == "hschool" ~ "High School",
      educ == "college" ~ "College"
    ),
    Education = factor(Education, levels = c("No High School", "High School", "College")),
    Age = age,
    Tenure = tenure,
    Wage = nominal_wage,
    Industry = case_when(
      as.numeric(activity) %in% c(1:9) ~ "Extraction Industries",
      as.numeric(activity) %in% c(10:39) ~ "Manufacturing and Utilities",
      as.numeric(activity) %in% c(41:43) ~ "Construction",
      as.numeric(activity) %in% c(45:53) ~ "Commerce",
      as.numeric(activity) %in% c(55:56) ~ "Hotels and Restaurants",
      as.numeric(activity) %in% c(57:99) ~ "Other Services"
    ),
    Occupation = case_when(
      as.numeric(occupation) %in% c(200:299) ~ "Scientific and Liberal Arts",
      as.numeric(occupation) %in% c(300:399) ~ "High School Level Technicians",
      as.numeric(occupation) %in% c(520:529) ~ "Retail and Wholesale",
      as.numeric(occupation) %in% c(600:699) ~ "Rural Occupations",
      as.numeric(occupation) %in% c(700:999) ~ "Other Factory Occupations",
      as.numeric(occupation) %in% c(410:519) ~ "Other Services"
    )
  )

N <- NULL

datasummary(
  (Wage + Age + Tenure) * mean + (Sex + Race + Education + Industry + Occupation) * ((Percent(fn = function(x, y) length(x) / length(y), denom = "col"))) +
    (1 * N) ~ (Unit),
  data = stat_vars,
  output = "latex_tabular"
) %>%
  save_kable("./out/tab_summary_2013.tex")


table(rais$state, rais$year)


# Statistics for the 2018 data
#------------------------------------------------------------

rais <- read_fst("./data/parsed_data_vz.fst") 

rais <- rais %>%
  mutate(
    state = case_when(
      substr(municipality, 1, 2) == "11" ~ "Rondônia",
      substr(municipality, 1, 2) == "12" ~ "Acre",
      substr(municipality, 1, 2) == "13" ~ "Amazonas",
      substr(municipality, 1, 2) == "14" ~ "Roraima",
      substr(municipality, 1, 2) == "15" ~ "Pará",
      substr(municipality, 1, 2) == "16" ~ "Amapá",
      substr(municipality, 1, 2) == "17" ~ "Tocantins"
    )
  ) %>%
  mutate(
    education = as.numeric(education),
    educ = case_when(
      education < 7 ~ "low",
      education %in% c(7, 8) ~ "hschool",
      education > 8 ~ "college"
    ),
    race = as.numeric(race)
  )

rais <- rais %>%
  mutate(
    activity = substr(activity, 1, 2),
    occupation = substr(occupation, 1, 2)
  )

vz_acts <- rais %>%
  filter(nationality == 26) %>%
  count(activity) %>%
  mutate(n = n / sum(n)) %>%
  arrange(-n) %>%
  slice(1:5)

vz_occs <- rais %>%
  filter(nationality == 26) %>%
  count(occupation) %>%
  mutate(n = n / sum(n)) %>%
  arrange(-n) %>%
  slice(1:5)

stat_vars <- rais %>%
  filter(year %in% 2018) %>%
  filter(nationality %in% c(10, 26)) %>%
  filter(state == "Roraima") %>%
  transmute(
    Municipality = municipality,
    Nationality = ifelse(nationality == 10, "Brazilian", "Venezuelan"),
    Unit = ifelse(state == "Roraima", "Roraima", "Control States"),
    Race = case_when(
      as.numeric(race) == 2 ~ "White",
      as.numeric(race) == 4 ~ "Black",
      as.numeric(race) == 1 ~ "Indigenous",
      as.numeric(race) == 8 ~ "Mixed",
      TRUE ~ "Not Declared",
    ),
    Race = factor(Race, levels = c("White", "Black", "Indigenous", "Mixed", "Not Declared")),
    Sex = ifelse(male == 1, "Male", "Female"),
    Education = case_when(
      educ == "low" ~ "No High School",
      educ == "hschool" ~ "High School",
      educ == "college" ~ "College"
    ),
    Education = factor(Education, levels = c("No High School", "High School", "College")),
    Age = age,
    Wage = nominal_wage,
    Industry = case_when(
      activity == 47 ~ "Retail Commerce",
      activity == 56 ~ "Restaurants",
      activity == 41 ~ "Construction",
      activity == 46 ~ "Wholesale Commerce",
      activity == 81 ~ "Gardening Landscaping",
      TRUE ~ "Other Industries"
    ),
    Occupation = case_when(
      occupation == 51 ~ "General Service Employees",
      occupation == 52 ~ "Salesperson",
      occupation == 71 ~ "General Construction Workers",
      occupation == 41 ~ "Office Clerks",
      occupation == 78 ~ "Machine Operators",
      TRUE ~ "Other Occupations"
    )
  )

vz_data_act <- stat_vars  %>% 
  filter(Unit == "Roraima") %>% 
  filter(Nationality == "Venezuelan") %>% 
  group_by(Industry) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(share = n / sum(n)) %>%
  arrange(-share)

vz_data_act <- bind_rows(filter(vz_data_act, Industry != "Other Industries"), filter(vz_data_act, Industry == "Other Industries"))

position1 <- vz_data_act$Industry

vz_data_occ <- stat_vars  %>% 
  filter(Unit == "Roraima") %>% 
  filter(Nationality == "Venezuelan") %>% 
  group_by(Occupation) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(share = n / sum(n)) %>%
  arrange(-share)

vz_data_occ <- bind_rows(filter(vz_data_occ, Occupation != "Other Occupations"), filter(vz_data_occ, Occupation == "Other Occupations"))


position2 <- vz_data_occ$Occupation


# Plotting worker type proportions
p <- ggplot(vz_data_act) +
  geom_bar(
    aes(x = Industry, y = share, fill = Industry),
    stat = "identity",
    position = "dodge"
  ) +
  theme_publication(16) +
  #ylim(0,) +
  scale_fill_jama()+
  theme(legend.text = element_blank())+
  theme(legend.position = "none") + 
  scale_x_discrete(limits = position1, labels = function(x) str_wrap(x, width = 0.0001))

ggsave(
  p,
  file = "out/fig_vz_act.png",
  width = 10, height = 6, dpi = 300, bg = "white"
)


# Plotting worker type proportions
p <- ggplot(vz_data_occ) +
  geom_bar(
    aes(x = Occupation, y = share, fill = Occupation),
    stat = "identity",
    position = "dodge"
  ) +
  theme_publication(16) +
  scale_fill_aaas()+
  theme(legend.text = element_blank())+
  theme(legend.position = "none") + 
  scale_x_discrete(limits = position2, labels = function(x) str_wrap(x, width = 0.0001))

ggsave(
  p,
  file = "out/fig_vz_occ.png",
  width = 10, height = 6, dpi = 300, bg = "white"
)



# Creating Venezuela Ratio plot
#-------------------------------------------------------------------------------


# Trend for Non Venezuelans
#-------------------------------------------------------------------------------
vz_ratio_dt <- rais %>%
  mutate(
    group = ifelse(state == "Roraima", "Roraima", "Other Northern States"),
    group = ifelse(is.na(group), "Other Northern States", group)
  ) %>%
  mutate(nonbrazilian = ifelse(nationality != "10", 1, 0)) %>%
  filter(nationality != "26") %>%
  group_by(group, year, nonbrazilian) %>%
  summarise(n = n()) %>%
  group_by(group, year) %>%
  mutate(share = 100 * n / sum(n)) %>%
  filter(nonbrazilian == 1)

p <- ggplot(vz_ratio_dt, aes(x = year, y = share)) +
  geom_point(aes(color = group, shape = group)) +
  geom_line(aes(color = group, group = group)) +
  theme_minimal() +
  ylim(0, 3) +
  geom_vline(
    xintercept = 2013,
    linetype = "dashed",
    color = "#b90c2e"
  ) +
  xlab("Years") +
  ylab("Percentage of Other Foreigners in the Formal Sector") +
  theme_minimal() +
  scale_color_aaas() +
  theme_publication() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 11)) +
  theme(legend.position = "bottom")

ggsave(
  plot = p,
  paste0("out/fig_ofr_trend.png"),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)




# Trend for Venezuelans
#-------------------------------------------------------------------------------

vz_ratio_dt <- rais %>%
  mutate(
    group = ifelse(state == "Roraima", "Roraima", "Other Northern States"),
    group = ifelse(is.na(group), "Other Northern States", group)
  ) %>%
  mutate(nonbrazilian = ifelse(nationality != "10", 1, 0)) %>%
  filter(nationality %in% c(10, 26)) %>%
  group_by(group, year, nonbrazilian) %>%
  summarise(n = n()) %>%
  group_by(group, year) %>%
  mutate(share = 100 * n / sum(n)) %>%
  filter(nonbrazilian == 1) %>%
  bind_rows(
    tibble(
      year = rep(c(2008:2010), 2),
      share = rep(c(0, 0, 0), 2),
      group = rep(c("Roraima", "Other Northern States"), each = 3)
    )
  )

p <- ggplot(vz_ratio_dt, aes(x = year, y = share)) +
  geom_point(aes(color = group, shape = group)) +
  geom_line(aes(color = group, group = group)) +
  theme_minimal() +
  ylim(0, 3) +
  geom_vline(
    xintercept = 2013,
    linetype = "dashed",
    color = "#b90c2e"
  ) +
  xlab("Years") +
  ylab("Percentage of Venezuelans in the Formal Sector") +
  theme_minimal() +
  scale_color_aaas() +
  theme_publication() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 11)) +
  theme(legend.position = "bottom")

ggsave(
  plot = p,
  paste0("out/fig_vz_trend.png"),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


#-------------------------------------------------------------------------------
# Wage Trends
#-------------------------------------------------------------------------------

ipca <- tibble(
  year = c(2008:2018),
  ipca = c(
    1,
    1.043,
    1.043 * 1.059,
    1.043 * 1.059 * 1.065,
    1.043 * 1.059 * 1.065 * 1.058,
    1.043 * 1.059 * 1.065 * 1.058 * 1.059,
    1.043 * 1.059 * 1.065 * 1.058 * 1.059 * 1.064,
    1.043 * 1.059 * 1.065 * 1.058 * 1.059 * 1.064 * 1.1067,
    1.043 * 1.059 * 1.065 * 1.058 * 1.059 * 1.064 * 1.1067 * 1.0629,
    1.043 * 1.059 * 1.065 * 1.058 * 1.059 * 1.064 * 1.1067 * 1.0629 * 1.0295,
    1.043 * 1.059 * 1.065 * 1.058 * 1.059 * 1.064 * 1.1067 * 1.0629 * 1.0295 * 1.037
  )
)


# Trend for the states
rais <- read_fst("data/data_wrangled_final.fst") %>%
  filter(state %in% c("Roraima", "Acre", "Amapá"))

#-------------------------------------------------------------------------------
plot_data <- rais %>%
  left_join(ipca, by = "year") %>%
  mutate(real_wage = nominal_wage / ipca) %>%
  group_by(treated, year) %>%
  summarise(mean_wage = mean(real_wage)) %>%
  ungroup() %>%
  mutate(group = ifelse(treated == 1, "Roraima", "Control States"))

p <- plot_data %>%
  ggplot() +
  geom_point(aes(
    x = year,
    y = mean_wage,
    group = group,
    color = group,
    shape = group
  )) +
  geom_line(aes(
    x = year,
    y = mean_wage,
    group = group,
    color = group,
    linetype = group
  )) +
  theme_minimal() +
  geom_vline(
    xintercept = 2014,
    linetype = "dashed"
  ) +
  xlab("Years") +
  ylab("Monthly Nominal Average Wage") +
  scale_color_aaas() +
  theme_publication() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 11)) +
  theme(legend.position = "bottom")

ggsave(
  plot = p,
  paste0("out/fig_wage_trends.png"),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# END OF SCRIPT
