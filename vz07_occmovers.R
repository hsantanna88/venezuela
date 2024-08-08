# -----------------------------------------------------------------------------
# Hugo Sant'Anna
# November 2023
# Description: This scripts analyzes occupation dynamics

# Loading required libraries
# -----------------------------------------------------------------------------

library(tidyverse) # Data wrangling
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader
library(fixest) # Fast fixed effects models
library(modelsummary) # Pretty tables
library(kableExtra) # Even prettier tables
library(ggsci) # Scientific colors

# Loading the dataset
# -----------------------------------------------------------------------------

dt_vz_orig <- read_fst("./data/parsed_data_vz.fst") %>%
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
  filter(state %in% c("Roraima", "Amapá", "Acre")) %>%
  filter(nationality == 26) %>%
  mutate(
    occupation = substr(occupation, 1, 3),
    activity = substr(activity, 1, 2),
  )

# Loading the dataset
dt <- read_fst("data/data_wrangled_final.fst") %>%
  filter(state %in% c("Roraima", "Amapá", "Acre")) %>%
  mutate(
    activity = substr(activity, 1, 2),
    occupation = substr(occupation, 1, 3)
  )

# Only the individuals that appear in 2017
dt_br <- dt %>%
  filter(year %in% c(2017, 2018)) %>%
  filter(nationality == 10) %>%
  group_by(occupation, year) %>%
  count() %>%
  ungroup()

dt_final <- dt_vz_orig %>%
  filter(year %in% c(2017, 2018)) %>%
  filter(nationality == 26) %>%
  group_by(occupation, year) %>%
  count() %>%
  ungroup() %>%
  right_join(dt_br, by = c("occupation", "year")) %>%
  mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>%
  mutate(ratio = n.x / n.y) %>%
  group_by(occupation) %>%
  mutate(ratio = mean(ratio)) %>%
  filter(year != 2018) %>%
  ungroup() %>%
  mutate(
    immi_occ = case_when(
      ratio > 0 ~ "any",
      TRUE ~ "none"
    )
  )


dt <- dt %>%
  left_join(select(dt_final, immi_occ, occupation), by = c("occupation")) %>%
  distinct()

mover_dt <- dt %>%
  arrange(pis, year) %>%
  group_by(pis) %>%
  mutate(
    mover = ifelse(lag(immi_occ) == "any" & immi_occ == "none", 1, 0)
  ) %>%
  ungroup()


ps_model <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure +
    tenure^2 + education,
  data = mover_dt,
  family = "binomial",
  cluster = "municipality"
))

dt <- mover_dt %>%
  mutate(
    ps = ifelse(treated == 1, 1 / ps_model, 1 / (1 - ps_model))
  )




# Function to generate the models
#-------------------------------------------------------------------------------
vz_model <- function(data) {
  frml <- formula(mover ~ sw(treat, vz_ratio) | pis + year)

  model <- feols(
    frml, data,
    cluster = ~municipality,
    weights = ~ps
  )
  return(model)
}

twfemodel_immimover <- vz_model(dt)



vz_es_model <- function(data) {
  frml <- formula(mover ~ sw(i(year, treated, 2013), i(year, vz_ratio, 2013)) | pis + year)
  model <- feols(
    frml, data,
    cluster = ~municipality,
    weights = ~ps
  )
  return(model)
}

esmodel_immimover <- vz_es_model(dt)



# Generating the table
tabprinter <- function(data, model, outcome = "main") {
  # Define a custom statistic function to count the number of clusters
  n_cl <- as.integer(length(unique(data$municipality)))

  names(model) <- sapply(
    1:length(model),
    function(x) paste0("(", x, ")")
  )

  # Create a custom row for the table using a tribble
  row <- tribble(
    ~term, ~"(1)", ~"(2)",
    "N Clusters", n_cl, n_cl
  )
  attr(row, "position") <- c(10)

  # Create the output file
  modelsummary(
    model,
    coef_map = dict,
    coef_omit = "^(?!treat|vz_ratio)",
    gof_map = gof_names,
    # fmt = 3,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = row,
    output = "latex_tabular"
  ) %>%
    add_header_above(c(" ", "Log Wage" = 2, "Job Retainment" = 2)) %>%
    footnote(number = c(note_muni, note_cova, note_pval)) %>%
    save_kable(
      file = paste0(
        "out/tab_", outcome, ".tex"
      )
    )
}

# Generate the tables
tabprinter(dt, twfemodel_immimover, outcome = "immimover")
