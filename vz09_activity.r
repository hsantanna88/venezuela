# Hugo Sant'Anna
# November 2023
# Description: This scripts analyzes education

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
  filter(state == "Roraima") %>%
  group_by(activity, year) %>%
  count() %>%
  ungroup()

dt_final <- dt_vz_orig %>%
  filter(year %in% c(2017, 2018)) %>%
  filter(nationality == 26) %>%
  group_by(activity, year) %>%
  count() %>%
  ungroup() %>%
  right_join(dt_br, by = c("activity", "year")) %>%
  mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>%
  mutate(ratio = n.x / n.y) %>%
  group_by(activity) %>%
  mutate(ratio = mean(ratio)) %>%
  filter(year != 2018) %>%
  ungroup()

quant_top <- quantile(dt_final$ratio, 0.75)
quant_bottom <- quantile(dt_final$ratio, 0.25)

dt_final <- dt_final %>%
  mutate(
    immi_act = case_when(
      ratio > 0 ~ "any",
      ratio == 0 ~ "none"
    )
  )

dt <- dt %>%
  left_join(select(dt_final, immi_act, activity), by = c("activity")) %>%
  distinct()

# ps_model1 <- predict(feglm(
#   treated ~ male + race + age + age^2 + tenure + tenure^2 + education,
#   data = filter(dt, immi_act == "top"),
#   family = "binomial",
#   cluster = "municipality",
# ))


ps_model2 <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure + tenure^2 + education,
  data = filter(dt, immi_act == "any"),
  family = "binomial",
  cluster = "municipality",
))

ps_model3 <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure + tenure^2 + education,
  data = filter(dt, immi_act == "none"),
  family = "binomial",
  cluster = "municipality",
))

dt <- bind_rows(
  # dt %>%
  #   filter(immi_act == "top") %>%
  #   mutate(ps_model = ps_model1),
  dt %>%
    filter(immi_act == "any") %>%
    mutate(ps_model = ps_model2),
  dt %>%
    filter(immi_act == "none") %>%
    mutate(ps_model = ps_model3)
)

dt <- dt %>%
  mutate(
    ps = ifelse(treated == 1, 1 / ps_model, 1 / (1 - ps_model))
  ) %>%
  filter(ps <= quantile(ps, 0.995))


# Function to generate the models
#-------------------------------------------------------------------------------
vz_model <- function(data) {
  frml <- formula(c(log_wage) ~ sw(treat, vz_ratio) | pis + year)

  model <- feols(
    frml, data,
    cluster = ~municipality,
    split = ~immi_act,
    weights = ~ps
  )

  return(model)
}

twfemodel_immiact <- vz_model(dt)

vz_es_model <- function(data) {
  frml <- formula(c(log_wage) ~ sw(i(year, treated, 2013), i(year, vz_ratio, 2013)) | pis + year)
  model <- feols(
    frml, data,
    cluster = ~municipality,
    split = ~immi_act,
    weights = ~ps
  )
  return(model)
}

esmodel_immiact <- vz_es_model(dt)

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
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
    "N Clusters", n_cl, n_cl, n_cl, n_cl,
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
        "out/tab2_", outcome, ".tex"
      )
    )
}

# Generate the tables
tabprinter(dt, twfemodel_immiact, outcome = "immiact")

# Event Study
#-------------------------------------------------------------------------------
eventstudy <- function(model, outcome = "immiocc") {
  plot_coef <- function(index, model) {
    data <- iplot(model[[index]])
    data <- data$prms %>%
      rename(
        est = estimate,
        ci_low = ci_low,
        ci_high = ci_high,
        year = estimate_names
      )


    p <- ggplot(data) +
      geom_pointrange(aes(x = year, y = est, ymin = ci_low, ymax = ci_high)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = 2013,
        linetype = "dashed",
        color = "#b90c2e"
      ) +
      xlab("Years") +
      ylab("Estimates") +
      theme_minimal() +
      scale_color_aaas() +
      theme(text = element_text(family = "serif")) +
      scale_x_continuous(breaks = function(x) pretty(x, n = 11))

    ggsave(
      plot = p,
      filename = paste0(
        "out/fig_",
        outcome, index, ".png"
      ),
      width = 1500, height = 1000, units = "px", bg = "white"
    )

    print(paste("Plot saved for outcome", outcome))
  }
  lapply(1:length(model), function(x) plot_coef(x, model))
}

eventstudy(esmodel_immiact, outcome = "immiact")
