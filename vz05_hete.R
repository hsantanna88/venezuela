#-------------------------------------------------------------------------------
# Description: Heterogeneity analysis (education and etc)
#-------------------------------------------------------------------------------
#
# Author: H. Sant'Anna
# Date: 2023
# Institution: University of Georgia
# Version: 1.1
#
#-------------------------------------------------------------------------------
# Loading required libraries
# -----------------------------------------------------------------------------

library(tidyverse) # Data wrangling
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader
library(fixest) # Fast fixed effects models
library(modelsummary) # Pretty tables
library(kableExtra) # Even prettier tables
library(ggsci) # Scientific colors

source("scripts/vz00_aux.R")

# Loading the dataset
# -----------------------------------------------------------------------------
dt <- read_fst("data/data_wrangled_final.fst") %>%
  filter(state %in% c("Roraima", "Amapá", "Acre")) %>%
  mutate(
    treated = ifelse(state == "Roraima", 1, 0),
    activity = substr(activity, 1, 2),
    occupation = substr(occupation, 1, 3)
  )


dt <- dt %>%
  mutate(
    education = as.numeric(education),
    educ = case_when(
      education < 7 ~ "low",
      education %in% c(7, 8) ~ "hschool",
      education > 8 ~ "college"
    )
  )

ps_model1 <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure + tenure^2,
  data = filter(dt, educ == "college"),
  family = "binomial",
  cluster = ~municipality,
))

ps_model2 <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure + tenure^2,
  data = filter(dt, educ == "hschool"),
  family = "binomial",
  cluster = "municipality",
))

ps_model3 <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure + tenure^2,
  data = filter(dt, educ == "low"),
  family = "binomial",
  cluster = "municipality",
))

dt <- bind_rows(
  dt %>%
    filter(educ == "college") %>%
    mutate(ps_model = ps_model1),
  dt %>%
    filter(educ == "hschool") %>%
    mutate(ps_model = ps_model2),
  dt %>%
    filter(educ == "low") %>%
    mutate(ps_model = ps_model3)
)

dt <- dt %>%
  mutate(
    ps = ifelse(treated == 1, 1 / ps_model, 1 / (1 - ps_model))
  ) %>%
  filter(ps <= quantile(ps, 0.995))

plot_densities <- function(data, ed) {
  data <- data %>%
    filter(educ == ed)

  # Combine the data into one data frame for ggplot
  combined_data <- rbind(
    data.frame(ps_model = data$ps_model[data$treated == 1], group = "Treated"),
    data.frame(ps_model = data$ps_model[data$treated == 0], group = "Control")
  )

  # Create the plot
  p <- ggplot(combined_data) +
    geom_density(aes(x = ps_model, fill = group, linetype = group), alpha = 0.5) +
    labs(x = "Propensity Scores", y = "Density") +
    theme_minimal() +
    theme_publication() +
    scale_fill_aaas()

  ggsave(p,
    file = paste0("out/fig_ps_", ed, "_density.png"),
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

plot_densities(dt, "college")
plot_densities(dt, "hschool")
plot_densities(dt, "low")

# Function to generate the models
#-------------------------------------------------------------------------------
vz_model <- function(data) {
  frml <- formula(c(log_wage) ~ sw(treat, vz_ratio) | pis + year)

  model <- feols(
    frml, data,
    cluster = ~municipality,
    split = ~educ,
    weights = ~ps
  )

  return(model)
}

twfemodel_educ <- vz_model(dt)

vz_es_model <- function(data) {
  frml <- formula(c(log_wage) ~ sw(i(year, treated, 2013), i(year, vz_ratio, 2013)) | pis + year)
  model <- feols(
    frml, data,
    cluster = ~municipality,
    split = ~educ,
    weights = ~ps
  )
  return(model)
}

esmodel_educ <- vz_es_model(dt)



# Table Generator
#------------------------------------------------------------
# Loading the models
load("out/twfemodel_educ.Rdata")
load("out/esmodel_educ.Rdata")


tabprinter <- function(data, model, outcome = "main") {
  # Define a custom statistic function to count the number of clusters
  n_cl <- as.integer(length(unique(data$municipality)))

  names(model) <- sapply(
    1:length(model),
    function(x) paste0("(", x, ")")
  )

  # Create a custom row for the table using a tribble
  row <- tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "N Clusters", n_cl, n_cl, n_cl, n_cl, n_cl, n_cl,
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
    add_header_above(c(" ", "Log Wage" = 2)) %>%
    footnote(number = c(note_muni, note_cova, note_pval)) %>%
    save_kable(
      file = paste0(
        "out/tab_", outcome, ".tex"
      )
    )
}

# Generate the tables
tabprinter(dt, twfemodel_educ, outcome = "educbinary")


# Event Study
#-------------------------------------------------------------------------------


eventstudy <- function(model, outcome = "educ") {
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

eventstudy(esmodel_educ, outcome = "educ")
