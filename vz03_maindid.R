#-------------------------------------------------------------------------------
# Description: Main Model for the Venezuela project
#-------------------------------------------------------------------------------
#
# Author: H. Sant'Anna
# Date: 2023
# Institution: University of Georgia
# Version: 1.1
#
#-------------------------------------------------------------------------------

# Loading required libraries
#-------------------------------------------------------------------------------

library(tidyverse) # Data wrangling
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader
library(fixest) # Fast fixed effects models
library(modelsummary) # Pretty tables
library(kableExtra) # Even prettier tables
library(ggsci) # Scientific colors
library(ggrepel) # Repel labels

source("scripts/vz00_aux.R")

# Loading the dataset
# In this case we only need the three states we using for the DID.

dt <- read_fst("data/data_wrangled_final.fst") %>%
  filter(state %in% c("Roraima", "Amapá", "Acre"))


# calculating the propensity scores
# This is for the doubly robust. We assume a logit relationship.
ps_model <- predict(feglm(
  treated ~ male + race + age + age^2 + tenure + tenure^2 + education,
  data = dt,
  family = "binomial",
  cluster = "municipality"
))


# Notice that we are using the inverse of the propensity score
# Filter the data to exclude the top 0.5% of the propensity scores

dt <- dt %>%
  mutate(
    ps_model = ps_model,
    ps = ifelse(treated == 1, 1 / ps_model, 1 / (1 - ps_model))
  ) %>%
  filter(ps <= quantile(ps, 0.995))


# Combine the data into one data frame for ggplot
combined_data <- rbind(
  data.frame(ps_model = dt$ps_model[dt$treated == 1], group = "Treated"),
  data.frame(ps_model = dt$ps_model[dt$treated == 0], group = "Control")
)

combined_data$group <- factor(combined_data$group, levels = c("Treated", "Control"))

# Create the plot
# This plot shows the histograms of the pscores for the treated and control groups
p <- ggplot(combined_data) +
  geom_density(aes(x = ps_model, fill = group, linetype = group), alpha = 0.5) +
  labs(x = "Propensity Scores", y = "Density") +
  theme_minimal() +
  theme_publication() +
  scale_fill_aaas()

# You can save here, or you can just use X11(), or Rstudio or whatever.
ggsave(p,
  file = "out/fig_ps_density.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# This is a function to gen the models, so we can have one code for everything.
# Employed Dec 31 is the job retention variable.
# We are using the doubly robust estimator here (notice the model has ps weights)
#-------------------------------------------------------------------------------
vz_model <- function(data) {
  frml <- formula(c(log_wage, employed_dec31) ~ sw(treat, vz_ratio) | pis + year)

  model <- feols(
    frml, data,
    cluster = ~municipality,
    weight = ~ps
  )

  return(model)
}

twfemodel <- vz_model(dt)


# This function here generates the event study for the main model.
vz_es_model <- function(data) {
  frml <- formula(c(log_wage, employed_dec31) ~ sw(i(year, treated, 2013), i(year, vz_ratio, 2013)) | pis + year)
  model <- feols(
    frml, data,
    cluster = ~municipality,
  )
  return(model)
}

esmodel <- vz_es_model(dt)

# Table Generator
#------------------------------------------------------------

tabprinter <- function(data, model, outcome = "main") {
  # Define a custom statistic function to count the number of clusters
  n_cl <- as.integer(length(unique(data$municipality)))

  names(model) <- sapply(
    1:length(model),
    function(x) paste0("(", x, ")")
  )

  # Create a custom row for the table using a tribble
  row <- tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", # ~"(5)", ~"(6)",
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
        "out/tab_", outcome, ".tex"
      )
    )
}

# Generate the tables
tabprinter(dt, twfemodel, outcome = "main")

# Event Study
#-------------------------------------------------------------------------------


eventstudy <- function(model, outcome = "main") {
  model <- model[c(1, 3)]

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
  lapply(1:2, function(x) plot_coef(x, model))
}

eventstudy(esmodel, outcome = "main")

# END OF SCRIPT
