#-------------------------------------------------------------------------------
# Robustness check..... Synthetic Control, Synthetic DID...
#-------------------------------------------------------------------------------
#
# Author: H. Sant'Anna
# Date: 2024
# Institution: University of Georgia
# Version: 1.0
#
#-------------------------------------------------------------------------------

# Loading required libraries
# -----------------------------------------------------------------------------

library(tidyverse)
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader
library(synthdid) # Synthetic control and DID
library(ggsci) # Pretty Plots

# Aux function
source("scripts/vz00_aux.R")


# Loading the dataset
# -----------------------------------------------------------------------------

# Pulling the data, and creating the treatment variable
dt <- read_fst("data/data_wrangled_final.fst") %>%
  mutate(treat = ifelse(state == "Roraima" & year >= 2014, 1, 0))


# Dataset for the synthetic control. We remove Amazonas since it got the 
#   treatment in 2014 (some VZ went there and may not be a good control)
dt_synth <- dt %>%
  filter(state != "Amazonas") %>%
  select(state, year, log_wage, treat, employed_dec31) %>%
  group_by(state, year) %>%
  summarise(
    log_wage = mean(log_wage), treat = mean(treat),
    employed_dec31 = mean(employed_dec31)
  ) %>%
  ungroup() %>%
  mutate(treat = as.integer(treat)) %>%
  as.data.frame()

setup_lw <- panel.matrices(dt_synth, unit = 1, time = 2, outcome = 3, treatment = 4)
#setup_emp <- panel.matrices(dt_synth, unit = 1, time = 2, outcome = 5, treatment = 4)

# Compute synthdid estimate for log wage
sdd_est_lw <- synthdid::synthdid_estimate(setup_lw$Y, setup_lw$N0, setup_lw$T0)

# extract the ggplot_build
ggbuild <- ggplot_build(plot(sdd_est_lw, se.method = "placebo"))$data[[1]]
ggbuild <- ggbuild %>%
  mutate(
    group = ifelse(group == 1, "Synthetic Control", "Roraima"),
    colour = ifelse(group == "Synthetic Control", "Synthetic Control", "Roraima")
  )

# Bulding the plots
p <- ggplot(ggbuild) +
  geom_line(aes(x = x, y = y, group = group, color = group, linetype = group)) +
  geom_point(aes(x = x, y = y, group = group, color = group, shape = group)) +
  geom_vline(
    xintercept = 2013,
    linetype = "dashed",
    color = "#b90c2e"
  ) +
  xlab("Years") +
  ylab("Estimates") +
  theme_publication() +
  scale_color_aaas() +
  theme(text = element_text(family = "serif"), legend.position = "bottom") +
  scale_x_continuous(breaks = function(x) pretty(x, n = 11))

ggsave(
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  filename = paste0(
    "out/fig_synthdid_lw.png"
  )
)


# weight analysis
# -----------------------------------------------------------------------------

weight_plot <- tibble(
  states = unique(dt_synth$state[dt_synth$state != "Roraima"]),
  weights = ggplot_build(synthdid_units_plot(sdd_est_lw, se.method = "placebo"))$data[[1]] $y *
   ggplot_build(synthdid_units_plot(sdd_est_lw, se.method = "placebo"))$data[[1]]$size
)

weight_plot$weights <- weight_plot$weights / sum(weight_plot$weights)
weight_plot <- weight_plot %>% arrange(weights)
weight_plot <- weight_plot %>% mutate(states = factor(
  states, levels = states, ordered = TRUE))

# Assuming your data frame is named 'weight_plot'
p <- ggplot(weight_plot) +
  geom_segment( aes(x=states, xend=states, y=0, yend=weights), color="#051c25") +
  geom_point(aes(x = states, y = weights), color="blue", size=4, alpha=0.6) +
  xlab("States") +
  ylab("Weights") +
  theme_light() +
  coord_flip() +
  theme_publication() +
  theme(legend.position = "none")

# Remove the legend if it's not necessary
ggsave(
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  filename = paste0(
    "out/fig_sdid_weights.png"
  ),
  bg = "white"
)




# Compute synth control estimate for log wage
#-------------------------------------------------------------------------------

sdd_est_lw <- synthdid::sc_estimate(setup_lw$Y, setup_lw$N0, setup_lw$T0)

# extract the ggplot_build
ggbuild <- ggplot_build(plot(sdd_est_lw, se.method = "placebo"))$data[[1]]
ggbuild <- ggbuild %>%
  mutate(
    group = ifelse(group == 1, "Synthetic Control", "Roraima"),
    colour = ifelse(group == "Synthetic Control", "Synthetic Control", "Roraima")
  )

p <- ggplot(ggbuild) +
  geom_line(aes(x = x, y = y, group = group, color = group, linetype = group)) +
  geom_point(aes(x = x, y = y, group = group, color = group, shape = group)) +
  geom_vline(
    xintercept = 2013,
    linetype = "dashed",
    color = "#b90c2e"
  ) +
  xlab("Years") +
  ylab("Estimates") +
  theme_publication() +
  scale_color_aaas() +
  theme(text = element_text(family = "serif"), legend.position = "bottom") +
  scale_x_continuous(breaks = function(x) pretty(x, n = 11))

ggsave(
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  filename = paste0(
    "out/fig_scm_lw.png"
  )
)

weight_plot <- tibble(
  states = unique(dt_synth$state[dt_synth$state != "Roraima"]),
  weights = c(0.8, 0.199, 0.01, 0, 0)
)

weight_plot <- weight_plot %>% arrange(weights) %>% mutate(states = factor(
  states, levels = states, ordered = TRUE))

p <- ggplot(weight_plot) +
  geom_segment( aes(x=states, xend=states, y=0, yend=weights), color="#051c25") +
  geom_point(aes(x = states, y = weights), color="blue", size=4, alpha=0.6) +
  xlab("States") +
  ylab("Weights") +
  theme_light() +
  coord_flip() +
  theme_publication() +
  theme(legend.position = "none")

ggsave(
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  filename = paste0(
    "out/fig_scm_weights.png"
  )
)