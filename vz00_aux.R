#-------------------------------------------------------------------------------
# Description: Bunch of functions for plotting and table
#-------------------------------------------------------------------------------
#
# Author: H. Sant'Anna
# Date: 2024
# Institution: University of Georgia
# Version: 1.0
#
#-------------------------------------------------------------------------------



# Loading required libraries
#-------------------------------------------------------------------------------

library(ggplot2)

# Function to create a publication ready theme
theme_publication <- function(base_size = 20) {
  theme_minimal(base_size = base_size) +
    theme(
      aspect.ratio = , # this makes the physical aspect ratio square
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_line(color = "grey90"),
      panel.grid.major = element_line(color = "grey90"),
      panel.border = element_blank(),
      axis.text.x = element_text(
        # angle = 45,
        hjust = 1, size = base_size - 2
      ),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size),
      text = element_text(family = "serif")
    )
}

# Dictionary of Variables for the table
#-------------------------------------------------------------------------------

dict <- c(
  "GENDERM" = "Male",
  "male" = "Male",
  "white" = "White",
  "education" = "Education",
  "mixed" = "Mixed",
  "AGE" = "Age",
  "I(AGE^2)" = "Age-squared",
  "age" = "Age",
  "I(age^2)" = "Age-squared",
  "cum_number_fr" = "Hundredths of Venezuelans",
  "treated_bin" = "Binary Treatment",
  "treated" = "Treated",
  "post" = "Post",
  "treat" = "Treat: Binary",
  "treat:post" = "Treat: Binary",
  "treat:post:act_int" = "Treat x Post",
  "treat1:post" = "Activity: Treat x Post",
  "year::2010:treat_unit" = "Roraima x 2010",
  "year::2011:treat_unit" = "Roraima x 2011",
  "year::2012:treat_unit" = "Roraima x 2012",
  "year::2013:treat_unit" = "Roraima x 2013",
  "year::2015:treat_unit" = "Roraima x 2015",
  "year::2016:treat_unit" = "Roraima x 2016",
  "year::2017:treat_unit" = "Roraima x 2017",
  "tenure" = "Tenure",
  "vz_ratio" = "Treat: VZ Ratio x 100",
  "treated:tenure" = "Treated x Tenure",
  "log_wage" = "Log Wage",
  "get(\"CPF\")" = "Individual",
  "get(\"MUNI\")" = "Municipality",
  "year" = "Year",
  "PLANT_ID" = "Firms",
  "n_firms" = "Number of Firms",
  "firm_concentration" = "Worker Firm Ratio",
  "Food and Bed" = "Restaurant",
  "Commerce" = "Retail",
  "fr_ratio" = "Ratio",
  "brazilians" = "# of Brazilians",
  "venezuelans" = "# of Venezuelans",
  "ratio" = "Ratio",
  "n" = "N"
)


# Dictionary for "metadata" of goodness-of-fit in the table
gof_names <- tribble(
  ~raw, ~clean, ~fmt,
  "FE: pis", "Individual FE", 3,
  "FE: cnpj^pis", "Individual x Firm FE", 1,
  "FE: municipality^pis", "Individual x Municipality FE", 1,
  "FE: cnpj", "Firm FE", 1,
  "FE: municipality", "Municipality FE", 1,
  "FE: year", "Year FE", 1,
  "r.squared", "R2", 3,
  "adj.r.squared", "R2 Adj.", 3,
  "rmse", "RMSE", 3,
  "nobs", "N", 0
)

# Footnotes for the table
note_firm <- "Standard-errors are clustered by firm identifiers."
note_muni <- "Standard-errors are clustered by municipality."
note_stat <- "Standard-errors are clustered by state."
note_cova <- "Propensity score explanatory variables are gender and race indicators, age, age-squared, tenure, tenure-squared and education level."
note_pval <- "* p < 0.1, ** p < 0.05, *** p < 0.01"
note_pop <- "Control for labor market population is present."
