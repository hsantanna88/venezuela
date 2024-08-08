#-------------------------------------------------------------------------------
# Description: Function to parse the raw data
#-------------------------------------------------------------------------------
#
# Author: H. Sant'Anna
# Date: 2024
# Institution: University of Georgia
# Version: 1.0
#
#-------------------------------------------------------------------------------
# Here I will parse the raw data from the RAIS database to a more manageable
#   format. The data is stored in the .fst format, which is a fast and
#   efficient way to store data in R.

# In the end, I will save the data in a .fst file to be used in the next
#   scripts.

# Loading required libraries
#-------------------------------------------------------------------------------

library(tidyverse) # Tidyverse package.
library(tidylog) # Verbose tidyverse functions
library(fst) # fast data loader



# Testing the parallelization of fst
#-------------------------------------------------------------------------------

library(parallel)
num_cores <- detectCores() - 1
fst::threads_fst(num_cores)

# Extracting the data
#-------------------------------------------------------------------------------

# Function to parse RAIS to a more manageable format
# It also harmonizes the variables.
parser <- function(ano) {
  path <- paste0("../data_raw/rais", ano, ".fst")

  if (ano <= 2010) {
    columns <- c(
      "MUNI", "PLANT_ID", "PIS", "CPF", "EARN_AVG_MONTH_NOM",
      "CONTRACT_SALARY", "EARN_AVG_MONTH_MW", "NUM_HOURS_CONTRACTED",
      "GENDER", "RACE_v2006", "DATE_OF_BIRTH", "EDUC_v2005", "TENURE_MONTHS",
      "NATIONALITY", "HIRE_DATE", "CONTRACT_TYPE", "NUM_LEAVE_DAYS",
      "LEAVE_1_CAUSE", "MONTH_OF_SEP", "CAUSE_OF_SEP", "OCCUP_CBO2002",
      "CNAE20_CLASS", "LEGAL_NATURE_CONCLA2002", "EMP_ON_DEC31"
    )

    data <- read_fst(path, columns = columns) %>%
      transmute(
        municipality = MUNI,
        cnpj = PLANT_ID,
        pis = PIS,
        cpf = CPF,
        year = ano,
        nominal_wage = EARN_AVG_MONTH_NOM,
        nominal_mw = EARN_AVG_MONTH_MW,
        hours_contract = NUM_HOURS_CONTRACTED,
        male = ifelse(GENDER == "M", 1, 0),
        race = RACE_v2006,
        age = ano - as.numeric(substr(DATE_OF_BIRTH, 1, 4)),
        education = EDUC_v2005,
        tenure = TENURE_MONTHS,
        nationality = NATIONALITY,
        hire_date = HIRE_DATE,
        contract_type = CONTRACT_TYPE,
        leave_days = NUM_LEAVE_DAYS,
        leave_cause = LEAVE_1_CAUSE,
        sep_date = MONTH_OF_SEP,
        sep_cause = CAUSE_OF_SEP,
        occupation = OCCUP_CBO2002,
        activity = CNAE20_CLASS,
        nature = LEGAL_NATURE_CONCLA2002,
        employed_dec31 = as.numeric(EMP_ON_DEC31)
      )
  } else if (ano > 2010 & ano < 2018) {
    columns <- c(
      "MUNI", "PLANT_ID", "PIS", "CPF", "EARN_AVG_MONTH_NOM",
      "CONTRACT_SALARY", "EARN_AVG_MONTH_MW", "NUM_HOURS_CONTRACTED",
      "GENDER", "RACE_v2006", "AGE", "EDUC_v2005", "TENURE_MONTHS",
      "NATIONALITY", "HIRE_DATE", "CONTRACT_TYPE", "NUM_LEAVE_DAYS",
      "LEAVE_1_CAUSE", "MONTH_OF_SEP", "CAUSE_OF_SEP", "OCCUP_CBO2002",
      "CNAE20_CLASS", "LEGAL_NATURE_CONCLA2002", "EMP_ON_DEC31"
    )
    data <- read_fst(path, columns = columns) %>%
      transmute(
        municipality = MUNI,
        cnpj = PLANT_ID,
        pis = PIS,
        cpf = CPF,
        year = ano,
        nominal_wage = EARN_AVG_MONTH_NOM,
        nominal_mw = EARN_AVG_MONTH_MW,
        hours_contract = NUM_HOURS_CONTRACTED,
        male = ifelse(GENDER == "M", 1, 0),
        race = RACE_v2006,
        age = AGE,
        education = EDUC_v2005,
        tenure = TENURE_MONTHS,
        nationality = NATIONALITY,
        hire_date = HIRE_DATE,
        contract_type = CONTRACT_TYPE,
        leave_days = NUM_LEAVE_DAYS,
        leave_cause = LEAVE_1_CAUSE,
        sep_date = MONTH_OF_SEP,
        sep_cause = CAUSE_OF_SEP,
        occupation = OCCUP_CBO2002,
        activity = CNAE20_CLASS,
        nature = LEGAL_NATURE_CONCLA2002,
        employed_dec31 = as.numeric(EMP_ON_DEC31)
      )
  } else if (ano == 2018) {
    # 2018 with Portuguese variables

    columns <- c(
      "município", "cnpjcei", "pis", "cpf",
      "vlremunmédianom", "vlremunmédiasm", "qtdhoracontr", "sexotrabalhador",
      "raçacor", "idade", "escolaridadeapós2005", "tempoemprego", "nacionalidade",
      "dataadmissãodeclarada", "tipovínculo", "qtddiasafastamento", "causaafastamento1",
      "mêsdesligamento", "motivodesligamento", "cboocupação2002", "cnae20classe",
      "naturezajurídica", "vínculoativo3112"
    )

    data <- read_fst(path, column = columns) %>%
      transmute(
        municipality = as.character(município),
        cnpj = str_pad(as.character(cnpjcei), width = 14, side = "left", pad = "0"),
        pis = str_pad(as.character(pis), width = 11, side = "left", pad = "0"),
        cpf = str_pad(as.character(cpf), width = 11, side = "left", pad = "0"),
        year = ano,
        nominal_wage = as.numeric(gsub(",", ".", vlremunmédianom)),
        nominal_mw = as.numeric(gsub(",", ".", vlremunmédiasm)),
        hours_contract = qtdhoracontr,
        male = ifelse(sexotrabalhador == 1, 1, 0),
        race = str_pad(as.character(raçacor), width = 2, side = "left", pad = "0"),
        age = idade,
        education = str_pad(as.character(escolaridadeapós2005), width = 2, side = "left", pad = "0"), #
        tenure = as.numeric(gsub(",", ".", tempoemprego)),
        nationality = as.character(nacionalidade),
        hire_date = dmy(str_pad(as.character(dataadmissãodeclarada), width = 8, side = "left", pad = "0")),
        contract_type = str_pad(as.character(tipovínculo), width = 2, side = "left", pad = "0"),
        leave_days = qtddiasafastamento,
        leave_cause = str_pad(as.character(causaafastamento1), width = 2, side = "left", pad = "0"),
        sep_date = str_pad(as.character(mêsdesligamento), width = 2, side = "left", pad = "0"),
        sep_cause = str_pad(as.character(motivodesligamento), width = 2, side = "left", pad = "0"),
        occupation = cboocupação2002,
        activity = str_pad(as.character(cnae20classe), width = 5, side = "left", pad = "0"),
        nature = str_pad(as.character(naturezajurídica), width = 2, side = "left", pad = "0"),
        employed_dec31 = vínculoativo3112
      )
  }

  # returning the data
  return(data)

  # Printing the current state
  print(paste0(
    "Year ", ano, " parsed. Completed: ",
    round((ano - 2007) / 11, 2) * 100, "%"
  ))
}

# Parsing the files
data_list <- lapply(2008:2018, parser)

# Read and row bind data from all files
data_list <- bind_rows(data_list)


# Saving progress
write_fst(data_list, "./data/parsed_data_vz.fst")

# END OF SCRIPT
