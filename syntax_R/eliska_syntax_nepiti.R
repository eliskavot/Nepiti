packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car")

purrr::walk(packages, library, character.only = TRUE)



data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))

library(labelled)
data_labelled <- generate_dictionary(data)
