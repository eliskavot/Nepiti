

# Packages and data download ----------------------------------------------

packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled")
purrr::walk(packages, library, character.only = TRUE)

data <- read_sav(file = "Data/Alkohol 2025_v02.sav") %>%    
  mutate(across(where(is.labelled), as_factor))
data_labelled <- generate_dictionary(data)



# Analysis K1 K1 --------------------------------------------------------------

data %>% 
  count(nQ61_0_1)

  

?count
