
packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled")
purrr::walk(packages, library, character.only = TRUE)


data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))
data_labelled <- generate_dictionary(data)



# Jak rozšířená je krátkodobá  abstinence v české společnosti? ------------




# V jakých částech společnosti je krátkodobá/dlouhodobá abstinence --------




# Má krátkodobá abstinence efekt na snižování míry konzumace alkoh --------




# Jaké jsou osobní důvody (motivace) občanů ČR ke krátkodobé absti --------




# Co představuje překážky během krátkodobé abstinence? --------------------





# Co představuje překážky vedoucí k předčasnému ukončení krátkodob --------





# Jaká část obyvatel ČR se pokouší nějakým způsobem kontrolovaně o --------




# Jaké strategie občané ČR využívají v oblasti kontrolovaného omez --------



