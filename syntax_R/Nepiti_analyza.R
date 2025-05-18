
packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled")
purrr::walk(packages, library, character.only = TRUE)


data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))
data_labelled <- generate_dictionary(data)


basic_color = "#137F41"

data %>%
  filter(!is.na(nQ52_r1)) %>% 
  count(nQ52_r1) %>%
  mutate(procento = n / sum(n) * 100,
         nQ52_r1 = reorder(nQ52_r1, procento)) %>%
  ggplot(aes(x = nQ52_r1, y = procento)) +
  geom_col(fill = basic_color) +
  geom_text(aes(label = paste0(" ", round(procento, 0), "%")), 
            hjust = 0.5, size = 3.5, fontface = "bold")+
  theme_minimal() +
  coord_flip() +
  labs(title = "Abstinence < 3 týdny", x = "", y = "%", subtitle = paste("N = 397"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))
  
# Jak rozšířená je krátkodobá  abstinence v české společnosti? ------------




# V jakých částech společnosti je krátkodobá/dlouhodobá abstinence --------




# Má krátkodobá abstinence efekt na snižování míry konzumace alkoh --------




# Jaké jsou osobní důvody (motivace) občanů ČR ke krátkodobé absti --------




# Co představuje překážky během krátkodobé abstinence? --------------------





# Co představuje překážky vedoucí k předčasnému ukončení krátkodob --------





# Jaká část obyvatel ČR se pokouší nějakým způsobem kontrolovaně o --------




# Jaké strategie občané ČR využívají v oblasti kontrolovaného omez --------



