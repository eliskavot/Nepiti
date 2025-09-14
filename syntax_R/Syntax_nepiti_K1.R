

# Packages and data download ----------------------------------------------

packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled")
purrr::walk(packages, library, character.only = TRUE)

data <- read_sav(file = "Data/Alkohol 2025_v02.sav") %>%    
  mutate(across(where(is.labelled), as_factor))
data_labelled <- generate_dictionary(data)


# NASTAVENÍ BAREVNÝCH PALET

basic_color = "#D9A939"
missing_color = "grey80"

seq_pallet5 = c("#FAF0D1", "#F0C661", "#D9A939", "#B57F22", "#855A13")
seq_pallet4 = c("#FAF0D1", "#F0C661", "#B57F22", "#855A13")

# Analysis K1 jen zkousim jak na to :)))))))))) zadny vysledky tu zatim nejsou --------------------------------------------------------------

n <- nrow(data) #ukaze nam pocet pripadu

#STRATEGIE omezeni alkoholu:
#  nQ61_0_1 - nQ61_10_1


data %>% 
  count(nQ61_0_1)

data %>% 
  mean(nQ61_0_1)

describe(data$nQ61_0_1)

data %>% 
  select(starts_with("nQ61"))


table(data$nQ61_0_1, useNA = "ifany")
levels(data$nQ61_0_1)

#netusim co to je
recode_levels_2 <- function(x) {
  fct_collapse(
    x,
    "0 = Vůbec nepomáhalo" = c("0 = Vůbec"),
    "2" = c("1","2", "3"),
    "3" = c("4", "5", "6"),
    "Velmi" = c("7", "8", "9", "10 = Velmi pomáhalo"),
    na_if(x = "Tuto strategii jsem nepraktikoval/a"),
    "Nevím" = c("Nevím"),
    rn
  )
}

#nefunguje
data_s_upr_nQ61 <- data %>%
  mutate(across(starts_with("nQ61_"), recode_levels_2))

levels(data$nQ61_0_1)

# prekodovani -------------------------------------------------------------


table(data$nQ58_0_1, useNA = "ifany")
levels(data$nQ58_0_1)

#nefunguje
recode_levels <- function(x) {
  fct_collapse(
    x,
    "Zcela nedůležité" = c("0 = Zcela nedůležité", "1"),
    "2" = c("2", "3"),
    "3" = c("4", "5", "6"),
    "4" = c("7", "8"),
    "Zcela zásadní" = c("9", "10 = Naprosto zásadní"),
    "Nevím" = c("Nevím")
  )
}

# prumerne pouyzivani strategii pro cloveka --------------------------------

#nefunguje
data %>%
  mutate(avg_strategy_use = rowMeans(select(., starts_with("nQ61"), na.rm = TRUE))) %>%
  arrange(avg_strategy_use) %>%
  select(celk_spotr3_5kat, avg_strategy_use)


nrow()

# kratkodoba abstinence ---------------------------------------------------
ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 37)) +
  labs(title = "Zkušenost s krátkodobou abstinencí (delší než 3 týdny)", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()
