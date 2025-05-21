packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car")

library(ggplot2)
library(tidyverse)
library(scales)
library(stringr)
library(forcats)
purrr::walk(packages, library, character.only = TRUE)


data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))


library(labelled)
data_labelled <- generate_dictionary(data)


basic_color = "#D9A939"
missing_color = "grey80"

seq_pallet5 = c("#FAF0D1", "#F0C661", "#D9A939", "#B57F22", "#855A13")
seq_pallet4 = c("#FAF0D1", "#F0C661", "#B57F22", "#855A13")
# Výpočet intervalů spolehlivosti + graf ----------------------------------


tabulka <- table(data$nQ55_r1) #zde je třeba vložit proměnnou, u které chceme ověřit

# 2. Celkový počet platných odpovědí (bez NA)
n <- sum(tabulka)

# 3. Inicializace datového rámce pro výstup
vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

# 4. Smyčka přes všechny odpovědi
for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]  # počet výskytů dané odpovědi
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = round(x / n, 3),
    CI_dolni = round(test$conf.int[1], 3),
    CI_horni = round(test$conf.int[2], 3)
  ))
}

# 5. Výpis výsledků
print(vysledky)

vysledky$Podil_pct <- vysledky$Podil * 100
vysledky$CI_dolni_pct <- vysledky$CI_dolni * 100
vysledky$CI_horni_pct <- vysledky$CI_horni * 100

# Vykreslení grafu
ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = CI_dolni_pct, ymax = CI_horni_pct), width = 0.2) +
  labs(
    title = "Podíl odpovědí s 95% intervaly spolehlivosti",
    x = "Odpověď",
    y = "Procento (%)"
  ) +
  theme_minimal()

# Konec kontroly intervalů spolehlivosti ----------------------------------


# Motivace ke krátkodobé abstinenci ---------------------------------------

table(data$nQ58_0_1, useNA = "ifany")
levels(data$nQ58_0_1)

recode_levels <- function(x) {
  fct_collapse(
    x,
    "Zcela nedůležité" = c("0 = Zcela nedůležité", "1"),
    "Spíše nedůležité" = c("2", "3"),
    "Středně důležité" = c("4", "5", "6"),
    "Spíše důležité" = c("7", "8"),
    "Zcela zásadní" = c("9", "10 = Naprosto zásadní"),
    "Nevím" = c("Nevím")
  )
}

data_s_upr_nQ58 <- data %>%
  mutate(across(matches("^nQ58_\\d+_1$"), recode_levels))

data_s_upr_nQ58 %>%
  select(starts_with("nQ58"), -nQ58_0_1) %>%
  filter(!is.na(nQ58_2_1)) %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "value") %>%
  count(item, value) %>%
  left_join(data_labelled %>% 
              select(item = variable, label) %>%
              mutate(label = str_extract(label, "\\[.*?\\]") %>% 
                       str_remove_all("\\[|\\]")), 
            by = "item") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n, na.rm = TRUE),
         pos_freq = sum(percent[value %in% c("Zcela zásadní", "Spíše důležité")])) %>%
  ungroup() %>%
  mutate(
    percent_label = percent(percent, accuracy = 1, suffix = ""),
    percent_label = if_else(percent <= 0.05, "", percent_label),
    label = paste("...", label, sep = ""),
    label = fct_reorder(label, pos_freq),
    value = fct_rev(value)
  ) %>%
  ggplot(aes(x = percent, y = label, fill = value, label = percent_label)) +
  geom_col(color = "white") +
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(labels = ~str_wrap(., width = 40)) +
  scale_fill_manual(values = c(rev(seq_pallet5), missing_color)) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "right")
