packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car")

library(ggplot2)

purrr::walk(packages, library, character.only = TRUE)


data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))

library(labelled)
data_labelled <- generate_dictionary(data)

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


