packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car")

library(ggplot2)
library(tidyverse)
library(scales)
library(stringr)
library(forcats)
purrr::walk(packages, library, character.only = TRUE)

data <- read_sav(file = "Data/Alkohol 2025_v02.sav") %>%    
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
    "2" = c("2", "3"),
    "3" = c("4", "5", "6"),
    "4" = c("7", "8"),
    "Zcela zásadní" = c("9", "10 = Naprosto zásadní"),
    "Nevím" = c("Nevím")
  )
}

data_s_upr_nQ58 <- data %>%
  mutate(across(matches("^nQ58_\\d+_1$"), recode_levels))

nQ58_battery = data_s_upr_nQ58 %>%
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
  scale_fill_manual(values = c(missing_color, rev(seq_pallet5))) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE, nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "top")+
  labs(title = "Důvody pro rozhodnutí krátkodobě abstinovat ",
       fill = "",
       y = "",
       x = "")

ggsave(plot = nQ58_battery, filename = "nQ58-battery.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24, height = 14.5, scaling = 1)
# kontrola nulařů ---------------------------------------------------------

kontrola_nul = data %>% 
  filter(celk_spotr2 == 0) %>% 
  select(celk_spotr2, nQ01_r1, starts_with("nQ13"))

table(kontrola_nul$nQ13_4_r)

# překážky při abstinenci -------------------------------------------------

table(data$nQ59_0_1, useNA = "ifany")
levels(data$nQ59_0_1)

recode_levels_2 <- function(x) {
  fct_collapse(
    x,
    "Vůbec ne" = c("0 = Vůbec"),
    "2" = c("1","2", "3"),
    "3" = c("4", "5", "6"),
    "Velmi" = c("7", "8", "9", "10 = Velmi"),
    "Nevím" = c("Nevím")
  )
}

data_s_upr_nQ59 <- data %>%
  mutate(across(matches("^nQ59_\\d+_1$"), recode_levels_2))

nQ59_battery = data_s_upr_nQ59 %>%
  select(starts_with("nQ59")) %>%
  filter(!is.na(nQ59_0_1)) %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "value") %>%
  count(item, value) %>%
  left_join(data_labelled %>% 
              select(item = variable, label) %>%
              mutate(label = str_extract(label, "\\[.*?\\]") %>% 
                       str_remove_all("\\[|\\]")), 
            by = "item") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n, na.rm = TRUE),
         pos_freq = sum(percent[value %in% c("Velmi")])) %>%
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
  scale_fill_manual(values = c(missing_color, rev(seq_pallet4))) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top")+
  labs(title = "Do jaké míry jste se během Vaší poslední krátkodobé abstinence potýkal/a
s následujícími překážkami? ",
       fill = "",
       y = "",
       x = "")

ggsave(plot = nQ59_battery, filename = "nQ59-battery.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24, height = 15, scaling = 1)


# Jaké strategie k úspěšnému dodržení krátkodobé abstinence lidé h --------

table(data$nQ61_0_1, useNA = "ifany")
levels(data$nQ61_0_1)

recode_levels_3 <- function(x) {
  fct_collapse(
    x,
    "Vůbec nepomáhalo" = c("0 = Vůbec nepomáhalo"),
    "2" = c("1","2", "3"),
    "3" = c("4", "5", "6"),
    "4" = c("7", "8", "9"),
    "Velmi pomáhalo" = c("10 = Velmi pomáhalo"),
    "Tuto strategii jsem nepraktikoval/a" = c("Tuto strategii jsem nepraktikoval/a"),
    "Nevím" = c("Nevím")
  )
}

data_s_upr_nQ61 <- data %>%
  mutate(across(matches("^nQ61_\\d+_1$"), recode_levels_3))

nQ61_battery = data_s_upr_nQ61 %>%
  select(starts_with("nQ61")) %>%
  filter(!is.na(nQ61_0_1)) %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "value") %>%
  count(item, value) %>%
  left_join(data_labelled %>% 
              select(item = variable, label) %>%
              mutate(label = str_extract(label, "\\[.*?\\]") %>% 
                       str_remove_all("\\[|\\]")), 
            by = "item") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n, na.rm = TRUE),
         pos_freq = sum(percent[value %in% c("Velmi pomáhalo")])) %>%
  ungroup() %>%
  mutate(
    percent_label = percent(percent, accuracy = 1, suffix = ""),
    percent_label = if_else(percent <= 0.01, "", percent_label),
    label = paste("...", label, sep = ""),
    label = fct_reorder(label, pos_freq),
    value = fct_rev(value)
  ) %>%
  ggplot(aes(x = percent, y = label, fill = value, label = percent_label)) +
  geom_col(color = "white") +
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(labels = ~str_wrap(., width = 40)) +
  scale_fill_manual(values = c(missing_color, "#7C1F28", rev(seq_pallet5))) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE, nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "top")+
  labs(title = "Pokud jste praktikoval/a následující strategie,
do jaké míry Vám to pomáhalo při krátkodobé abstinenci? ",
       fill = "",
       y = "",
       x = "")

ggsave(plot = nQ61_battery, filename = "nQ61-battery.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)
