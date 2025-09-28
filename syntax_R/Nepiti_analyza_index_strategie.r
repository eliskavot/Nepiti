packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled", "parameters", "performance",
              "scales", "ggthemes")
purrr::walk(packages, library, character.only = TRUE)


#vaše původní cesta k datům - já musím mít jinou
#data <- read_sav(file = "Data/Alkohol 2025_v02.sav") %>%    
#  mutate(across(where(is.labelled), as_factor))

data <- read_sav(file = "C:/Users/user/Desktop/Sociologie FF UK/Magistr/LS 2024,25/Výzkumný projekt - Alkohol/Nepiti/Nepiti/Data/Alkohol 2025_v02.sav") %>%    
  mutate(across(where(is.labelled), as_factor))

data_labelled <- generate_dictionary(data)

# NASTAVENÍ BAREVNÝCH PALET

basic_color = "#D9A939"
missing_color = "grey80"

seq_pallet5 = c("#FAF0D1", "#F0C661", "#D9A939", "#B57F22", "#855A13")
seq_pallet4 = c("#FAF0D1", "#F0C661", "#B57F22", "#855A13")
seq_pallet3 = c("#FAF0D1", "#F0C661", "#B57F22")
seq_pallet2 = seq_pallet3[-1]

n6_pallet = c(my_colors <- c("#A32A2F","#87485B","#F8E3DB","#91837D","#D9A939", "#37947E"))
n5_pallet = n6_pallet[-4] #n6 bez sedivy
n4_pallet = n5_pallet[-3] #n6 bez sedive a baby ruzove
n3_pallet = n4_pallet[-2]

#n6_pallet = c(my_colors <- c("#87485B","#A32A2F","#EB9352","#F8E3DB","#91837D", "#37947E"))         

##### tvorba veku se 4 kategoriemi 

data$vek4 <- cut(
  data$tQ89_0_0,
  breaks = c(17, 29, 49, 64, Inf),
  labels = c("18–29", "30–49", "50–64", "65+"),
  right = TRUE)

table(data$vek4)

##### prejmenovani kategorie ve vzd4 a v celkove spotrebe

table(data$vzd4)
data$vzd4 <- fct_recode(data$vzd4,
                        "VOŠ a VŠ" = "3 \"VOŠ, Bc. a VŠ\"")

table(data$celk_spotr_filtr_5kat)
data$celk_spotr_filtr_5kat <- fct_recode(data$celk_spotr_filtr_5kat,
                                         "0 - 0,5" = "0- 0,5")

table(data$vzd3)
data$vzd3 <- fct_recode(data$vzd3,
                        "VOŠ a VŠ" = "VOŠ, Bc. a VŠ")

# Tvorba indexu -----------------------------------------------------------
# z proměnných nQ61_0_1 až nQ61_10_1 (11 proměnných)

levels(data$nQ61_0_1)
levels(data$nQ51_r1)

#PODLE JAKÉ LOGIKY JSEM INDEX DĚLALA:
#pokud proměnná nQ61_X_1 == "Nevím", pak index_strategie = index_strategie
#ELSE:
#pokud proměnná != "Tuto strategii jsem nepraktikoval/a", pak index_stategie = index_strategie + 1

data = data %>%
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a a neplánuji to zkusit") %>%
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a, ale plánuji to") %>%
  rowwise() %>%
  mutate(
    index_strategie = sum(
      as.numeric(
        c_across(starts_with("nQ61")) %in% c("0 = Vůbec nepomáhalo", "1","2","3","4","5","6","7","8","9","10 = Velmi pomáhalo")),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

class(data$index_strategie)


# Analýzy index a délka abstinence ----------------------------------------

class(data$tQ54_0_0)

# předělávání na numerickou prom.
data = data %>% 
  filter(!is.na(tQ54_0_0)) %>% 
  mutate(tQ54_0_0_num = as.numeric(as.character(tQ54_0_0)))

#Výpočet Spearmanova korelačního koeficientu
cor.test(data$index_strategie, data$tQ54_0_0_num, method = "spearman")

# Analýzy index a (ne)dodržení plánované délky ----------------------------

ggplot(data, aes(x = nQ55_r1, y = index_strategie)) +
  geom_boxplot()


#index duvody
data = data %>%
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a a neplánuji to zkusit") %>%
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a, ale plánuji to") %>%
  select(-nQ58_0_1) %>%
  rowwise() %>%
  mutate(
    index_duvody = sum(
      as.numeric(
        c_across(starts_with("nQ58")) %in% c("6","7","8","9","10 = Naprosto zásadní")),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>% 
  select(index_duvody, starts_with("nQ58"))

hist(data$index_duvody)


#index prekázky
data = data %>%
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a a neplánuji to zkusit") %>%
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a, ale plánuji to") %>%
  rowwise() %>%
  mutate(
    index_duvody = sum(
      as.numeric(
        c_across(starts_with("nQ59")) %in% c("6","7","8","9","10 = Naprosto zásadní")),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>% 
  select(index_duvody, starts_with("nQ59"))

hist(data$index_duvody)
