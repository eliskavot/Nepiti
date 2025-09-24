

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

#vek: tQ89_0_0
#vzdelani: vzd4

data %>% 
  group_by(vzd4) %>% 
  summarise(m = mean(tQ89_0_0, na.rm = TRUE))

#spotreba: 

#přidat graf nQ52_r1


#Klarka: mela jsem napad pospojovat ty kategorie kdyby nam to nekde treba pomohlo
# hihi
#rekodovani promenne nQ51_r1 na nQ51_r1_3kat
table(data$nQ51_r1)

data$nQ51_r1 <- as.factor(data$nQ51_r1)

library(forcats)
data <- data %>%
  mutate(nQ51_r1_3kat = fct_collapse(nQ51_r1, "Nikdy jsem nezkusil/a" = c("Nikdy jsem nezkusil/a a neplánuji to zkusit", "Nikdy jsem nezkusil/a, ale plánuji to"),
                                     "Zkusil/a jsem jednou/vicekrat" = c("Jednou jsem zkusil/a", "Zkusil/a jsem vícekrát"),
                                     "Krátkodobě abstinuji jednou/vícekrát ročně" = c("Krátkodobě abstinuji jednou ročně", "Krátkodobě abstinuji vícekrát ročně"))) 
table(data$nQ51_r1_3kat)


#------------------------------ nQ51_r1_3kat x vzd4 --------------------------------#
#Klarka: tohle moc nefunguje ... ale pokus bzl

table(data$nQ51_r1_3kat)

#crosstab: vzdelani x kratkodoba abstinence
data %>%
  count(vzd4, nQ51_r1_3kat) %>%
  pivot_wider(
    names_from = nQ51_r1_3kat,
    values_from = n,
    values_fill = 0)

#radkova procenta
data %>%
  count(vzd4, nQ51_r1_3kat) %>%
  group_by(vzd4) %>% 
  mutate(row_percent = n/ sum(n) *100) %>%
  select(-n) %>% 
  pivot_wider(
    names_from = nQ51_r1_3kat,
    values_from = row_percent,
    values_fill = 0)

######graf bez CI
data %>% 
  count(nQ51_r1_3kat, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd4, y = perc, fill = nQ51_r1_3kat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "Abstinence > 3 týdny x vzdělání 4 kategorie",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))


#nQ52_r1 / graf -----------------------------------------------------------

# N = 397
unique(data$nQ52_r1)

data %>% 
  filter(!is.na(nQ52_r1)) %>%
  nrow() #N = 397

data %>% 
  filter(!is.na(nQ52_r1)) %>%
  count(nQ52_r1)

#graf
var_label(data$nQ52_r1)
plot_nQ52_r1 <- data %>% 
  filter(!is.na(nQ52_r1)) %>% 
  mutate(Q10_1 = factor(nQ52_r1,
                        levels = c("6", "5", "4","3","2","1"))) %>%
  count(nQ52_r1) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(x = 1, y = pct, fill = factor(nQ52_r1))) +
  geom_col(width = 0.05, position = "fill") +
  geom_text(aes(label = round(pct*100, 0)),
            position = position_fill(vjust = 0.5),
            color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = NULL) +
  scale_fill_manual(values = n6_pallet,
                    labels = c(
                      "1" = "Nikdy jsem nezkusil/a a neplánuji to zkusit",
                      "2" = "Nikdy jsem nezkusil/a, ale plánuji to",
                      "3" = "Jednou jsem zkusil/a",
                      "4" = "Zkusil/a jsem vícekrát",
                      "5" = "Tímto způsobem abstinuji jednou ročně",
                      "6" = "Tímto způsobem abstinuji jednou ročně")) +
  theme_minimal() +
  labs(
    x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 3, reverse = TRUE))

plot_nQ52_r1
ggsave(plot = plot_nQ52_r1, filename = "nQ52_r1.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 8, scaling = 1.2)
