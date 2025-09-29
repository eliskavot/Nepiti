

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
  group_by(vzd3) %>% 
  summarise(m = mean(tQ89_0_0, na.rm = TRUE))

data %>% 
  filter(vzd3 == 1) %>% 
  ggplot(aes(x = tQ89_0_0)) +
  geom_histogram()

data %>% 
  ggplot(aes(tQ89_0_0)) +
  geom_histogram() + 
  facet_wrap(~vzd3)


table(data$tQ89_0_0)

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

table(data$nQ79_r1)



##########spotreba x dodrzeni 

table <- table(data$nQ55_r1, data$celk_spotr_filtr_5kat)
table

round(prop.table(table, margin = 2) * 100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  filter(!is.na(nQ55_r1) & !is.na(celk_spotr_filtr_5kat)) %>% 
  nrow() #512

levels(data$nQ55_r1)
plot_nQ55_spotr <- data %>% 
  count(nQ55_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n)) %>% 
  mutate(nQ55_r1 = fct_rev(nQ55_r1)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(missing_color, n3_pallet),
                    labels = c(
                      "1" = "Ano, dodržel/a jsem ji v původně naplánované délce",
                      "2" = "Abstinoval/a jsem déle, než jsem si původně naplánoval/a",
                      "3" = "Ne, ukončil/a jsem ji dříve",
                      "4" = "Neměl/a jsem naplánovanou konkrétní délku")) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal") +
  guides(fill = guide_legend(nrow = 4, reverse = TRUE))

plot_nQ55_spotr
ggsave(plot = plot_nQ55_spotr, filename = "nQ55_r1 x celk_spotr_filtr_5kat.png.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 19, scaling = 1.4)

# tQ80_cat / vek prvni konzumace alkoholu ------------------------------------------

levels(data$tQ80_cat)

data %>%
  filter(!is.na(tQ80_cat)) %>%
  count(tQ80_cat) %>%
  mutate(perc = n / sum(n) * 100)


tQ80_cat <- data %>% 
  filter(!is.na(tQ80_cat)) %>%
  count(tQ80_cat) %>%
  mutate(perc = n / sum(n)) %>%
  mutate(tQ80_cat = fct_rev(tQ80_cat)) %>% 
  ggplot(aes(x = 1, y = perc, fill = tQ80_cat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet4)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

tQ80_cat
ggsave(plot = tQ80_cat, filename = "tQ82_kat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 8, scaling = 1.4)


# graf: tQ54_0_0_kat2 - delka kratkodob abst kategorizovane --------------------------

levels(data$tQ54_0_0_kat2)


tQ54_0_0_kat2 <- data %>% 
  filter(!is.na(tQ54_0_0_kat2)) %>%
  count(tQ54_0_0_kat2) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = 1, y = perc, fill = tQ54_0_0_kat2)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet4)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

tQ54_0_0_kat2
ggsave(plot = tQ54_0_0_kat2, filename = "tQ54_0_0_kat2.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 8, scaling = 1.4)


# tQ54_0_0_kat2_dodrzeni / delka abstinence X dodrzeni --------------------------------------------------

label_attribute(data$tQ54_0_0)
tQ54_0_0_kat2_dodrzeni <- data %>% 
  filter(!is.na(tQ54_0_0_kat2) & !is.na(nQ55_r1)) %>%
  count(tQ54_0_0_kat2, nQ55_r1) %>% 
  mutate(perc = n / sum(n)) %>% 
  mutate(nQ55_r1 = fct_rev(nQ55_r1)) %>% 
  ggplot(aes(x = tQ54_0_0_kat2, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(missing_color, n3_pallet)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(nrow = 4, reverse = TRUE))

tQ54_0_0_kat2_dodrzeni
ggsave(plot = tQ54_0_0_kat2_dodrzeni, filename = "tQ54_0_0_kat2 x dodrzeni abstinence.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 18, scaling = 1.4)


# omexXspotreba / index omezování a množství konzumace,  ----------------------------------

table(data$omez_sum_cat)
table(data$celk_spotr_filtr_5kat)

table(data$tQ54_0_0_kat2)
data %>% 
  filter(!is.na(omez_sum_cat) & !is.na(celk_spotr_filtr_5kat)) %>% 
  nrow() #929

omexXspotreba <- data %>% 
  filter(!is.na(celk_spotr_filtr_5kat) & !is.na(omez_sum_cat)) %>%
  count(celk_spotr_filtr_5kat, omez_sum_cat) %>%
  group_by(celk_spotr_filtr_5kat) %>% 
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(omez_sum_cat = fct_relevel(omez_sum_cat, "Žádné",
                                                      "1-2 způsoby",
                                                      "3 a více způsobů")) %>% 
  mutate(omez_sum_cat = fct_rev(omez_sum_cat)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = omez_sum_cat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 5, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet3)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

omexXspotreba
ggsave(plot = omexXspotreba, filename = "omez_sum_cat x celk_spotr_filtr_5kat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)



# konzumace po kr abstinenci ----------------------------------------------
#N = 695


levels(data$nQ63_r1)
data %>%
  filter(!is.na(nQ63_r1)) %>%
  nrow()
  

nQ63_r1 <- data %>% 
  filter(!is.na(nQ63_r1)) %>%
  count(nQ63_r1) %>% 
  mutate(perc = n / sum(n)) %>% 
  mutate(nQ63_r1 = fct_relevel(nQ63_r1, 
                               "Trvale snížil/a spotřebu alkoholu",
                               "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
                               "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
                               "Pil/a alkohol přibližně stejně jako před ní",
                               "Pil/a alkohol více než před ní")) %>% 
  mutate(nQ63_r1 = fct_rev(nQ63_r1)) %>% 
  ggplot(aes(x = 1, y = perc, fill = nQ63_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n5_pallet) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(nrow = 5, reverse = TRUE))

nQ63_r1
ggsave(plot = nQ63_r1, filename = "nQ63_r1_v2.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 10, scaling = 1.2)


# index omezovani ---------------------------------------------------------
# N = 1022

table(data$omez_sum_cat)
data %>%
  filter(!is.na(omez_sum_cat)) %>%
  nrow()


omez_sum_cat <- data %>% 
  filter(!is.na(omez_sum_cat)) %>%
  count(omez_sum_cat) %>% 
  mutate(perc = n / sum(n)) %>% 
  mutate(omez_sum_cat = fct_relevel(omez_sum_cat, 
                               "Žádné",
                               "1-2 způsoby",
                               "3 a více způsobů")) %>% 
  mutate(omez_sum_cat = fct_rev(omez_sum_cat)) %>% 
  ggplot(aes(x = 1, y = perc, fill = omez_sum_cat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet3)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

omez_sum_cat
ggsave(plot = omez_sum_cat, filename = "omez_sum_cat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 6, scaling = 1.2)


var_label(data$nQ77_r1)

data %>% 
  filter(!is.na(nQ79_r1) & !is.na(nQ77_r1)) %>% 
  count(nQ79_r1)


# kratkodoba abs X omez_sum_cat -------------------------------------------

levels(data$nQ51_r1)
levels(data$omez_sum_cat)

nQ51_r1xomez_sum_cat

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(omez_sum_cat)) %>%
  count(nQ51_r1, omez_sum_cat) %>%
  group_by(nQ51_r1) %>% 
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(omez_sum_cat = fct_relevel(omez_sum_cat, "Žádné",
                                    "1-2 způsoby",
                                    "3 a více způsobů")) %>% 
  mutate(omez_sum_cat = fct_rev(omez_sum_cat)) %>% 
  ggplot(aes(x = nQ51_r1, y = perc, fill = omez_sum_cat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 5, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet3)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

nQ51_r1xomez_sum_cat
ggsave(plot = nQ51_r1xomez_sum_cat, filename = "nQ51_r1 x omez_sum_cat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(omez_sum_cat)) %>%
  count(nQ51_r1)

tab = table(data$nQ51_r1, data$omez_sum_cat)
round(prop.table(tab, margin = 2)*100, 0)  #sloupcova procenta (radkova by byl margin = 1)


