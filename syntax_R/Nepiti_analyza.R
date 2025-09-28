
packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled", "parameters", "performance",
              "scales", "ggthemes")
purrr::walk(packages, library, character.only = TRUE)

data <- read_sav(file = "Data/Alkohol 2025_v02.sav") %>%    
  mutate(across(where(is.labelled), as_factor))

data_labelled <- generate_dictionary(data)

# NASTAVENÍ BAREVNÝCH PALET

basic_color = "#D9A939"
missing_color = "grey80"

seq_pallet5 = c("#FAF0D1", "#F0C661", "#D9A939", "#B57F22", "#855A13")
seq_pallet4 = c("#FAF0D1", "#F0C661", "#B57F22", "#855A13")
seq_pallet3 = c("#FAF0D1", "#F0C661", "#B57F22")
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

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Popis souboru / dat
# N = 1022
nrow(data)

# pohlavi: M = 525, Z = 497
data %>% 
  count(	
    nQ88_r1) %>% 
  mutate(pct = n/sum(n)*100)

# vek 
describe(data$tQ89_0_0)
data %>% 
  group_by(nQ88_r1) %>% 
  summarise(mean = mean(tQ89_0_0)) 

# vzdelani: vzd4
data %>% 
  count(vzd5) %>% 
  mutate(pct = n/sum(n)*100)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Jak rozšířená je krátkodobá  abstinence v české společnosti? ------------

#--------------------------------nQ51_r1--------------------------------------#
# Jake mate zkusenosti s kratkodobou abstinenci
#jednoduche trideni

table(data$nQ51_r1) 

data %>%
  filter(!is.na(nQ51_r1)) %>%
  count(nQ51_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ51_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  

vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Nikdy jsem nezkusil/a a neplánuji to zkusit",
                                              "Nikdy jsem nezkusil/a, ale plánuji to",
                                              "Jednou jsem zkusil/a",
                                              "Zkusil/a jsem vícekrát",
                                              "Krátkodobě abstinuji jednou ročně",
                                              "Krátkodobě abstinuji vícekrát ročně")))
levels(data$nQ51_r1)

nQ51_r1 = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
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

nQ51_r1
ggsave(plot = nQ51_r1, filename = "nQ51.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


# Klarka: me prijde ze by tady ty odpovedi nemeli byt razene od nejvice procent do nejmene, 
## ale tak jak jsme je meli v dotazniku... --> 
vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Nikdy jsem nezkusil/a a neplánuji to zkusit",
                                                "Nikdy jsem nezkusil/a, ale plánuji to",
                                                "Jednou jsem zkusil/a",
                                                "Zkusil/a jsem vícekrát",
                                                "Krátkodobě abstinuji jednou ročně",
                                                "Krátkodobě abstinuji vícekrát ročně"))) %>%
  arrange(Odpoved)

#--------------------------------nQ51_r2--------------------------------------#
# A zkusil/a jste někdy záměrně abstinovat na dobu kratší než 3 týdny? 
#nQ52_r1 / graf

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

#--------------------------------tQ54_0_0--------------------------------------#
#-> medián, min, max, průměr otázky tQ54_0_0 (Jak dlouho trvala/trvá Vaše
#poslední krátkodobá abstinence? (v týdnech))

#???? Klara: heledte tahle promenna, je hodne sikma... si trochu rikam, jestli
##            by nebylo od veci, ta odlehla pozorovani nejak poresit, pac nam 
##            to docela zkresluje aj ten graf ne? - a prumer nam to hoodne 
##            posouva, takze bychom meli pouzivat median pak?

## M: co to zkusit kategorizovat tedy?
## Klarka: jj to je dobry! jen tech 5 nejvzssich hodnot mi ptoste prijde porad podezrelych...
##      ptame se na posledni abstinenci a u jednoto trvala 13 let? to je bizar
## 5 nejvyssich pripadu
data %>% 
  slice_max(n = 5, tQ54_0_0_num) %>% 
  select(tQ54_0_0_num)

# zobrazeni promenne
table(data$tQ54_0_0)
data$tQ54_0_0[data$tQ54_0_0 == "Nevím, nedokážu spočítat"] <- NA 
data$tQ54_0_0_num <- as.numeric(as.character(data$tQ54_0_0))


describe(data$tQ54_0_0_num, quant=c(.25,.75)) 
#  vars   n  mean    sd median trimmed  mad min max range  skew kurtosis   se Q0.25 Q0.75
#1    1 489 16.12 50.07      6    8.88 4.45   1 670   669 10.35   118.35 2.26     4    12

# kontrola medianu pro lidi co nekdy zkusili kratkodobou abstinenci delsi ney 3 tydny
table(data$nQ51_r1)
data %>% 
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a a neplánuji to zkusit ") %>% 
  filter(nQ51_r1 != "Nikdy jsem nezkusil/a, ale plánuji to") %>% 
  filter(!is.na(tQ54_0_0)) %>%
  summarise(med = median(tQ54_0_0_num, na.rm = TRUE), 
            min = min(tQ54_0_0_num, na.rm = TRUE),
            mod = names(sort(table(tQ54_0_0), decreasing = TRUE))[1])

table(data$nQ51_r1)
data %>% 
  filter(!is.na(tQ54_0_0)) %>%
  summarise(med = median(tQ54_0_0_num, na.rm = TRUE), 
            min = min(tQ54_0_0_num, na.rm = TRUE),
            mod = names(sort(table(tQ54_0_0), decreasing = TRUE))[1])

##CI pro prumer 
mean_val <- mean(data$tQ54_0_0_num, na.rm = TRUE)
se_val <- sd(data$tQ54_0_0_num, na.rm = TRUE) / sqrt(sum(!is.na(data$tQ54_0_0_num))) #sd/sqrt(n)
ci_lower <- mean_val - 1.96 * se_val
ci_upper <- mean_val + 1.96 * se_val
cat("95% CI pro prumer:", round(ci_lower, 2), "-", round(ci_upper, 2)) #11,7 - 20,6

# median pro muze x zeny
data %>% 
  group_by(nQ88_r1) %>% 
  summarise(MED = median(tQ54_0_0_num,  na.rm = TRUE))

# histogram puvodni numericke promenne
data %>%
  ggplot(aes(x = tQ54_0_0_num)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15, fill = "#D9A939", color = "white", alpha = 0.9) +
  geom_density(color = "black", size = 0.4) +
  theme_minimal() +
  labs(title = "Doba abstinence (v týdnech)", x = "Týdny", y = "",
       subtitle = paste("N: 489",
                        "| Průměr:", round(mean(data$tQ54_0_0_num, na.rm=TRUE), 1),
                        "| Median:", median(data$tQ54_0_0_num, na.rm=TRUE),
                        "| SD:", round(sd(data$tQ54_0_0_num, na.rm=TRUE), 1))) +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_log10()

data %>% 
  ggplot(mapping = aes(x = tQ54_0_0_num))+
  geom_histogram(bins = 15,fill = "#D9A939", color = "white", alpha = 0.9)+
  theme_minimal()+
  labs(title = "Doba abstinence (v týdnech)", x = "Týdny", y = "N",
       subtitle = paste("N: 489",
                        "| Průměr:", round(mean(data$tQ54_0_0_num, na.rm=TRUE), 1),
                        "| Median:", median(data$tQ54_0_0_num, na.rm=TRUE),
                        "| SD:", round(sd(data$tQ54_0_0_num, na.rm=TRUE), 1)))+
  theme(plot.title = element_text(face = "bold"))+
  scale_x_log10()

##s prumerem + CI
data %>% 
  ggplot(aes(x = tQ54_0_0_num)) +
  geom_histogram(bins = 15, fill = "#D9A939", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean_val, color = "darkred", linetype = "solid", size = 1, alpha = 0.5) +
  geom_vline(xintercept = ci_lower, color = "darkred", linetype = "dashed", size = 0.8, alpha = 0.5) +
  geom_vline(xintercept = ci_upper, color = "darkred", linetype = "dashed", size = 0.8, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Doba abstinence (v týdnech)",
    x = "Týdny", y = "N",
    subtitle = paste("N: 489",
                     "| Průměr:", round(mean_val, 1),
                     "| 95% CI pro průměr:", round(ci_lower, 1), "-", round(ci_upper, 1),
                     "| Median:", median(data$tQ54_0_0_num, na.rm = TRUE),
                     "| SD:", round(sd(data$tQ54_0_0_num, na.rm = TRUE), 1))
  ) +
  theme(plot.title = element_text(face = "bold")) +
  scale_x_log10()


##navrh kategorizace

char_values <- as.character(data$tQ54_0_0)

numeric_values <- suppressWarnings(as.numeric(char_values))

tQ54_0_0_kat <- cut(numeric_values,
                    breaks = c(-Inf, 3, 7, 23, Inf),
                    labels = c("Méně než jeden měsíc",
                               "<1–2) měsíce",
                               "<2–6) měsíců",
                               "Půl roku a více"))

tQ54_0_0_kat <- as.character(tQ54_0_0_kat)

tQ54_0_0_kat[char_values == "Nevím, nedokážu spočítat"] <- "Nevím, nedokážu spočítat"

tQ54_0_0_kat[is.na(tQ54_0_0_kat)] <- NA

data$tQ54_0_0_kat <- factor(tQ54_0_0_kat,
                            levels = c("Méně než jeden měsíc",
                                       "<1–2) měsíce",
                                       "<2–6) měsíců",
                                       "Půl roku a více",
                                       "Nevím, nedokážu spočítat"))
# zobrazeni promenne
tabulka <- table(data$tQ54_0_0_kat)
sum(tabulka)

# graf
## % + CI
data %>%
  filter(!is.na(tQ54_0_0_kat)) %>%
  count(tQ54_0_0_kat) %>%
  mutate(perc = n / sum(n) * 100)

tabulka <- table(data$tQ54_0_0_kat)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  

vysledky

vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Nevím, nedokážu spočítat",
                                              "Méně než jeden měsíc",
                                              "<1–2) měsíce",
                                              "<2–6) měsíců",
                                              "Půl roku a více")))


#graf: doba trvani posledni kratkodobe abstinence (N=489)
tQ54_0_0_cat = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 35)) +
  labs(title = "Jak dlouho trvala/trvá poslední abstinence", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8)) +
  coord_flip() 

# zobrazeni a ulozeni grafu
tQ54_0_0_cat
ggsave(plot = tQ54_0_0_cat, filename = "tQ54_0_0_cat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)



# kategorizace 2 ukonceni posledni abstinence -------------------------------

# navrh nove kategorizace 2
data %>% 
  count(tQ54_0_0 == 4)

# tQ54_0_0 -> N = 695
describe(data$tQ54_0_0)
class(data$tQ54_0_0)
table(data$tQ54_0_0)

#[tQ54_0_0_hovno_kat] --> N = 695
data <- data %>% 
  mutate(tQ54_0_0_hovno = fct_recode(tQ54_0_0, "0" = "Nevím, nedokážu spočítat")) %>% 
  mutate(tQ54_0_0_hovno = as.numeric(as.character(tQ54_0_0_hovno))) %>% 
  mutate(tQ54_0_0_hovno_kat = case_when(
    tQ54_0_0_hovno == 0 ~ "Nevím, nedokážu spočítat",
    tQ54_0_0_hovno >= 1 & tQ54_0_0_hovno <= 3 ~ "1-3 týdny (méně než 1 měsíc)",
    tQ54_0_0_hovno == 4 ~ "4 týdny (1 měsíc)",
    tQ54_0_0_hovno >= 5 & tQ54_0_0_hovno <= 12 ~ "5-12 týdnů",
    tQ54_0_0_hovno >= 13 ~ "Více než 12 týdnů (více než čtvrt roku)",
    TRUE ~ NA_character_ ),
    tQ54_0_0_hovno_kat = factor(tQ54_0_0_hovno_kat, 
                                levels = c("Nevím, nedokážu spočítat",
                                           "1-3 týdny (méně než 1 měsíc)",
                                           "4 týdny (1 měsíc)",
                                           "5-12 týdnů",
                                           "Více než 12 týdnů (více než čtvrt roku)")))

tabulka = table(data$tQ54_0_0_hovno_kat)
sum(tabulka)


# graf: tQ54_0_0_kat2 - delka kratkodob abst kat --------------------------

levels(data$tQ54_0_0_hovno_kat)
table(data$tQ54_0_0_hovno_kat)


tQ54_0_0_hovno_kat <- data %>% 
  filter(!is.na(tQ54_0_0_hovno_kat)) %>%
  count(tQ54_0_0_hovno_kat) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = 1, y = perc, fill = tQ54_0_0_hovno_kat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet5)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

tQ54_0_0_hovno_kat
ggsave(plot = tQ54_0_0_hovno_kat, filename = "tQ54_0_0_kat2.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 8, scaling = 1.4)

# graf tQ54_0_0_hovno_kat_dodrzeni - doba posledni abstinece X dodrzeni posledni --------

#N = 547
tabulka <- table(data$tQ54_0_0_hovno_kat, data$nQ55_r1)
sum(tabulka)

tQ54_0_0_hovno_kat_dodrzeni <- data %>% 
  filter(!is.na(tQ54_0_0_hovno_kat) & !is.na(nQ55_r1)) %>%
  group_by(tQ54_0_0_hovno_kat) %>% 
  count(nQ55_r1) %>% 
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(nQ55_r1 = fct_rev(nQ55_r1)) %>% 
  ggplot(aes(x = tQ54_0_0_hovno_kat, y = perc, fill = nQ55_r1)) + 
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

tQ54_0_0_hovno_kat_dodrzeni
ggsave(plot = tQ54_0_0_hovno_kat_dodrzeni, filename = "tQ54_0_0_hovno_kat_dodrzeni x dodrzeni abstinence.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 18, scaling = 1.4)

#---------------------------nQ55_r1 + nQ56_r1---------------------------------#

#-> rozložení odpovědí otázky nQ55_r1 (Dodržel/a jste poslední krátkodobou
#abstinenci v původně naplánované délce?) + nQ56_r1 (Abstinujete v původně
#naplánované délce?) -> Je třeba přepočítat na absolutní čísla, spojit odpovědi
#a následně znovu přepočítat na procenta

table(data$nQ55_r1)
table(data$nQ56_r1)

unified_levels <- c("Ano, dodržel/a jsem ji v původně naplánované délce",
                    "Abstinoval/a jsem déle, než jsem si původně naplánoval/a",
                    "Ne, ukončil/a jsem ji dříve",
                    "Neměl/a jsem naplánovanou konkrétní délku")

#Ano -> Ano, dodržel/a jsem ji v původně naplánované délce
#Ne, abstinuji déle, než jsem si naplánoval/a -> Abstinoval/a jsem déle, než jsem si původně naplánoval/a
#Nemám naplánovanou konkrétní délku -> Neměl/a jsem naplánovanou konkrétní délku

data <- data %>%
  mutate(recoded_nQ55_r1 = case_when(
    nQ55_r1 == "Ano, dodržel/a jsem ji v původně naplánované délce" ~ unified_levels[1],
    nQ55_r1 == "Abstinoval/a jsem déle, než jsem si původně naplánoval/a" ~ unified_levels[2],
    nQ55_r1 == "Ne, ukončil/a jsem ji dříve" ~ unified_levels[3],
    nQ55_r1 == "Neměl/a jsem naplánovanou konkrétní délku" ~ unified_levels[4],
    TRUE ~ NA_character_
  ),
  recoded_nQ56_r1 = case_when(
    nQ56_r1 == "Ano" ~ unified_levels[1],
    nQ56_r1 == "Ne, abstinuji déle, než jsem si naplánoval/a" ~ unified_levels[2],
    nQ56_r1 == "Nemám naplánovanou konkrétní délku" ~ unified_levels[4], 
    TRUE ~ NA_character_
  ),
  nQ55_56_r1 = coalesce(recoded_nQ55_r1, recoded_nQ56_r1),
  nQ55_56_r1 = factor(nQ55_56_r1, levels = unified_levels)
  ) %>%
  select(-recoded_nQ55_r1, -recoded_nQ56_r1)

##spojena delka abstinence
table(data$nQ55_56_r1) 

data %>%
  filter(!is.na(nQ55_56_r1)) %>%
  count(nQ55_56_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ55_56_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Neměl/a jsem naplánovanou konkrétní délku",
                                              "Ne, ukončil/a jsem ji dříve",
                                              "Ano, dodržel/a jsem ji v původně naplánované délce",
                                              "Abstinoval/a jsem déle, než jsem si původně naplánoval/a")))

nQ55_56 = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 50)) +
  labs(title = "Dodržení délky abstinence", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()

ggsave(plot = nQ55_56, filename = "nQ55 + 56.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)

# V jakých částech společnosti je krátkodobá/dlouhodobá abstinence --------

#-> otázku nQ51_r1 protřídit demografiky
#(vzd4/vzd3, prijem_osob, vek5 - ten je ještě potřeba sloučit na méně kategorií
#případně udělat novou proměnnou z tQ89_0_0, nQ88_r1 (pohlaví), 
#celk_spotr_filtr_5kat (celková spotřeba) ) 




#------------------------------ nQ51_r1 x vzd4 --------------------------------#

##### N
data %>% 
  filter(!is.na(nQ51_r1) & !is.na(vzd4)) %>% 
  nrow() # N = 1022

##### tabulka s N a %
tabulka <- table(data$nQ51_r1, data$vzd4)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = 0,09 
##lidé s nejvyšším vzděláním častěji nikdy nezkusili a ani neplánují 



#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ51_r1), !is.na(vzd4)) %>%
  count(vzd4, nQ51_r1) %>%
  group_by(vzd4) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

ggplot(ci_data, aes(x = vzd4, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(6)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Zkušenost s krátkodobou abstinencí dle vzdělání",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


######graf bez CI
nQ51_r1_vzd4 = data %>% 
  count(nQ51_r1, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd4, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n6_pallet) +
  labs(x = "", y = "", fill = "", 
       title = "Zkušenost s krátkodobou abstinencí dle vzdělání",
       subtitle = "N = 1022") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

ggsave(plot = nQ51_r1_vzd4, filename = "nQ51_r1 x vzd4.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


#------------------------------ nQ51_r1 x vzd3 --------------------------------#

table(data$vzd3)

##### N
data %>% 
  filter(!is.na(nQ51_r1) & !is.na(vzd3)) %>% 
  nrow() # N = 1022

##### tabulka s N a %
tabulka <- table(data$nQ51_r1, data$vzd3)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = 0,18
##lidé s nejvyšším vzděláním častěji nikdy nezkusili a ani neplánují 

#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ51_r1), !is.na(vzd3)) %>%
  count(vzd3, nQ51_r1) %>%
  group_by(vzd3) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

ggplot(ci_data, aes(x = vzd3, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.9), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    x = "", y = "",
    fill = "",
    title = "Abstinence > 3 týdny x vzdělání 3 kategorie",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

######graf bez CI
nQ51_r1_vzd3 <- data %>% 
  count(nQ51_r1, vzd3) %>% 
  na.omit() %>%
  group_by(vzd3) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd3, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = n6_pallet) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "", fill = "")+
  theme(legend.position = "top",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(ncol = 2, reverse = TRUE, byrow = TRUE))

nQ51_r1_vzd3
ggsave(plot = nQ51_r1_vzd3, filename = "nQ51_r1 x vzd3.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1.4)

#------------------------------ nQ51_r1 x prijem_osob --------------------------------#


table(data$prijem_osob)


##### N
data %>% 
  filter(!is.na(nQ51_r1) & !is.na(prijem_osob)) %>% 
  nrow() # N = 747

##### tabulka s N a %
tabulka <- table(data$nQ51_r1, data$prijem_osob)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = 0,013
##prumery prijem casteji nikdy nezkusil a neplanuje 
##nejnizsi casteji planuje zkusit 



#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ51_r1), !is.na(prijem_osob)) %>%
  count(prijem_osob, nQ51_r1) %>%
  group_by(prijem_osob) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

 ggplot(ci_data, aes(x = prijem_osob, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(6)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Zkušenost s krátkodobou abstinencí dle příjmu",
    subtitle = "N = 747"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


######graf bez CI
 nQ51_r1_prijem = data %>% 
  count(nQ51_r1, prijem_osob) %>% 
  na.omit() %>%
  group_by(prijem_osob) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = prijem_osob, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n6_pallet) +
  labs(x = "", y = "", fill = "", 
       title = "Zkušenost s krátkodobou abstinencí dle příjmu",
       subtitle = "N = 1022") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

ggsave(plot = nQ51_r1_prijem, filename = "nQ51_r1 x prijem_osob.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)




#------------------------------ nQ51_r1 x celk_spotr_filtr_5kat --------------------------------#

table(data$celk_spotr_filtr_5kat)


##### N
data %>% 
  filter(!is.na(nQ51_r1) & !is.na(celk_spotr_filtr_5kat)) %>% 
  nrow() # N = 929

##### tabulka s N a %
tabulka <- table(data$nQ51_r1, data$celk_spotr_filtr_5kat)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = 0,006
## 10+ sklenic casteji zkusili jednou nebo vicekrat, mene casto abstinuji vicekrat rocne
## 1 - 2 sklenice casteji abstinuji vicekrat rocne, mene casto jednou rocne




#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ51_r1), !is.na(celk_spotr_filtr_5kat)) %>%
  count(celk_spotr_filtr_5kat, nQ51_r1) %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

 ggplot(ci_data, aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(6)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Zkušenost s krátkodobou abstinencí dle počtu sklenic alkholu (za týden)",
    subtitle = "N = 929"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
 nQ51_r1_spotr = data %>% 
  count(nQ51_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n6_pallet) +
  labs(x = "", y = "", fill = "", 
       title = "Zkušenost s krátkodobou abstinencí dle počtu sklenic alkoholu (za týden)",
       subtitle = "N = 929") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

ggsave(plot = nQ51_r1_spotr, filename = "nQ51_r1 x celk_spotr.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


#------------------------------ nQ51_r1 x pohlavi --------------------------------#

table(data$nQ88_r1)


##### N
data %>% 
  filter(!is.na(nQ51_r1) & !is.na(nQ88_r1)) %>% 
  nrow() # N = 1022

##### tabulka s N a %
tabulka <- table(data$nQ51_r1, data$nQ88_r1)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = 0,036
## zeny casteji oproti muzum kratkodobe abstinuji vicekrat rocne
##muzi casteji zkusili jednou 




#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ51_r1), !is.na(nQ88_r1)) %>%
  count(nQ88_r1, nQ51_r1) %>%
  group_by(nQ88_r1) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

 ggplot(ci_data, aes(x = nQ88_r1, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(6)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Zkušenost s krátkodobou abstinencí dle pohlaví",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
 nQ51_r1_pohlavi = data %>% 
  count(nQ51_r1, nQ88_r1) %>% 
  na.omit() %>%
  group_by(nQ88_r1) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = nQ88_r1, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n6_pallet) +
  labs(x = "", y = "", fill = "", 
       title = "Zkušenost s krátkodobou abstinencí dle pohlaví",
       subtitle = "N = 1022") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

ggsave(plot = nQ51_r1_pohlavi, filename = "nQ51_r1 x pohlavi.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


#------------------------------ nQ51_r1 x vek --------------------------------#



##### tvorba veku se 4 kategoriemi 

table(data$vek5)
describe(data$tQ89_0_0)

data$vek4 <- cut(
  data$tQ89_0_0,
  breaks = c(17, 29, 49, 64, Inf),
  labels = c("18–29", "30–49", "50–64", "65+"),
  right = TRUE
)

table(data$vek4)
sum(!is.na(data$vek4))



##### N
data %>% 
  filter(!is.na(nQ51_r1) & !is.na(vek4)) %>% 
  nrow() # N = 1022

##### tabulka s N a %
tabulka <- table(data$nQ51_r1, data$vek4)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = <0,001





#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ51_r1), !is.na(vek4)) %>%
  count(vek4, nQ51_r1) %>%
  group_by(vek4) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

ggplot(ci_data, aes(x = vek4, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(6)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Zkušenost s krátkodobou abstinencí dle věku",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


######graf bez CI
nQ51_r1_vek4 = data %>% 
  count(nQ51_r1, vek4) %>% 
  na.omit() %>%
  group_by(vek4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vek4, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 4, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n6_pallet) +
  labs(x = "", y = "", fill = "", 
       title = "Zkušenost s krátkodobou abstinencí dle věku",
       subtitle = "N = 1022") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

ggsave(plot = nQ51_r1_vek4, filename = "nQ51_r1 x vek4.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


# Má krátkodobá abstinence efekt na snižování míry konzumace alkoh --------

#--------------------------------nQ63_r1--------------------------------------#

table(data$nQ63_r1) 

data %>%
  filter(!is.na(nQ63_r1)) %>%
  count(nQ63_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ63_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  

vysledky


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(c(
    "Trvale snížil/a spotřebu alkoholu",
    "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
    "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
    "Pil/a alkohol přibližně stejně jako před ní",
    "Pil/a alkohol více než před ní"
  ))))

vysledky$x <- factor(1) 


nQ63_r1 = ggplot(vysledky, aes(x = x, y = Podil, fill = Odpoved)) +
  geom_col(width = 0.4) +  
  geom_text(aes(label = paste0(round(Podil*100))),
            position = position_stack(vjust = 0.5),
            size = 5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n5_pallet) +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12)) +
  guides(fill = guide_legend(nrow = 5, reverse = TRUE))


ggsave(plot = nQ63_r1, filename = "nQ63_r1.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


#----------------------------nQ63_r1 x vzd4----------------------------------#

##### tabulka s N a %
tabulka <- table(data$nQ63_r1, data$vzd4)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = <0,001





#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ63_r1), !is.na(vzd4)) %>%
  count(vzd4, nQ63_r1) %>%
  group_by(vzd4) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

ggplot(ci_data, aes(x = vzd4, y = perc, fill = nQ63_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(5)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Pití po krátkodobé abstinenci dle vzdělání",
    subtitle = paste0("N = ", sum(ci_data$n))
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(nrow = 2))


######graf bez CI
nQ63_r1_vzd4 = data %>% 
  mutate(nQ63_r1 = factor(nQ63_r1, levels = c(
    "Pil/a alkohol více než před ní",
    "Pil/a alkohol přibližně stejně jako před ní",
    "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
    "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
    "Trvale snížil/a spotřebu alkoholu"
  ))) %>%
  count(nQ63_r1, vzd5) %>% 
  na.omit() %>%
  group_by(vzd5) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd5, y = perc, fill = nQ63_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 6, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n5_pallet) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 5, reverse = TRUE))

ggsave(plot = nQ63_r1_vzd4, filename = "nQ63_r1 x vzd4.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)


#----------------------------nQ63_r1 x prijem_osob----------------------------------#

##### tabulka s N a %
tabulka <- table(data$nQ63_r1, data$prijem_osob)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = <0,001





#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ63_r1), !is.na(prijem_osob)) %>%
  count(prijem_osob, nQ63_r1) %>%
  group_by(prijem_osob) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

 ggplot(ci_data, aes(x = prijem_osob, y = perc, fill = nQ63_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(5)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Pití po krátkodobé abstinenci dle příjmu",
    subtitle = paste0("N = ", n)
  ) +
  theme_minimal() +
  theme(legend.position = "top",
      legend.box = "horizontal",
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))



######graf bez CI
 nQ63_r1_prijem =data %>% 
   mutate(nQ63_r1 = factor(nQ63_r1, levels = c(
     "Pil/a alkohol více než před ní",
     "Pil/a alkohol přibližně stejně jako před ní",
     "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
     "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
     "Trvale snížil/a spotřebu alkoholu"
   ))) %>%
   count(nQ63_r1, prijem_osob) %>% 
   na.omit() %>%
   group_by(prijem_osob) %>%
   mutate(perc = n / sum(n)) %>% 
   ggplot(aes(x = prijem_osob, y = perc, fill = nQ63_r1)) + 
   geom_col(position = "fill") +
   geom_text(aes(label = round(perc*100, 0)), 
             position = position_fill(vjust = 0.5), 
             size = 6, color = "black") +
   theme_minimal() +
   coord_flip() +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_fill_manual(values = n5_pallet) +
   labs(x = "", y = "", fill = "") +
   theme(legend.position = "top",
         legend.box = "horizontal",
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14),
         legend.text = element_text(size = 14),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank()) +
   guides(fill = guide_legend(nrow = 5, reverse = TRUE))

ggsave(plot = nQ63_r1_prijem, filename = "nQ63_r1 x prijem_osob.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)

#----------------------------nQ63_r1 x vek4----------------------------------#

##### tabulka s N a %
tabulka <- table(data$nQ63_r1, data$vek4)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = <0,001





#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ63_r1), !is.na(vek4)) %>%
  count(vek4, nQ63_r1) %>%
  group_by(vek4) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

 ggplot(ci_data, aes(x = vek4, y = perc, fill = nQ63_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(5)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Pití po krátkodobé abstinenci dle věku",
    subtitle = paste0("N = ", n)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  guides(fill = guide_legend(nrow = 2))


######graf bez CI
 nQ63_r1_vek4 = data %>% 
   mutate(nQ63_r1 = factor(nQ63_r1, levels = c(
     "Pil/a alkohol více než před ní",
     "Pil/a alkohol přibližně stejně jako před ní",
     "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
     "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
     "Trvale snížil/a spotřebu alkoholu"
   ))) %>%
   count(nQ63_r1, vek4) %>% 
   na.omit() %>%
   group_by(vek4) %>%
   mutate(perc = n / sum(n)) %>% 
   ggplot(aes(x = vek4, y = perc, fill = nQ63_r1)) + 
   geom_col(position = "fill") +
   geom_text(aes(label = round(perc*100, 0)), 
             position = position_fill(vjust = 0.5), 
             size = 6, color = "black") +
   theme_minimal() +
   coord_flip() +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_fill_manual(values = n5_pallet) +
   labs(x = "", y = "", fill = "") +
   theme(legend.position = "top",
         legend.box = "horizontal",
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14),
         legend.text = element_text(size = 14),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank()) +
   guides(fill = guide_legend(nrow = 5, reverse = TRUE))

ggsave(plot = nQ63_r1_vek4, filename = "nQ63_r1 x vek4.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)

#----------------------------nQ63_r1 x pohlavi----------------------------------#

##### tabulka s N a %
tabulka <- table(data$nQ63_r1, data$nQ88_r1)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = <0,001





#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ63_r1), !is.na(nQ88_r1)) %>%
  count(nQ88_r1, nQ63_r1) %>%
  group_by(nQ88_r1) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

 ggplot(ci_data, aes(x = nQ88_r1, y = perc, fill = nQ63_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(5)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Pití po krátkodobé abstinenci dle pohlaví",
    subtitle = paste0("N = ", n)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  guides(fill = guide_legend(nrow = 2))


######graf bez CI
 nQ63_r1_pohlavi = data %>% 
   mutate(nQ63_r1 = factor(nQ63_r1, levels = c(
     "Pil/a alkohol více než před ní",
     "Pil/a alkohol přibližně stejně jako před ní",
     "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
     "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
     "Trvale snížil/a spotřebu alkoholu"
   ))) %>%
   count(nQ63_r1, nQ88_r1) %>% 
   na.omit() %>%
   group_by(nQ88_r1) %>%
   mutate(perc = n / sum(n)) %>% 
   ggplot(aes(x = nQ88_r1, y = perc, fill = nQ63_r1)) + 
   geom_col(position = "fill") +
   geom_text(aes(label = round(perc*100, 0)), 
             position = position_fill(vjust = 0.5), 
             size = 6, color = "black") +
   theme_minimal() +
   coord_flip() +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_fill_manual(values = n5_pallet) +
   labs(x = "", y = "", fill = "") +
   theme(legend.position = "top",
         legend.box = "horizontal",
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14),
         legend.text = element_text(size = 14),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank()) +
   guides(fill = guide_legend(nrow = 5, reverse = TRUE))

ggsave(plot = nQ63_r1_pohlavi, filename = "nQ63_r1 x pohlavi.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)


#----------------------------nQ63_r1 x celk_spotr----------------------------------#

##### tabulka s N a %
tabulka <- table(data$nQ63_r1, data$celk_spotr_filtr_5kat)
tabulka
round(prop.table(tabulka, margin = 2) *100, 0)


##### tabulka s % a CI

ci_vysledky <- list()

for (col in 1:ncol(tabulka)) {
  total <- sum(tabulka[, col])  
  ci_mat <- matrix(nrow = nrow(tabulka), ncol = 3)
  rownames(ci_mat) <- rownames(tabulka)
  
  for (row in 1:nrow(tabulka)) {
    success <- tabulka[row, col]
    ci <- BinomCI(success, total, conf.level = 0.95, method = "wilson")
    ci_mat[row, ] <- ci  # sloupce: estimate, lower, upper
  }
  
  ci_vysledky[[colnames(tabulka)[col]]] <- ci_mat
}

for (cat in names(ci_vysledky)) {
  cat("\nVzdělání:", cat, "\n")
  df <- ci_vysledky[[cat]]
  for (i in 1:nrow(df)) {
    cat(rownames(df)[i], 
        "- %:", round(df[i, 1] * 100, 1),
        " | 95% CI:", 
        paste0(round(df[i, 2:3] * 100, 1), collapse = " – "), "\n")
  }
}


###### X2
chisq_result <- chisq.test(tabulka) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(tabulka)
## p value = <0,001





#####graf s CI
ci_data <- data %>%
  filter(!is.na(nQ63_r1), !is.na(celk_spotr_filtr_5kat)) %>%
  count(celk_spotr_filtr_5kat, nQ63_r1) %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(
    bt = list(binom.test(n, total, conf.level = 0.95)),
    ci_lower = bt$conf.int[1],
    ci_upper = bt$conf.int[2]
  ) %>%
  ungroup()

ggplot(ci_data, aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ63_r1)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = round(perc * 100, 0)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_manual(values = tableau_color_pal("Superfishel Stone")(5)) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Pití po krátkodobé abstinenci dle počtu sklenic alkoholu (za týden)",
    subtitle = paste0("N = ", n)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  guides(fill = guide_legend(nrow = 2))



######graf bez CI
nQ63_r1_spotr = data %>% 
  mutate(nQ63_r1 = factor(nQ63_r1, levels = c(
    "Pil/a alkohol více než před ní",
    "Pil/a alkohol přibližně stejně jako před ní",
    "Přibližně pár týdnů pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
    "Přibližně pár měsíců pil/a alkohol méně, ale postupně se vrátil/a k původní spotřebě",
    "Trvale snížil/a spotřebu alkoholu"
  ))) %>%
  count(nQ63_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ63_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 6, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n5_pallet) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 5, reverse = TRUE))

ggsave(plot = nQ63_r1_spotr, filename = "nQ63_r1 x celk_spotr.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)



# Jaké jsou osobní důvody (motivace) občanů ČR ke krátkodobé absti --------

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
# kontrola nulařů

kontrola_nul = data %>% 
  filter(celk_spotr2 == 0) %>% 
  select(celk_spotr2, nQ01_r1, starts_with("nQ13"))



# Co představuje překážky během krátkodobé abstinence? --------------------

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
  geom_text(position = position_stack(vjust = 0.5), size = 4) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(labels = ~str_wrap(., width = 40)) +
  scale_fill_manual(values = c(missing_color, rev(seq_pallet4))) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        legend.text = element_text(size = 12))+
  labs(title = "",
       fill = "",
       y = "",
       x = "")

ggsave(plot = nQ59_battery, filename = "nQ59-battery.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24, height = 15, scaling = 1)




# Co představuje překážky vedoucí k předčasnému ukončení krátkodob --------


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

#------------------------------tQ66_0_0---------------------------------#

omez_pocet_skl = data %>% 
  select(respondent_id_internal,tQ66_0_0)




# OMEZOVANI Jaká část obyvatel ČR se pokouší nějakým způsobem kontrolovaně o --------

omezovani = data %>% 
  select(nQ65_r1, tQ66_0_0, nQ67_r1, tQ68_0_0, nQ69_r1,tQ70_0_0, nQ71_r1,tQ72_0_0, nQ73_r1,tQ74_0_0, nQ75_r1, tQ76_0_0)


#způsoby omezování alkoholu
  #nQ65_r1 nQ67_r1 nQ69_r1 nQ71_r1 nQ72_r1 nQ73_r1 nQ74_r1

zpusob_omezovani = data %>% 
  select(nQ65_r1, nQ67_r1, nQ69_r1, nQ71_r1, nQ73_r1, nQ75_r1)

nQ65_nQ75_battery = zpusob_omezovani %>% 
  pivot_longer(cols = everything(), names_to = "item", values_to = "value") %>%
  count(item, value) %>%
  left_join(data_labelled %>% 
              select(item = variable, label) %>%
              mutate(label = str_extract(label, "\\[.*?\\]") %>% 
                       str_remove_all("\\[|\\]")), 
            by = "item") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n, na.rm = TRUE),
         pos_freq = sum(percent[value %in% c("Ano, vícekrát", "Ano, jednou")])) %>%
  ungroup() %>%
  mutate(percent_label = percent(percent, accuracy = 1, suffix = ""),
         percent_label = if_else(percent <= 0.01, "", percent_label),
         label = paste("...", label, sep = ""),
         label = fct_reorder(label, pos_freq),
         value = fct_rev(value)) %>%
  ggplot(aes(x = percent, y = label, fill = value, label = percent_label)) +
  geom_col(color = "white") +
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(labels = ~str_wrap(., width = 40)) +
  scale_fill_manual(values = c(missing_color, rev(seq_pallet3))) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE, nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "top")+
  labs(title = "",
       fill = "",
       y = "",
       x = "")

nQ65_nQ75_battery
ggsave(plot = nQ65_nQ75_battery, filename = "nQ65_nQ75_battery.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)

  
#PODLE DEMOGRAFIK

#demografika
  #vzd4
  #pohlavi nQ88_r1
  #vek tQ89_0_0
  #velikost bydliste 5kat tQ108_10_1
  #ekonomicka aktivita ea2
  #prijem prijem_osob
  #celkova spotreba(jen co piji) celk_spotr_filtr_5kat

zpusob_omezovani_vzd = data %>% 
  select(nQ65_r1, nQ67_r1, nQ69_r1, nQ71_r1, nQ73_r1, nQ75_r1, vzd4)

ci_data_omez <- zpusob_omezovani_vzd %>%
  filter(!is.na(nQ65_r1),!is.na(nQ67_r1),!is.na(nQ69_r1),!is.na(nQ71_r1),
         !is.na(nQ73_r1), !is.na(vzd4)) %>% 
  count(vzd4, nQ65_r1, nQ67_r1, nQ69_r1, nQ71_r1, nQ73_r1) %>% 
  group_by(vzd4) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / total) %>%
  rowwise() %>%
  mutate(bt = list(binom.test(n, total, conf.level = 0.95)),
         ci_lower = bt$conf.int[1],
         ci_upper = bt$conf.int[2]) %>%
  ungroup()

#nedodelany
ggplot(ci_data_omez, aes(x = vzd4, y = perc, fill = nQ51_r1)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.9), width = 0.2, alpha = 0.35) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    x = "", y = "",
    fill = "",
    title = "",
    subtitle = "" ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

#vzdělání 4 kategorie
data %>% 
  count(nQ75_r1, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd4, y = perc, fill = nQ75_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "max množství x vzdělání",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

#pohlaví
data %>% 
  count(nQ65_r1, nQ88_r1) %>% 
  na.omit() %>%
  group_by(nQ88_r1) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = nQ88_r1, y = perc, fill = nQ65_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "max množství x pohlaví",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

#vek 

data %>% 
  count(nQ65_r1, vek5) %>% 
  na.omit() %>%
  group_by(vek5) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vek5, y = perc, fill = nQ65_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "max množství x věk",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))


# #INDEX OMEZOVÁNÍ --------------------------------------------------------
#INDEX OMEZOVÁNÍ

# recode

#verze 1

recode_limited <- function(x) {dplyr::recode(as.numeric(x), `1` = 2, `2` = 1, `3` = 0)}

data = data %>%
  mutate(nQ65_r1_rec = recode_limited(nQ65_r1),
         nQ67_r1_rec = recode_limited(nQ67_r1),
         nQ69_r1_rec = recode_limited(nQ69_r1),
         nQ71_r1_rec = recode_limited(nQ71_r1),
         nQ73_r1_rec = recode_limited(nQ73_r1),
         nQ75_r1_rec = recode_limited(nQ75_r1))

#verze 2 - final

recode_limited <- function(x) {dplyr::recode(as.numeric(x), `1` = 1, `2` = 1, `3` = 0)}

data = data %>%
  mutate(nQ65_r1_rec = recode_limited(nQ65_r1),
         nQ67_r1_rec = recode_limited(nQ67_r1),
         nQ69_r1_rec = recode_limited(nQ69_r1),
         nQ71_r1_rec = recode_limited(nQ71_r1),
         nQ73_r1_rec = recode_limited(nQ73_r1),
         nQ75_r1_rec = recode_limited(nQ75_r1))


# INDEX OMEZOVÁNÍ


#prumerovy
data = data %>%
  mutate(omez_mea = rowMeans(across(c(nQ65_r1_rec,
                                             nQ67_r1_rec,
                                             nQ69_r1_rec,
                                             nQ71_r1_rec,
                                             nQ73_r1_rec,
                                             nQ75_r1_rec)), na.rm = TRUE))

hist(data$omez_mea)

#souctovy
data = data %>%
  mutate(omez_sum = rowSums(select(., nQ65_r1_rec,
                                   nQ67_r1_rec,
                                   nQ69_r1_rec,
                                   nQ71_r1_rec,
                                   nQ73_r1_rec,
                                   nQ75_r1_rec), na.rm = TRUE))
hist(data$omez_sum)



#kategorizace indexu

data <- data %>%
  mutate(omez_sum_cat = case_when(
    omez_sum  == 0 ~ "Žádné",
    omez_sum  <= 2 ~ "1-2 způsoby",
    omez_sum  <= 6 ~ "3 a více způsobů",
    TRUE ~ "Silné"))
table(data$omez_sum_cat)

#nazdarek

#------------------------------ omez_sum x demografika --------------------------------#

data %>% 
  count(vek4, omez_sum_cat) %>% 
  mutate(omez_sum_cat = factor(omez_sum_cat, levels = rev(c("Žádné", "1-2 způsoby", "3 a více způsobů" )))) %>% 
  group_by(vek4) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vek4, y = perc, fill = omez_sum_cat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = rev(seq_pallet3)) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

#------------------------------ omez_sum x hodnoceni omezovani--------------------------------#

#nQ77 / nejde pouzit pac jsme se ptali jen tech kteri pouzili 

#nQ79

omez_sum_cat_nQ79_r1 = data %>% 
  filter(!is.na(nQ79_r1)) %>% 
  count(nQ79_r1, omez_sum_cat) %>% 
  mutate(omez_sum_cat = factor(omez_sum_cat, levels = rev(c("Žádné", "1-2 způsoby", "3 a více způsobů" )))) %>% 
  mutate(nQ79_r1 = factor(nQ79_r1,levels = rev(c("1", "2", "3", "4", "5", "Nevím / Nedokážu posoudit" )))) %>% 
  group_by(nQ79_r1) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = nQ79_r1, y = perc, fill = omez_sum_cat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 5, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = rev(seq_pallet3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

omez_sum_cat_nQ79_r1
ggsave(plot = omez_sum_cat_nQ79_r1, filename = "nQ63_r1 x vzd4.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)

#regrese - pohlavi, vek, prijem
library(performance)
library(parameters)

m1_omez = lm(omezovani_index_mea ~ vzd4 + nQ88_r1 + ea2 + tQ108_10_1, data = data_omezovani)
summary(m1_omez)
parameters(m1_omez)
check_model(m1_omez, check = c("linearity", "homogeneity", "normality"))

#------------------------------ omezovani_index_sum x pohlavi --------------------------------#

ggplot(data, aes(x = nQ88_r1, y = omezovani_index_sum, fill = nQ88_r1)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal()

#------------------------------ omezovani_index_sum x vek --------------------------------#

ggplot(data, aes(x = vek4, y = omezovani_index_sum, fill = vek4)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal()

#------------------------------tQ74_0_0---------------------------------#

install.packages("writexl")
library(writexl)

Q74_kod = data %>% 
  select(respondent_id_internal, tQ74_0_0)

write_xlsx(Q74_kod, "codebooky/prilez_alk.xlsx")

#------------------------------ nQ77_r1 --------------------------------#
#Jak u sebe z dlouhodobého pohledu hodnotíte úspěšnost takovéhoto omezování
#konzumace alkoholu bez úplné abstinence

table(data$nQ77_r1)

data %>%
  filter(!is.na(nQ77_r1)) %>%
  count(nQ77_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ77_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  

vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Tyto věci neřeším, nijak se záměrně nesnažím udržovat konzumaci na nějaké stanovené úrovni",
                                              "Dlouhodobě se mi to nedaří",
                                              "Po většinu času se mi to nedaří, ale někdy po nějaké období ano",
                                              "Daří se mi to po většinu času, ale někdy po nějaké období ne",
                                              "Daří se mi to dlouhodobě"
  )))

nQ77 = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 60)) +
  labs(title = "Hodnocení omezování konzumace alkoholu", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()

ggsave(plot = nQ77, filename = "nQ77.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)


#------------------------------ nQ79_r1 --------------------------------#
#Ohodnoťte, jak se vám celkově daří omezovat konzumaci alkoholu známkou jako ve škole

table(data$nQ79_r1)
levels(data$nQ79_r1)

data %>%
  filter(!is.na(nQ79_r1)) %>%
  count(nQ79_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ79_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Nevím / Nedokážu posoudit", "5", "4", "3", "2", "1")))  

vysledky


nQ79 = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 50)) +
  labs(title = "Celkové hodnocení omezování konzumace alkoholu", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()

ggsave(plot = nQ79, filename = "nQ79.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)




# Jaké strategie občané ČR využívají v oblasti kontrolovaného omez --------




# tQ80 / vek ochutnani alkoholu --------------------------------------------------------------------
# V kolika letech jste poprvé ochutnal/a nějaký alkoholický nápoj
label_attribute(data$tQ80_0_0)
table(data$tQ80_0_0) ##99??????

data$tQ80_0_0_num <- as.numeric(as.character(data$tQ80_0_0))


describe(data$tQ80_0_0_num, quant=c(.25,.75)) 
#   n  mean   sd median trimmed  mad min max range skew kurtosis   se Q0.25 Q0.75
# 813 14.94 4.69     15   15.19 2.97   1  99    98 6.87   127.17 0.16    13    17



tQ80 = data %>%
  ggplot(aes(x = tQ80_0_0_num)) +
  geom_histogram(bins = 30, fill = "#D9A939", color = "white", alpha = 0.9) +
  theme_minimal() +
  labs(title = "V kolika letech poprvé ochutnal/a alkoholický nápoj", x = "Roky", y = "",
       subtitle = paste("N: 813",
                        "| Průměr:", round(mean(data$tQ80_0_0_num, na.rm=TRUE), 1),
                        "| Median:", median(data$tQ80_0_0_num, na.rm=TRUE),
                        "| SD:", round(sd(data$tQ80_0_0_num, na.rm=TRUE), 1))) +
  theme(plot.title = element_text(face = "bold"))

tQ80
ggsave(plot = tQ80, filename = "tQ80.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)

###kategorizace 


data <- data %>%
  mutate(tQ80_cat = cut(
    tQ80_0_0_num,
    breaks = c(0, 10, 14, 17, 99),
    labels = c("10 let nebo méně", "11–14 let", "15–17 let", "18 a více let"),
    right = TRUE, include.lowest = TRUE
  ))

table(data$tQ80_cat)




data %>%
  filter(!is.na(tQ80_cat)) %>%
  count(tQ80_cat) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$tQ80_cat)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(c("10 let nebo méně",
                                                  "11–14 let",
                                                  "15–17 let",
                                                  "18 a více let"))))



vysledky$x <- factor(1) 


tQ80_cat = ggplot(vysledky, aes(x = x, y = Podil, fill = Odpoved)) +
  geom_col(width = 0.4) +  
  geom_text(aes(label = paste0(round(Podil*100))),
            position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet4)) +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top") +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

tQ80_cat
ggsave(plot = tQ80_cat, filename = "tQ80_cat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 8, scaling = 1.6)


# tQ82 --------------------------------------------------------------------

table(data$tQ82_0_0) 

data$tQ82_0_0_num <- as.numeric(as.character(data$tQ82_0_0))


describe(data$tQ82_0_0_num, quant=c(.25,.75)) 
#n   mean   sd median trimmed  mad min max range skew kurtosis   se Q0.25 Q0.75
#751 16.7 5.34     17   16.64 1.48   0 100   100 5.95    89.09 0.19    15    18


tQ82 = data %>%
  ggplot(aes(x = tQ82_0_0_num)) +
  geom_histogram(bins = 30, fill = "#D9A939", color = "white", alpha = 0.9) +
  theme_minimal() +
  labs(title = "V kolika letech poprvé cítil/a, že je pod vlivem alkoholu", x = "Roky", y = "",
       subtitle = paste("N: 751",
                        "| Průměr:", round(mean(data$tQ82_0_0_num, na.rm=TRUE), 1),
                        "| Median:", median(data$tQ82_0_0_num, na.rm=TRUE),
                        "| SD:", round(sd(data$tQ82_0_0_num, na.rm=TRUE), 1))) +
  theme(plot.title = element_text(face = "bold"))

ggsave(plot = tQ82, filename = "tQ82.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)

###kategorizace 


data <- data %>%
  mutate(tQ82_cat = cut(
    tQ82_0_0_num,
    breaks = c(0, 10, 15, 20, 100),
    labels = c("10 let nebo méně", "11–14 let", "15–17 let", "18 a více let"),
    right = TRUE, include.lowest = TRUE
  ))

table(data$tQ82_cat)




data %>%
  filter(!is.na(tQ82_cat)) %>%
  count(tQ82_cat) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$tQ82_cat)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(c("10 let nebo méně",
                                                  "11–14 let",
                                                  "15–17 let",
                                                  "18 a více let"))))



vysledky$x <- factor(1) 


tQ82_cat = ggplot(vysledky, aes(x = x, y = Podil, fill = Odpoved)) +
  geom_col(width = 0.4) +  
  geom_text(aes(label = paste0(round(Podil*100))),
            position = position_stack(vjust = 0.5),
            size = 6, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet4)) +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 13),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 13)) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

ggsave(plot = tQ82_cat, filename = "tQ82_cat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)

# predelany graf pro vek prvni konyumace
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

# celk_spotr_filtr_5kat --------------------------------------------------------


table(data$celk_spotr_filtr_5kat)


data %>%
  filter(!is.na(celk_spotr_filtr_5kat)) %>%
  count(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$celk_spotr_filtr_5kat)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("0 - 0,5",
                                              "1 - 2",
                                              "2,5 - 5",
                                              "5,5 - 9,5",
                                              "10+" )))

celk_spotr_filtr_5kat = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 35)) +
  labs(title = "Celková spotřeba alkoholu (počet sklenic za týden) ", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()

ggsave(plot = celk_spotr_filtr_5kat, filename = "celk_spotr_filtr_5kat.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)


# nQ52 - kratkodoba abst <3 tydny --------------------------------------------------------------------



table(data$nQ52_r1)


data %>%
  filter(!is.na(nQ52_r1)) %>%
  count(nQ52_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ52_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("Nikdy jsem nezkusil/a a neplánuji to zkusit",
                                              "Nikdy jsem nezkusil/a, ale plánuji to",
                                              "Jednou jsem zkusil/a",
                                              "Zkusil/a jsem vícekrát",
                                              "Tímto způsobem abstinuji jednou ročně",
                                              "Tímto způsobem abstinuji vícekrát ročně")))

nQ52_r1 = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 85)) +
  labs(title = "Zkušenost s abstinencí kratší než 3 týdny ", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()

nQ52_r1
ggsave(plot = nQ52_r1, filename = "nQ52.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)



# nQ53 Kdy jste ukončil/a svoji poslední krátkodobou abstinenci? --------------------------------------------------------------------

labe
label_attribute(data$nQ53_r1)
table(data$nQ53_r1)


data %>%
  filter(!is.na(nQ53_r1)) %>%
  count(nQ53_r1) %>%
  mutate(perc = n / sum(n) * 100)

##% + CI
tabulka <- table(data$nQ53_r1)
n <- sum(tabulka)


vysledky <- data.frame(
  Odpoved = character(),
  Pocet = integer(),
  Podil = numeric(),
  CI_dolni = numeric(),
  CI_horni = numeric(),
  stringsAsFactors = FALSE
)

for (odpoved in names(tabulka)) {
  x <- tabulka[odpoved]
  test <- binom.test(x, n, conf.level = 0.95)
  
  vysledky <- rbind(vysledky, data.frame(
    Odpoved = odpoved,
    Pocet = x,
    Podil = x / n,
    CI_dolni = test$conf.int[1],
    CI_horni = test$conf.int[2]
  ))
}


vysledky <- vysledky %>%
  mutate(
    Podil_pct = Podil * 100,
    CI_dolni_pct = CI_dolni * 100,
    CI_horni_pct = CI_horni * 100
  ) %>%
  arrange(desc(Podil_pct)) %>%
  mutate(Odpoved = factor(Odpoved, levels = rev(Odpoved)))  


vysledky <- vysledky %>%
  mutate(Odpoved = factor(Odpoved, levels = c("V tuto chvíli stále abstinuji",
                                              "Během posledního měsíce",
                                              "Během posledních 2 až 6 měsíců",
                                              "Během posledních 7 až 12 měsíců",
                                              "Před více než rokem")))

nQ53_r1 = ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_text(aes(label = paste0(round(Podil_pct, 0))), #" %"#
            hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), limits = c(0, 40)) +
  labs(title = "Kdy ukončil/a poslední krátkodobou abstinenci", x = "", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip()

ggsave(plot = nQ53_r1, filename = "nQ53.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 24.5, height = 15, scaling = 1)



    
celk_spotr_filtr_5katxvzd5 = data %>% 
  count(celk_spotr_filtr_5kat, vzd5) %>% 
  mutate(celk_spotr_filtr_5kat = factor(celk_spotr_filtr_5kat, levels = c(
    "10+",
    "5,5 - 9,5",
    "2,5 - 5",
    "1 - 2",
    "0 - 0,5"
  ))) %>%
  na.omit() %>%
  group_by(vzd5) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd5, y = perc, fill = celk_spotr_filtr_5kat)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 6, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = rev(seq_pallet5)) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

ggsave(plot = celk_spotr_filtr_5katxvzd5, filename = "celk_spotr_filtr_5kat x vzd5.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)

mutate(celk_spotr_filtr_5kat = factor(celk_spotr_filtr_5kat, levels = c(
  "10+",
  "5,5 - 9,5",
  "2,5 - 5",
  "1 - 2",
  "0 - 0,5"
))) %>%

nQ51_r1xvzd4 = data %>% 
  count(nQ51_r1, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd4, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = round(perc*100, 0)), 
            position = position_fill(vjust = 0.5), 
            size = 6, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = n6_pallet) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 3, reverse = TRUE))

ggsave(plot = nQ51_r1xvzd4, filename = "nQ51_r1xvzd4.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)




