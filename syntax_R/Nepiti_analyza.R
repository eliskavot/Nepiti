
packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled", "parameters", "performance",
              "scales")
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

##### tvorba veku se 4 kategoriemi 

data$vek4 <- cut(
  data$tQ89_0_0,
  breaks = c(17, 29, 49, 64, Inf),
  labels = c("18–29", "30–49", "50–64", "65+"),
  right = TRUE)

table(data$vek4)


##### prejmenovani kategorie ve vzd4

table(data$vzd4)
data$vzd4 <- fct_recode(data$vzd4,
                        "VOŠ a VŠ" = "3 \"VOŠ, Bc. a VŠ\"")


  
# Jak rozšířená je krátkodobá  abstinence v české společnosti? ------------



#--------------------------------tQ54_0_0--------------------------------------#
#-> medián, min, max, průměr otázky tQ54_0_0 (Jak dlouho trvala/trvá Vaše
#poslední krátkodobá abstinence? (v týdnech))

#???? Klara: heledte tahle promenna, je hodne sikma... si trochu rikam, jestli
##            by nebylo od veci, ta odlehla pozorovani nejak poresit, pac nam 
##            to docela zkresluje aj ten graf ne? - a prumer nam to hoodne 
##            posouva, takze bychom meli pouzivat median pak?

table(data$tQ54_0_0)
data$tQ54_0_0[data$tQ54_0_0 == "Nevím, nedokážu spočítat"] <- NA 
data$tQ54_0_0_num <- as.numeric(as.character(data$tQ54_0_0))


describe(data$tQ54_0_0_num, quant=c(.25,.75)) 
#  vars   n  mean    sd median trimmed  mad min max range  skew kurtosis   se Q0.25 Q0.75
#1    1 489 16.12 50.07      6    8.88 4.45   1 670   669 10.35   118.35 2.26     4    12


##CI pro prumer 
mean_val <- mean(data$tQ54_0_0_num, na.rm = TRUE)
se_val <- sd(data$tQ54_0_0_num, na.rm = TRUE) / sqrt(sum(!is.na(data$tQ54_0_0_num))) #sd/sqrt(n)
ci_lower <- mean_val - 1.96 * se_val
ci_upper <- mean_val + 1.96 * se_val
cat("95% CI pro prumer:", round(ci_lower, 2), "-", round(ci_upper, 2)) #11,7 - 20,6


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

#Klara: tohele je moooc hezkyy!
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

vysledky

#Klarka: ten errorbar tady! aaaa krasa
ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_errorbar(aes(ymin = CI_dolni_pct, ymax = CI_horni_pct), width = 0.2, alpha = 0.5) +
  geom_text(aes(label = paste0(round(Podil_pct, 0), " %")), 
            hjust = 0.25, size = 3.5, fontface = "bold") +
  labs(title = "Dodržení délky abstinence", x = "", y = "%", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip() +
  ylim(0, max(vysledky$CI_horni_pct)+5)

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
    title = "Abstinence > 3 týdny x vzdělání 4 kategorie",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
data %>% 
  count(nQ51_r1, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd4, y = perc, fill = nQ51_r1)) + 
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
data %>% 
  count(nQ51_r1, vzd3) %>% 
  na.omit() %>%
  group_by(vzd3) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd3, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "Abstinence > 3 týdny x vzdělání 3 kategorie",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))



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
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
data %>% 
  count(nQ51_r1, vzd3) %>% 
  na.omit() %>%
  group_by(vzd3) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd3, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "Abstinence > 3 týdny x vzdělání 3 kategorie",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))





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
    title = "Abstinence > 3 týdny x počet sklenic",
    subtitle = "N = 929"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
data %>% 
  count(nQ51_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "Abstinence > 3 týdny x počet sklenic",
       subtitle = "N = 929")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))



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
    title = "Abstinence > 3 týdny x pohlaví",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
data %>% 
  count(nQ51_r1, nQ88_r1) %>% 
  na.omit() %>%
  group_by(nQ88_r1) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = nQ88_r1, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "Abstinence > 3 týdny x pohlaví",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))


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
    title = "Abstinence > 3 týdny x věk 4 kategorie",
    subtitle = "N = 1022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



######graf bez CI
data %>% 
  count(nQ51_r1, vek4) %>% 
  na.omit() %>%
  group_by(vek4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vek4, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "", y = "", fill = "", title = "Abstinence > 3 týdny x věk 4 kategorie",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2))



# Má krátkodobá abstinence efekt na snižování míry konzumace alkoh --------




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


# Jaká část obyvatel ČR se pokouší nějakým způsobem kontrolovaně o --------

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
         pos_freq = sum(percent[value %in% c("Velmi pomáhalo")])) %>%
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

#INDEX OMEZOVÁNÍ

# recode

recode_limited <- function(x) {
  dplyr::recode(as.numeric(x), `1` = 2, `2` = 1, `3` = 0)}

data_omezovani = data %>%
  mutate(nQ65_r1_rec = recode_limited(nQ65_r1),
         nQ67_r1_rec = recode_limited(nQ67_r1),
         nQ69_r1_rec = recode_limited(nQ69_r1),
         nQ71_r1_rec = recode_limited(nQ71_r1),
         nQ73_r1_rec = recode_limited(nQ73_r1),
         nQ75_r1_rec = recode_limited(nQ75_r1))

data_omezovani_kontrola = data_omezovani %>% 
  select(nQ65_r1_rec, nQ67_r1_rec, nQ69_r1_rec, nQ71_r1_rec, nQ73_r1_rec, nQ75_r1_rec)


# vytvoření indexu

#prumerovy
data_omezovani <- data_omezovani %>%
  mutate(omezovani_index_mea = rowMeans(across(c(nQ65_r1_rec,
                                             nQ67_r1_rec,
                                             nQ69_r1_rec,
                                             nQ71_r1_rec,
                                             nQ73_r1_rec,
                                             nQ75_r1_rec)), na.rm = TRUE))

hist(data_omezovani$omezovani_index_mea)
summary(data_omezovani$omezovani_index_mea)

#souctovy

data_omezovani <- data_omezovani %>%
  mutate(omezovani_index_sum = rowSums(across(c(nQ65_r1_rec,
                                            nQ67_r1_rec,
                                            nQ69_r1_rec,
                                            nQ71_r1_rec,
                                            nQ73_r1_rec,
                                            nQ75_r1_rec)), na.rm = TRUE))
hist(data_omezovani$omezovani_index_sum)
summary(data_omezovani$omezovani_index_sum)

#interpretace demografika

#pohlavi
boxplot(omezovani_index_sum ~ nQ88_r1,data = data_omezovani)
boxplot(omezovani_index_mea ~ nQ88_r1,data = data_omezovani)

#vzd4
boxplot(omezovani_index_sum ~ vzd4,data = data_omezovani)
boxplot(omezovani_index_mea ~ vzd4,data = data_omezovani)

#velikost bydliste 5kat 
boxplot(omezovani_index_sum ~ tQ108_10_1,data = data_omezovani)
boxplot(omezovani_index_mea ~ tQ108_10_1,data = data_omezovani)

#ekonomicka aktivita 
boxplot(omezovani_index_sum ~ ea2,data = data_omezovani)
boxplot(omezovani_index_mea ~ ea2,data = data_omezovani)

#prijem 
boxplot(omezovani_index_sum ~ prijem_osob,data = data_omezovani)
boxplot(omezovani_index_mea ~ prijem_osob,data = data_omezovani)

#celkova spotreba(jen co piji) 
boxplot(omezovani_index_sum ~ celk_spotr_filtr_5kat,data = data_omezovani)
boxplot(omezovani_index_mea ~ celk_spotr_filtr_5kat,data = data_omezovani)



#regrese - pohlavi, vek, prijem
library(performance)
library(parameters)

m1_omez = lm(omezovani_index_mea ~ vzd4 + nQ88_r1 + ea2 + tQ108_10_1, data = data_omezovani)
summary(m1_omez)
parameters(m1_omez)
check_model(m1_omez, check = c("linearity", "homogeneity", "normality"))

#------------------------------tQ70_0_0---------------------------------#
#zatim nefunguje
druh_alk = data %>% 
  select(tQ70_0_0,respondent_id_internal)

install.packages("readxl")
library(readxl)
library(dplyr)
library(tidyverse)

Q70_kod  = read_excel("Q70_kod.xlsx")

data = data %>%
  left_join(Q70_kod %>% select(respondent_id_internal, kod_Q70), by = "respondent_id_internal")
head(q70_kodovane)
table(is.na(q70_kodovane$kod_Q70))
names(codebook)

druh_alk = data %>% 
  select(tQ70_0_0,respondent_id_internal,Q70_kod)

is.character(codebook$Q70_kod)

names(codebook)

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

vysledky


ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_errorbar(aes(ymin = CI_dolni_pct, ymax = CI_horni_pct), width = 0.2, alpha = 0.5) +
  geom_text(aes(label = paste0(round(Podil_pct, 0), " %")), 
            hjust = 0.25, size = 3.5, fontface = "bold") +
  labs(title = "Hodnocení omezování konzumace alkoholu", x = "", y = "%", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8)) +
  coord_flip() +
  ylim(0, max(vysledky$CI_horni_pct)+5)



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


ggplot(vysledky, aes(x = Odpoved, y = Podil_pct)) +
  geom_col(fill = "#D9A939", width = 0.8) +
  geom_errorbar(aes(ymin = CI_dolni_pct, ymax = CI_horni_pct), width = 0.2, alpha = 0.5) +
  geom_text(aes(label = paste0(round(Podil_pct, 0), " %")), 
            hjust = 0.25, size = 3.5, fontface = "bold") +
  labs(title = "Celkové hodnocení omezování konzumace alkoholu", x = "", y = "%", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9)) +
  coord_flip() +
  ylim(0, max(vysledky$CI_horni_pct)+5)



ggplot(vysledky, aes(x = Podil_pct, y = Odpoved)) +
  geom_point(size = 4, color = "#D9A939") +
  geom_errorbarh(aes(xmin = CI_dolni_pct, xmax = CI_horni_pct), height = 0.2, alpha = 0.5) +
  geom_text(aes(label = paste0(round(Podil_pct, 0), " %")), 
            hjust = -0.2, size = 3.5) +
  labs(title = "Celkové hodnocení omezování konzumace alkoholu", x = "%", y = "", subtitle = paste0("N = ", n)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  xlim(0, max(vysledky$CI_horni_pct)+5)



# Jaké strategie občané ČR využívají v oblasti kontrolovaného omez --------



