
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

##### tvorba veku se 4 kategoriemi 

data$vek4 <- cut(
  data$tQ89_0_0,
  breaks = c(17, 29, 49, 64, Inf),
  labels = c("18–29", "30–49", "50–64", "65+"),
  right = TRUE)

table(data$vek4)


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





#--------------------------------tQ54_0_0--------------------------------------#
#-> medián, min, max, průměr otázky tQ54_0_0 (Jak dlouho trvala/trvá Vaše
#poslední krátkodobá abstinence? (v týdnech))


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

#---------------rekodovani promennych v duvodech/motivacich--------------------#
# 0 - 1 = 1, 2 - 3 = 2, 4 - 6 = 3, 7 - 8 = 4, 9 - 10 = 5
# puvodni nQ58_0_1 : nQ58_10_1 v datech zustavaji
# nQ58_0_1_num : nQ58_10_1_num numericka verze
# nQ58_0_1_fac : nQ58_10_1_fac faktorova verze (1 = Zcela nedůležité, 2 = Spíše nedůležité, 3 = Středně důležité, 4 = Spíše důležité, 5 = Zcela zásadní)

vars <- paste0("nQ58_", 0:10, "_1")


var_labels <- sapply(data[vars], function(x) attr(x, "label"))


data_num <- lapply(data[vars], function(x) {
  x <- as.character(x)
  x[x == "Nevím"] <- NA
  as.numeric(gsub(" =.*", "", x))
})


data_num <- lapply(data_num, function(x) {
  case_when(
    x <= 1              ~ 1,
    x > 1 & x <= 3      ~ 2,
    x > 3 & x <= 6      ~ 3,
    x > 6 & x <= 8      ~ 4,
    x > 8 & x <= 10     ~ 5,
    TRUE                ~ NA_real_
  )
})


vars_num <- paste0(vars, "_num")
for (i in seq_along(vars)) {
  data[[vars_num[i]]] <- data_num[[i]]
  attr(data[[vars_num[i]]], "label") <- var_labels[[i]]
}


value_labels <- c(
  "Zcela nedůležité"    = 1,
  "Spíše nedůležité"    = 2,
  "Středně důležité"    = 3,
  "Spíše důležité"      = 4,
  "Zcela zásadní"       = 5
)


vars_fac <- paste0(vars, "_fac")
for (i in seq_along(vars_num)) {
  v_num <- vars_num[i]
  v_fac <- vars_fac[i]
  
  data[[v_fac]] <- factor(
    data[[v_num]],
    levels = 1:5,
    labels = names(value_labels)
  )
  attr(data[[v_fac]], "label") <- var_labels[[i]]
}



# Co představuje překážky během krátkodobé abstinence? --------------------

#--------------------rekodovani promennych v prekazkach------------------------#
#nQ59_0_1 : nQ59_10_1
# 0 = 1, 1 - 3 = 2, 4 - 6 = 3, 7 - 10 = 4
# puvodni nQ59_0_1 : nQ59_10_1 v datech zustavaji
# nQ59_0_1_num : nQ59_10_1_num numericka verze
# nQ59_0_1_fac : nQ59_10_1_fac faktorova verze (1 = Vůbec ne, 2 = Trochu, 3 = Do určité míry, 4 = Velmi)



vars <- paste0("nQ59_", 0:10, "_1")


var_labels <- sapply(data[vars], function(x) attr(x, "label"))


data_num <- lapply(data[vars], function(x) {
  x <- as.character(x)
  x[x == "Nevím"] <- NA
  as.numeric(gsub(" =.*", "", x))
})


data_num <- lapply(data_num, function(x) {
  case_when(
    x == 0                  ~ 1,
    x >= 1 & x <= 3         ~ 2,
    x >= 4 & x <= 6         ~ 3,
    x >= 7 & x <= 10        ~ 4,
    TRUE                    ~ NA_real_
  )
})



vars_num <- paste0(vars, "_num")
for (i in seq_along(vars)) {
  data[[vars_num[i]]] <- data_num[[i]]
  attr(data[[vars_num[i]]], "label") <- var_labels[[i]]
}


value_labels <- c(
  "Vůbec ne"    = 1,
  "Trochu"    = 2,
  "Do určité míry"    = 3,
  "Velmi"      = 4)


vars_fac <- paste0(vars, "_fac")
for (i in seq_along(vars_num)) {
  v_num <- vars_num[i]
  v_fac <- vars_fac[i]
  
  data[[v_fac]] <- factor(
    data[[v_num]],
    levels = 1:4,
    labels = names(value_labels)
  )
  attr(data[[v_fac]], "label") <- var_labels[[i]]
}

table(data$nQ59_0_1)
table(data$nQ59_0_1_num)
table(data$nQ59_0_1_fac)




# Co představuje překážky vedoucí k předčasnému ukončení krátkodob --------





# Jaká část obyvatel ČR se pokouší nějakým způsobem kontrolovaně o --------

#způsoby omezování alkoholu
  #nQ65_r1 nQ67_r1 nQ69_r1 nQ71_r1 nQ72_r1 nQ73_r1 nQ74_r1

zpusob_omezovani = data %>% 
  select(nQ65_r1, nQ67_r1, nQ69_r1, nQ71_r1, nQ73_r1, nQ75_r1)




# Jaké strategie občané ČR využívají v oblasti kontrolovaného omez --------



