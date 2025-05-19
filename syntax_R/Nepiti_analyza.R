
packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car", "FSA", "psych", "labelled")
purrr::walk(packages, library, character.only = TRUE)


data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))
data_labelled <- generate_dictionary(data)

##### tvorba veku se 4 kategoriemi 

data$vek4 <- cut(
  data$tQ89_0_0,
  breaks = c(17, 29, 49, 64, Inf),
  labels = c("18–29", "30–49", "50–64", "65+"),
  right = TRUE)

table(data$vek4)


basic_color = "#D9A939"

seq_pallet = c("#FAF0D1", "#F0C661", "#D9A939", "#B57F22", "#855A13")

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




# Co představuje překážky během krátkodobé abstinence? --------------------





# Co představuje překážky vedoucí k předčasnému ukončení krátkodob --------





# Jaká část obyvatel ČR se pokouší nějakým způsobem kontrolovaně o --------




# Jaké strategie občané ČR využívají v oblasti kontrolovaného omez --------



