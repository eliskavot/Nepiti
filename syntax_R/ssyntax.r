
packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
  "psych", "car", "FSA")

purrr::walk(packages, library, character.only = TRUE)



data <- read_sav(file = "Data/Alkohol 2025_v02.sav") %>%    
  mutate(across(where(is.labelled), as_factor))

library(labelled)
data_labelled <- generate_dictionary(data)

###Vidite to?
###ano vidim 
##uf vidim
##tak ja teda taky vidim k1

# 1)kratk. abst. ------------------------------------------------------------


#########nQ51_r1 > 3 tydny

sum(!is.na(data$nQ51_r1))

table(data$nQ51_r1)

data %>%
  count(nQ51_r1) %>%
  mutate(perc = n / sum(n) * 100)


data %>%
  count(nQ51_r1) %>%
  mutate(procento = n / sum(n) * 100,
         nQ51_r1 = reorder(nQ51_r1, procento)) %>%
  ggplot(aes(x = nQ51_r1, y = procento)) +
  geom_col(fill = "#FF7F00") +
  geom_text(aes(label = paste0(" ", round(procento, 0), "%")), 
            hjust = 0.5, size = 3.5, fontface = "bold")+
  theme_minimal() +
  coord_flip() +
  labs(title = "Abstinence > 3 týdny", x = "", y = "", subtitle = paste("N = 1022"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))








#########nQ52_r1 < 3 tydny 

sum(!is.na(data$nQ52_r1))

table(data$nQ52_r1)

data %>% 
  filter(!is.na(nQ52_r1)) %>% 
  count(nQ52_r1) %>% 
  mutate(perc = n / sum(n) * 100)


data %>%
  filter(!is.na(nQ52_r1)) %>% 
  count(nQ52_r1) %>%
  mutate(procento = n / sum(n) * 100,
         nQ52_r1 = reorder(nQ52_r1, procento)) %>%
  ggplot(aes(x = nQ52_r1, y = procento)) +
  geom_col(fill = "#FF7F00") +
  geom_text(aes(label = paste0(" ", round(procento, 0), "%")), 
            hjust = 0.5, size = 3.5, fontface = "bold")+
  theme_minimal() +
  coord_flip() +
  labs(title = "Abstinence < 3 týdny", x = "", y = "%", subtitle = paste("N = 397"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))









####### > 3 tydny x kolik piji (celk_spotr_filtr_5kat)

table(data$celk_spotr_filtr_5kat)

sum(!is.na(data$celk_spotr_filtr_5kat)) #929


table <- table(data$nQ51_r1, data$celk_spotr_filtr_5kat)
round(prop.table(table, margin = 2) * 100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)



data %>% 
  filter(!is.na(nQ51_r1) & !is.na(celk_spotr_filtr_5kat)) %>% 
  nrow() 

data %>% 
  count(nQ51_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+
  coord_flip() +
  labs(x = "Počet sklenic", y = "", fill = "", title = "Abstinence > 3 týdny x spotřeba",
       subtitle = "N = 929")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##ti, kteří krátkodobě abstinují vícekrát ročně, tak méně pijí 
## častěji zkusili alespoň jednou či vícekrát ti s 10+ sklenic
## neplánují zkusit ti, kteří skoro nepijí 0-0.5







####### < 3 tydny x kolik piji (celk_spotr_filtr_5kat)

sum(!is.na(data$nQ52_r1)) #397

table <- table(data$nQ52_r1, data$celk_spotr_filtr_5kat)
table

round(prop.table(table, margin = 2) * 100, 0)

data %>% 
  filter(!is.na(nQ52_r1) & !is.na(celk_spotr_filtr_5kat)) %>% 
  nrow() #366

levels(data$celk_spotr_filtr_5kat)

data %>% 
  count(nQ52_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ52_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+
  coord_flip() +
  labs(x = "Počet sklenic", y = "", fill = "", title = "Abstinence < 3 týdny x spotřeba",
       subtitle = "N = 366")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##ti kteri 10+ sklenic již vícekrát zkusili abstinenci kratší než 3 týdny






########pohlavi - nQ88_r1 (1022), vek5 (1022), vzd4 (1022), prijem_osob (747)

data %>% 
  count(nQ88_r1) %>% 
  mutate(perc = n / sum(n) * 100)
data %>% 
  count(vek5) %>% 
  mutate(perc = n / sum(n) * 100)
data %>% 
  count(vzd4) %>% 
  mutate(perc = n / sum(n)*100)
data %>% 
  count(prijem_osob) %>% 
  mutate(perc = n / sum(n)*100)

# > 3 tydny x pohlavi

table <- table(data$nQ51_r1, data$nQ88_r1)
table
round(prop.table(table, margin = 2) * 100, 0)

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(nQ88_r1)) %>% 
  nrow() ##1022

data %>% 
  count(nQ51_r1, nQ88_r1) %>% 
  na.omit() %>%
  group_by(nQ88_r1) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = nQ88_r1, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Pohlaví", y = "", fill = "", title = "Abstinence > 3 týdny x pohlaví",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##jednou zkusili časteji muži než ženy 
##ženy častěji krátkodobě abstinují vícekrát ročně


# >3 tydny x vek5

table <- table(data$nQ51_r1, data$vek5)
table
round(prop.table(table, margin = 2) * 100, 0)

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(vek5)) %>% 
  nrow() #1022

data %>% 
  count(nQ51_r1, vek5) %>% 
  na.omit() %>%
  group_by(vek5) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = vek5, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Věk", y = "", fill = "", title = "Abstinence > 3 týdny x věk",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##nejmladší častěji plánují abstinenci zkusit nebo jednou zkusili
##70+ častěji nikdy nezkusili a ani neplánují; méně často zkusili jednou či vícekrát a méně často abstinují vícekrát ročně

# >3 tydny x vzd4

table <- table(data$nQ51_r1, data$vzd4)
table
round(prop.table(table, margin = 2) *100, 0)

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(vzd4)) %>% 
  nrow() #1022

levels(data$vzd4)

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
  labs(x = "Vzdělání", y = "", fill = "", title = "Abstinence > 3 týdny x vzdělání",
       subtitle = "N = 1022")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##lidé s nejvyšším vzděláním častěji nikdy nezkusili a ani neplánují 
##zajímavé je, že lidé se základkou nejvíce krátkodobě abstinují vícekrát ročně


# > 3 tydny x prijem_osob

table <- table(data$nQ51_r1, data$prijem_osob)
table

round(prop.table(table, margin = 2) *100, 0)

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(prijem_osob)) %>% 
  nrow() #747

levels(data$prijem_osob)

data %>% 
  count(nQ51_r1, prijem_osob) %>% 
  na.omit() %>%
  group_by(prijem_osob) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = prijem_osob, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Příjem", y = "", fill = "", title = "Abstinence > 3 týdny x příjem",
       subtitle = "N = 747")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##nejnižší příjem = častěji plánují zkusit 
##průměrný příjem = častěji neplánují zkusit 



# <3 tydny x pohlavi 

table <- table(data$nQ52_r1, data$nQ88_r1)
table

round(prop.table(table, margin = 2) * 100, 0)

data %>% 
  filter(!is.na(nQ52_r1) & !is.na(nQ88_r1)) %>% 
  nrow() #397



data %>% 
  count(nQ52_r1, nQ88_r1) %>% 
  na.omit() %>%
  group_by(nQ88_r1) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = nQ88_r1, y = perc, fill = nQ52_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Pohlaví", y = "", fill = "Pohlaví", title = "Abstinence < 3 týdny x pohlaví",
       subtitle = "N = 397")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##žádný rozdíl


# < 3 tydny x vek5

table <- table(data$nQ52_r1, data$vek5)
table

round(prop.table(table, margin = 2) *100, 0)

data %>% 
  filter(!is.na(nQ52_r1) & !is.na(vek5)) %>% 
  nrow() #397


data %>% 
  count(nQ52_r1, vek5) %>% 
  na.omit() %>%
  group_by(vek5) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = vek5, y = perc, fill = nQ52_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Věk", y = "", fill = "", title = "Abstinence < 3 týdny x věk",
       subtitle = "N = 397")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##x2 nesig., ale 70+ častěji neplánují zkusit 


# < 3 tydny x vzd4

table <- table(data$nQ52_r1, data$vzd4)
table

round(prop.table(table, margin = 2)*100,0)

data %>% 
  filter(!is.na(nQ52_r1) & !is.na(vzd4)) %>% 
  nrow() #397


data %>% 
  count(nQ52_r1, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = vzd4, y = perc, fill = nQ52_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Vzdělání", y = "", fill = "", title = "Abstinence < 3 týdny x vzdělání",
       subtitle = "N = 397")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##i tímto způsobem vícekrát ročně patrně abstinují lidé se základkou


# < 3 tydny x prijem

table <- table(data$nQ52_r1, data$prijem_osob)
table

round(prop.table(table, margin =2)*100,0)

data %>% 
  filter(!is.na(nQ52_r1) & !is.na(prijem_osob)) %>% 
  nrow() #297

data %>% 
  count(nQ52_r1, prijem_osob) %>% 
  na.omit() %>%
  group_by(prijem_osob) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = prijem_osob, y = perc, fill = nQ52_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Příjem", y = "", fill = "", title = "Abstinence < 3 týdny x příjem",
       subtitle = "N = 747")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##x2 nesig., ale nejnižší = méne častěji neplánují zkusit 





###### >3tydny x piti po asbtinenci 


table <- table(data$nQ51_r1, data$nQ63_r1)
table

round(prop.table(table, margin = 2) *100, 0)

data %>% 
  filter(!is.na(nQ51_r1) & !is.na(nQ63_r1)) %>% 
  nrow() #695


chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  count(nQ51_r1, nQ63_r1) %>% 
  na.omit() %>%
  group_by(nQ63_r1) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = nQ63_r1, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Příjem", y = "", fill = "", title = "Abstinence > 3 týdny x pití po asbtinenci",
       subtitle = "N = 695")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        axis.text.y = element_text(angle = 40, hjust = 1, size = 6)) +
  guides(fill = guide_legend(nrow = 2))

##v kategoriich par mesicu a pil vice malo pozorovani 
##ti kteri asbtinuji vicekrat rocne, tak casteji trvale snizili spotrebu 
##lide, kteri pili par tydnu alkohol mene, tak casteji zkusili abstinenci vicekrat a mene casteji abstinuji vicekrat rocne






# 2)na jak dlouho zkouseli abstinenci > 3 tydny  ------------------------------------



table(data$tQ54_0_0)
data$tQ54_0_0[data$tQ54_0_0 == "Nevím, nedokážu spočítat"] <- NA 
data$tQ54_0_0_num <- as.numeric(as.character(data$tQ54_0_0))

describe(data$tQ54_0_0_num) #n=489



data %>%
  ggplot(aes(x = tQ54_0_0_num)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15, fill = "#FF7F00", color = "white", alpha = 0.9) +
  geom_density(color = "black", size = 0.4) +
  theme_minimal() +
  labs(title = "Doba abstinence", x = "Týdny", y = "",
       subtitle = paste("Mean:", round(mean(data$tQ54_0_0_num, na.rm=TRUE), 1),
                        "| Median:", median(data$tQ54_0_0_num, na.rm=TRUE),
                        "| SD:", round(sd(data$tQ54_0_0_num, na.rm=TRUE), 1),
                        "| N: 489")) +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_log10()




data %>%
  ggplot(aes(x = tQ54_0_0_num)) +
  geom_boxplot(fill = "#FF7F00", color = "black", alpha = 0.9) +
  theme_minimal() +
  labs(title = "Doba abstinence", x = "Týdny", y = "",
       subtitle = paste("Mean:", round(mean(data$tQ54_0_0_num, na.rm=TRUE), 1),
                        "| Median:", median(data$tQ54_0_0_num, na.rm=TRUE),
                        "| SD:", round(sd(data$tQ54_0_0_num, na.rm=TRUE), 1),
                        "| N: 489")) +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_log10()



######### kolik piji x delka abstinence 

data %>% 
  filter(!is.na(celk_spotr_filtr_5kat)) %>% 
  group_by(celk_spotr_filtr_5kat) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))

data %>% 
  filter(!is.na(tQ54_0_0_num) & !is.na(celk_spotr_filtr_5kat)) %>% 
  nrow() #444


data %>% 
  filter(!is.na(celk_spotr_filtr_5kat)) %>% 
  ggplot(mapping = aes(x = celk_spotr_filtr_5kat, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Spotřeba x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 484")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()



data %>% 
  filter(!is.na(celk_spotr_filtr_5kat)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Spotřeba x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 484")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

##asi žádný rozdíl


#########zpusob abstinence x delka abstinence

data %>% 
  group_by(nQ51_r1) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))

data %>% 
  filter(!is.na(tQ54_0_0_num) & !is.na(nQ51_r1)) %>% 
  nrow() #489

data %>% 
  filter(!nQ51_r1 %in% c(
    "Nikdy jsem nezkusil/a a neplánuji to zkusit",
    "Nikdy jsem nezkusil/a, ale plánuji to"  )) %>%
  ggplot(mapping = aes(x = nQ51_r1, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Způsob abstinence x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()


data %>% 
  filter(!nQ51_r1 %in% c(
    "Nikdy jsem nezkusil/a a neplánuji to zkusit",
    "Nikdy jsem nezkusil/a, ale plánuji to"  )) %>%
  ggplot(aes(x = nQ51_r1, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Způsob abstinence x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

##žádný rozdíl


data %>% 
  filter(!is.na(nQ52_r1)) %>% 
  group_by(nQ52_r1) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE)) ## u kratsi abstinence nez 3 tydny jsou nesmyslne odpovedi > nez 3 tydny



######### vek x délka abstinence 

data %>% 
  group_by(vek5) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))



data %>% 
  ggplot(mapping = aes(x = vek5, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Věk x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()



data %>% 
  ggplot(aes(x = vek5, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Věk x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))



kruskal.test(tQ54_0_0_num ~ vek5, data = data) 
dunnTest(tQ54_0_0_num ~ vek5, data = data, method = "bonferroni")

##18-29 a 30-39 abstinují delší dobu než 50-59


#########pohlavi x délka abstinence 

data %>% 
  group_by(nQ88_r1) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))


data %>% 
  ggplot(mapping = aes(x = nQ88_r1, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Pohlaví x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()



data %>% 
  ggplot(aes(x = nQ88_r1, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Pohlaví x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))



#########vzdelani x delka abstinence 

data %>% 
  group_by(vzd4) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))



data %>% 
  ggplot(mapping = aes(x = vzd4, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Vzdělání x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()



data %>% 
  ggplot(aes(x = vzd4, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Vzdělání x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))




#########prijem x delka abstinence

data %>% 
  filter(!is.na(prijem_osob)) %>% 
  group_by(prijem_osob) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))

data %>% 
  filter(!is.na(tQ54_0_0_num) & !is.na(prijem_osob)) %>% 
  nrow() #381

data %>% 
  filter(!is.na(prijem_osob)) %>% 
  ggplot(mapping = aes(x = prijem_osob, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Příjem x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 381")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()



data %>% 
  filter(!is.na(prijem_osob)) %>% 
  ggplot(aes(x = prijem_osob, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Příjem x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 381")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))



#########piti po abstinenci x delka abstinence

data %>% 
  filter(!is.na(nQ63_r1)) %>% 
  group_by(nQ63_r1) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))

data %>% 
  filter(!is.na(tQ54_0_0_num) & !is.na(nQ63_r1)) %>% 
  nrow() #489

data %>% 
  filter(!is.na(nQ63_r1)) %>% 
  ggplot(mapping = aes(x = nQ63_r1, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Pití po abstinenci x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_y_log10()



data %>% 
  filter(!is.na(nQ63_r1)) %>% 
  ggplot(aes(x = nQ63_r1, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Pití po abstinenci x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 489")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1))



kruskal.test(tQ54_0_0_num ~ nQ63_r1, data = data) # Kruskal-Wallis test (bez normality)
dunnTest(tQ54_0_0_num ~ nQ63_r1, data = data, method = "bonferroni")


#######dodrzeni abstinence x delka abstinence



data %>% 
  filter(!is.na(nQ55_r1)) %>% 
  group_by(nQ55_r1) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))

data %>% 
  filter(!is.na(tQ54_0_0_num) & !is.na(nQ55_r1)) %>% 
  nrow() #389

data %>% 
  filter(!is.na(nQ55_r1)) %>% 
  ggplot(mapping = aes(x = nQ55_r1, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Dodržení abstinence x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 389")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_y_log10()



data %>% 
  filter(!is.na(nQ55_r1)) %>% 
  ggplot(aes(x = nQ55_r1, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Dodržení abstinence x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 389")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1))



anova_model <- aov(tQ54_0_0_num ~ nQ55_r1, data = data) # ANOVA (normalita/rozptyl)
summary(anova_model)

kruskal.test(tQ54_0_0_num ~ nQ55_r1, data = data) # Kruskal-Wallis test (bez normality)
dunnTest(tQ54_0_0_num ~ nQ55_r1, data = data, method = "bonferroni")

# 3)dodrzeni delky abstinence  ----------------------------------------------


sum(!is.na(data$nQ55_r1)) #547

data %>%
  filter(!is.na(nQ55_r1)) %>% 
  count(nQ55_r1) %>%
  mutate(perc = n / sum(n) * 100)


data %>%
  filter(!is.na(nQ55_r1)) %>% 
  count(nQ55_r1) %>%
  mutate(procento = n / sum(n) * 100,
         nQ55_r1 = reorder(nQ55_r1, procento)) %>%
  ggplot(aes(x = nQ55_r1, y = procento)) +
  geom_col(fill = "#FF7F50") +
  geom_text(aes(label = paste0(" ", round(procento, 0), "%")), 
            hjust = 0.5, size = 3.5, fontface = "bold")+
  theme_minimal() +
  coord_flip() +
  labs(title = "Dodržení délky abstinence", x = "", y = "", subtitle = paste("N = 547"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))




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


data %>% 
  count(nQ55_r1, celk_spotr_filtr_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr_filtr_5kat) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = celk_spotr_filtr_5kat, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Accent")+
  labs(x = "Počet sklenic", y = "", fill = "", title = "Dodržení abstinence x počet sklenic",
       subtitle = "N = 512")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##10+ sklenic častěji ukončili abstinenci dříve 
##0-0,5 sklenic méně častěji ukončili abstinenci dříve 


##########pohlavi x dodrzeni 

table <- table(data$nQ55_r1, data$nQ88_r1)
table

round(prop.table(table, margin = 2)*100, 0)


data %>% 
  filter(!is.na(nQ55_r1) & !is.na(nQ88_r1)) %>% 
  nrow() #547


data %>% 
  count(nQ55_r1, nQ88_r1) %>% 
  na.omit() %>%
  group_by(nQ88_r1) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = nQ88_r1, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  labs(x = "", y = "", fill = "", 
       title = "Dodržení abstinence x pohlaví",
       subtitle = "N = 547")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))


##########vek x dodrzeni 

table <- table(data$nQ55_r1, data$vek5)
table

round(prop.table(table, margin = 2)*100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  filter(!is.na(nQ55_r1) & !is.na(vek5)) %>% 
  nrow() #547


data %>% 
  count(nQ55_r1, vek5) %>% 
  na.omit() %>%
  group_by(vek5) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = vek5, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  labs(x = "", y = "", fill = "", 
       title = "Dodržení abstinence x věk",
       subtitle = "N = 547")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##nejmladsi casteji abstinovali dele, nez planovali 
##30-39 mene castejni nemeli naplanovanou delku a dodrzeli v delce, kterou naplanovali
##50-59 mene casteji abstinovali dele, nez planovali a casteji ukoncili drive



#########vzdelani x dodrzeni 


table <- table(data$nQ55_r1, data$vzd4)
table

round(prop.table(table, margin = 2)*100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  filter(!is.na(nQ55_r1) & !is.na(vzd4)) %>% 
  nrow() #547


data %>% 
  count(nQ55_r1, vzd4) %>% 
  na.omit() %>%
  group_by(vzd4) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = vzd4, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  labs(x = "", y = "", fill = "", 
       title = "Dodržení abstinence x vzdělání",
       subtitle = "N = 547")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##se zakladnim abstinovali dele, nez si naplanovali



#######prijem x dodrzeni

table <- table(data$nQ55_r1, data$prijem_osob)
table

round(prop.table(table, margin = 2)*100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  filter(!is.na(nQ55_r1) & !is.na(prijem_osob)) %>% 
  nrow() #392


data %>% 
  count(nQ55_r1, prijem_osob) %>% 
  na.omit() %>%
  group_by(prijem_osob) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = prijem_osob, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  labs(x = "", y = "", fill = "", 
       title = "Dodržení abstinence x příjem",
       subtitle = "N = 392")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))

##lide s nejnizsim prijmem casteji abstinovali dele, nez si naplanovali 




#######piti po abstinenci x dodrzeni

table <- table(data$nQ55_r1, data$nQ63_r1)
table

round(prop.table(table, margin = 2)*100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  filter(!is.na(nQ55_r1) & !is.na(nQ63_r1)) %>% 
  nrow() #547


data %>% 
  count(nQ55_r1, nQ63_r1) %>% 
  na.omit() %>%
  group_by(nQ63_r1) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = nQ63_r1, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  labs(x = "", y = "", fill = "", 
       title = "Dodržení abstinence x pití po asbtinenci",
       subtitle = "N = 547")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        axis.text.y = element_text(hjust = 1, size = 6)) +
  guides(fill = guide_legend(nrow = 2))

##lide, kteri po asbtinenci snizili trvale spotrebu tak mene casto ukoncili abstinenci drive
##lide, kteri pili mene ale vratili se k puvodni spotrebe casteji ukoncili asbtinenci drive
##lide, kteri pili alkohol podobne jako pred abstinenci casteji nemeli naplanovanou delku a mene casto abstinovali dele 
## u kategorie pili vice je malo pozorovani 


# 4) Pití po krátkodobé abstinenci  ---------------------------------------

#nQ63_r1

sum(!is.na(data$nQ63_r1)) #695

data %>%
  filter(!is.na(nQ63_r1)) %>% 
  count(nQ63_r1) %>%
  mutate(perc = n / sum(n) * 100)


data %>%
  filter(!is.na(nQ63_r1)) %>% 
  count(nQ63_r1) %>%
  mutate(procento = n / sum(n) * 100,
         nQ63_r1 = reorder(nQ63_r1, procento)) %>%
  ggplot(aes(x = nQ63_r1, y = procento)) +
  geom_col(fill = "#FF7F50") +
  geom_text(aes(label = paste0(" ", round(procento, 0), "%")), 
            hjust = 0.5, size = 3.5, fontface = "bold")+
  theme_minimal() +
  coord_flip() +
  labs(title = "Pití po abstinenci", x = "", y = "", subtitle = paste("N = 695"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(angle = 20, hjust = 1, size = 7.5))



########piti po abstinenci x vek 

table <- table(data$nQ63_r1, data$vek5)
table

round(prop.table(table, margin = 2)*100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)

data %>% 
  filter(!is.na(nQ63_r1) & !is.na(vek5)) %>% 
  nrow() #695


data %>% 
  count(nQ63_r1, vek5) %>% 
  na.omit() %>%
  group_by(vek5) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = vek5, y = perc, fill = nQ63_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  labs(x = "", y = "", fill = "", 
       title = "Pití po asbtinenci x věk",
       subtitle = "N = 695")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 4))

