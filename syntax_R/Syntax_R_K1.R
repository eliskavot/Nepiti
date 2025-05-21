#Syntax K1
#Nepiti - analyza D01
#-------------------------------------------------------------------------------

# Packages ----------------------------------------------------------------

packages <- c("tidyverse", "haven", "DescTools", "GGally", "skimr", "dplyr",
              "psych", "car")

purrr::walk(packages, library, character.only = TRUE)

library(labelled)

# Data --------------------------------------------------------------------
data <- read_sav(file = "Data/Alkohol 2025_v01.sav") %>%    
  mutate(across(where(is.labelled), as_factor))

data_labelled <- generate_dictionary(data)


# 1)kratk. abst. ------------------------------------------------------------
#martin (ne)smrdi

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









####### > 3 tydny x kolik piji (celk_spotr3_5kat)

table(data$celk_spotr3_5kat)

sum(!is.na(data$celk_spotr3_5kat)) #1011


table <- table(data$nQ51_r1, data$celk_spotr3_5kat)
round(prop.table(table, margin = 2) * 100, 0)

chisq_result <- chisq.test(table) 
chisq_result$stdres 
p_value <- chisq_result$p.value
print(p_value) 
chisq_value <- chisq_result$statistic
print(chisq_value) 
CramerV(table)



data %>% 
  filter(!is.na(nQ51_r1) & !is.na(celk_spotr3_5kat)) %>% 
  nrow() 

data %>% 
  count(nQ51_r1, celk_spotr3_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr3_5kat) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = celk_spotr3_5kat, y = perc, fill = nQ51_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 2, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+
  coord_flip() +
  labs(x = "Počet sklenic", y = "", fill = "", title = "Abstinence > 3 týdny x spotřeba",
       subtitle = "N = 1011")








####### < 3 tydny x kolik piji (celk_spotr3_5kat)

sum(!is.na(data$nQ52_r1)) #397

table <- table(data$nQ52_r1, data$celk_spotr3_5kat)
table

round(prop.table(table, margin = 2) * 100, 0)

data %>% 
  filter(!is.na(nQ52_r1) & !is.na(celk_spotr3_5kat)) %>% 
  nrow() #395

levels(data$celk_spotr3_5kat)

data %>% 
  count(nQ52_r1, celk_spotr3_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr3_5kat) %>%
  mutate(perc = n / sum(n))%>%
  ggplot(aes(x = celk_spotr3_5kat, y = perc, fill = nQ52_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 2, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+
  coord_flip() +
  labs(x = "Počet sklenic", y = "", fill = "", title = "Abstinence < 3 týdny x spotřeba",
       subtitle = "N = 395")








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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Pohlaví", y = "", fill = "", title = "Abstinence > 3 týdny x pohlaví",
       subtitle = "N = 1022")

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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Věk", y = "", fill = "", title = "Abstinence > 3 týdny x věk",
       subtitle = "N = 1022")


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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Vzdělání", y = "", fill = "", title = "Abstinence > 3 týdny x vzdělání",
       subtitle = "N = 1022")

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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Příjem", y = "", fill = "", title = "Abstinence > 3 týdny x příjem",
       subtitle = "N = 747")






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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Pohlaví", y = "", fill = "Pohlaví", title = "Abstinence < 3 týdny x pohlaví",
       subtitle = "N = 397")

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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Věk", y = "", fill = "", title = "Abstinence < 3 týdny x věk",
       subtitle = "N = 397")

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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Vzdělání", y = "", fill = "", title = "Abstinence < 3 týdny x vzdělání",
       subtitle = "N = 397")


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
            size = 2, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Příjem", y = "", fill = "", title = "Abstinence < 3 týdny x příjem",
       subtitle = "N = 747")







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



## kolik piji x delka abstinence 

data %>% 
  filter(!is.na(celk_spotr3_5kat)) %>% 
  group_by(celk_spotr3_5kat) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE))

data %>% 
  filter(!is.na(tQ54_0_0_num) & !is.na(celk_spotr3_5kat)) %>% 
  nrow() #484


data %>% 
  filter(!is.na(celk_spotr3_5kat)) %>% 
  ggplot(mapping = aes(x = celk_spotr3_5kat, y = tQ54_0_0_num))+
  geom_boxplot(width = 0.3, fill = "#FF7F00")+
  theme_minimal()+
  labs(title = "Spotřeba x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 484")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  scale_y_log10()



data %>% 
  filter(!is.na(celk_spotr3_5kat)) %>% 
  ggplot(aes(x = celk_spotr3_5kat, y = tQ54_0_0_num)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Spotřeba x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 484")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))


##zpusob abstinence x delka abstinence

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

data %>% 
  filter(!is.na(nQ52_r1)) %>% 
  group_by(nQ52_r1) %>% 
  summarise(across(.cols = tQ54_0_0_num,
                   .fns = median, na.rm = TRUE)) ## u kratsi abstinence nez 3 tydny jsou nesmyslne odpovedi > nez 3 tydny



## vek x délka abstinence 

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



##pohlavi x délka abstinence 

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



##vzdelani x delka abstinence 

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


##prijem x delka abstinence

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
  labs(title = "Vzdělání x délka abstinence", x = "", y = "Týdny",
       subtitle = paste("N: 381")) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))



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




##spotreba x dodrzeni 

table <- table(data$nQ55_r1, data$celk_spotr3_5kat)
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
  filter(!is.na(nQ55_r1) & !is.na(celk_spotr3_5kat)) %>% 
  nrow() #539


data %>% 
  count(nQ55_r1, celk_spotr3_5kat) %>% 
  na.omit() %>%
  group_by(celk_spotr3_5kat) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = celk_spotr3_5kat, y = perc, fill = nQ55_r1)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(perc, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Accent")+
  labs(x = "Počet sklenic", y = "", fill = "", title = "Dodržení abstinence x počet sklenic",
       subtitle = "N = 539")


##pohlavi x dodrzeni 

table <- table(data$nQ55_r1, data$nQ88_r1)
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
       subtitle = "N = 547")


# Otevrene otazky - kodovani ----------------------------------------------

library(writexl)

odpovedi_tQ57_0_0 <- data %>% 
  select(respondent_id_internal, tQ57_0_0) %>% 
  filter(tQ57_0_0 != "")
write_xlsx(odpovedi_tQ57_0_0, "odpovedi_tQ57_0_0.xlsx")

OE_all <- data %>% 
  select(respondent_id_internal, tQ57_0_0, tQ60_0_0, tQ62_0_0, tQ64_0_0, tQ70_0_0, 
         tQ74_0_0, tQ76_0_0, tQ78_0_1, tQ78_1_1)
write_xlsx(OE_all, "odpovedi_all.xlsx")

OE_tQ60_0_0<- data %>% 
  select(respondent_id_internal, tQ60_0_0) %>% 
  filter(tQ60_0_0 != "")
write_xlsx(OE_tQ60_0_0, "OE_tQ60_0_0.xlsx")

OE_tQ62_0_0<- data %>% 
  select(respondent_id_internal, tQ62_0_0) %>% 
  filter(tQ62_0_0 != "")
write_xlsx(OE_tQ62_0_0, "OE_tQ62_0_0.xlsx")

OE_tQ64_0_0<- data %>% 
  select(respondent_id_internal, tQ64_0_0) %>% 
  filter(tQ64_0_0 != "")
write_xlsx(OE_tQ64_0_0, "OE_tQ64_0_0.xlsx")

OE_tQ70_0_0<- data %>% 
  select(respondent_id_internal, tQ70_0_0) %>% 
  filter(tQ70_0_0 != "")
write_xlsx(OE_tQ70_0_0, "OE_tQ70_0_0.xlsx")

OE_tQ74_0_0<- data %>% 
  select(respondent_id_internal, tQ74_0_0) %>% 
  filter(tQ74_0_0 != "")
write_xlsx(OE_tQ74_0_0, "OE_tQ74_0_0.xlsx")

OE_tQ76_0_0<- data %>% 
  select(respondent_id_internal, tQ76_0_0) %>% 
  filter(tQ76_0_0 != "")
write_xlsx(OE_tQ76_0_0, "OE_tQ76_0_0.xlsx")

OE_tQ78_0_1<- data %>% 
  select(respondent_id_internal, tQ78_0_1) %>% 
  filter(tQ78_0_1 != "")
write_xlsx(OE_tQ78_0_1, "OE_tQ78_0_1.xlsx")

OE_tQ78_1_1<- data %>% 
  select(respondent_id_internal, tQ78_1_1) %>% 
  filter(tQ78_1_1 != "")
write_xlsx(OE_tQ78_1_1, "OE_tQ78_1_1.xlsx")


# skaly a indexy ----------------------------------------------------------
library(psych)

sloupce_nQ59 <- grep("^nQ59", names(data), value = TRUE)

# Vytvoří podmnožinu dat jen s těmito sloupci
sloupce_nQ59 <- grep("^nQ59", names(data), value = TRUE)
data_nQ59 <- data[, sloupce_nQ59]
describe(data_nQ59)

select(data$nQ59_0_1)
table(data$nQ59_0_1)

