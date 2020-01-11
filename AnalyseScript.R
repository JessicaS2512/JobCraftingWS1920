# AnalyseScript.R
## Teamname: Job Crafting

# Pakete laden: ----
library(tidyverse)
source("data/qualtricshelpers.R") 
library(psych)
library(ggplot2)
library(jmv)
library(devtools)
# Rohdaten laden: ----
raw <- read_csv("job_crafting.csv")

# DataCleaning und Skalenberechnung: ----

raw <- load_qualtrics_csv("job_crafting.csv")

filename <- "job_crafting.csv" 

names(raw)

raw <- filter(raw, Status == "0")

raw_short <- raw %>% select(gender, age, education, activity, regfoc_1, regfoc_2, regfoc_3, regfoc_4, regfoc_5, regfoc_6, regfoc_7, regfoc_8, jc_scen1_question_1, jc_scen1_question_2, jc_scen1_question_3, jc_scen1_question_4, jc_scen1_question_5, jc_scen1_question_6, jc_scen1_question_7, jc_scen1_question_8, jc_scen1_question_9, jc_scen2_question_1, jc_scen2_question_2, jc_scen2_question_3, jc_scen2_question_4, jc_scen2_question_5, jc_scen2_question_6, jc_scen2_question_7, jc_scen2_question_8, jc_scen2_question_9)

raw_short$education <- ordered(raw_short$education, levels = c("Kein Schulabschluss", "Volks- oder Hauptschulabschluss", "Mittlere Reife/Realschulabschluss", "Fachhochschulreife/Fachabitur", "Allgemeine Hochschulreife/Abitur", "Abgeschlossene Berufsausbildung", "Bachelor", "Master", "Promotion", "Habilitation", "Keiner der hier genannten"))

raw_short$activity <- ordered(raw_short$activity, levels = c("Schüler", "Student", "Auszubildender", "Angestellter", "Selbstständiger", "Rentner", "Arbeitssuchender"))

raw_short$regfoc_1 <- as.numeric(raw_short$regfoc_1)
raw_short$regfoc_2 <- as.numeric(raw_short$regfoc_2)
raw_short$regfoc_3 <- as.numeric(raw_short$regfoc_3)
raw_short$regfoc_4 <- as.numeric(raw_short$regfoc_4)
raw_short$regfoc_5 <- as.numeric(raw_short$regfoc_5)
raw_short$regfoc_6 <- as.numeric(raw_short$regfoc_6)
raw_short$regfoc_7 <- as.numeric(raw_short$regfoc_7)
raw_short$regfoc_8 <- as.numeric(raw_short$regfoc_8)

raw_short$jc_scen1_question_1 <- as.numeric(raw_short$jc_scen1_question_1)
raw_short$jc_scen1_question_2 <- as.numeric(raw_short$jc_scen1_question_2)
raw_short$jc_scen1_question_3 <- as.numeric(raw_short$jc_scen1_question_3)
raw_short$jc_scen1_question_4 <- as.numeric(raw_short$jc_scen1_question_4)
raw_short$jc_scen1_question_5 <- as.numeric(raw_short$jc_scen1_question_5)
raw_short$jc_scen1_question_6 <- as.numeric(raw_short$jc_scen1_question_6)
raw_short$jc_scen1_question_7 <- as.numeric(raw_short$jc_scen1_question_7)
raw_short$jc_scen1_question_8 <- as.numeric(raw_short$jc_scen1_question_8)
raw_short$jc_scen1_question_9 <- as.numeric(raw_short$jc_scen1_question_9)

raw_short$jc_scen2_question_1 <- as.numeric(raw_short$jc_scen2_question_1)
raw_short$jc_scen2_question_2 <- as.numeric(raw_short$jc_scen2_question_2)
raw_short$jc_scen2_question_3 <- as.numeric(raw_short$jc_scen2_question_3)
raw_short$jc_scen2_question_4 <- as.numeric(raw_short$jc_scen2_question_4)
raw_short$jc_scen2_question_5 <- as.numeric(raw_short$jc_scen2_question_5)
raw_short$jc_scen2_question_6 <- as.numeric(raw_short$jc_scen2_question_6)
raw_short$jc_scen2_question_7 <- as.numeric(raw_short$jc_scen2_question_7)
raw_short$jc_scen2_question_8 <- as.numeric(raw_short$jc_scen2_question_8)
raw_short$jc_scen2_question_9 <- as.numeric(raw_short$jc_scen2_question_9)

scale.zustimmung <- c("Stimme völlig zu", "2", "3", "4", "5", "Stimme gar nicht zu")

raw_short$regfoc_1 <- ordered(raw_short$regfoc_1, levels = scale.zustimmung)
raw_short$regfoc_2 <- ordered(raw_short$regfoc_2, levels = scale.zustimmung)
raw_short$regfoc_3 <- ordered(raw_short$regfoc_3, levels = scale.zustimmung)
raw_short$regfoc_4 <- ordered(raw_short$regfoc_4, levels = scale.zustimmung)
raw_short$regfoc_5 <- ordered(raw_short$regfoc_5, levels = scale.zustimmung)
raw_short$regfoc_6 <- ordered(raw_short$regfoc_6, levels = scale.zustimmung)
raw_short$regfoc_7 <- ordered(raw_short$regfoc_7, levels = scale.zustimmung)
raw_short$regfoc_8 <- ordered(raw_short$regfoc_8, levels = scale.zustimmung)

raw_short$jc_scen1_question_1 <- ordered(raw_short$jc_scen1_question_1, levels = scale.zustimmung)
raw_short$jc_scen1_question_2 <- ordered(raw_short$jc_scen1_question_2, levels = scale.zustimmung)
raw_short$jc_scen1_question_3 <- ordered(raw_short$jc_scen1_question_3, levels = scale.zustimmung)
raw_short$jc_scen1_question_4 <- ordered(raw_short$jc_scen1_question_4, levels = scale.zustimmung)
raw_short$jc_scen1_question_5 <- ordered(raw_short$jc_scen1_question_5, levels = scale.zustimmung)
raw_short$jc_scen1_question_6 <- ordered(raw_short$jc_scen1_question_6, levels = scale.zustimmung)
raw_short$jc_scen1_question_7 <- ordered(raw_short$jc_scen1_question_7, levels = scale.zustimmung)
raw_short$jc_scen1_question_8 <- ordered(raw_short$jc_scen1_question_8, levels = scale.zustimmung)
raw_short$jc_scen1_question_9 <- ordered(raw_short$jc_scen1_question_9, levels = scale.zustimmung)

raw_short$jc_scen2_question_1 <- ordered(raw_short$jc_scen2_question_1, levels = scale.zustimmung)
raw_short$jc_scen2_question_2 <- ordered(raw_short$jc_scen2_question_2, levels = scale.zustimmung)
raw_short$jc_scen2_question_3 <- ordered(raw_short$jc_scen2_question_3, levels = scale.zustimmung)
raw_short$jc_scen2_question_4 <- ordered(raw_short$jc_scen2_question_4, levels = scale.zustimmung)
raw_short$jc_scen2_question_5 <- ordered(raw_short$jc_scen2_question_5, levels = scale.zustimmung)
raw_short$jc_scen2_question_6 <- ordered(raw_short$jc_scen2_question_6, levels = scale.zustimmung)
raw_short$jc_scen2_question_7 <- ordered(raw_short$jc_scen2_question_7, levels = scale.zustimmung)
raw_short$jc_scen2_question_8 <- ordered(raw_short$jc_scen2_question_8, levels = scale.zustimmung)
raw_short$jc_scen2_question_9 <- ordered(raw_short$jc_scen2_question_9, levels = scale.zustimmung)

schluesselliste <- list(REGFOC = c("-regfoc_1", "-regfoc_2", "-regfoc_3", "-regfoc_4", "regfoc_5", "regfoc_6", "regfoc_7", "regfoc_8"), JC_SCEN1 = c("-jc_scen1_question_1", "-jc_scen1_question_2", "-jc_scen1_question_3", "-jc_scen1_question_4", "-jc_scen1_question_5", "-jc_scen1_question_6", "jc_scen1_question_7", "jc_scen1_question_8", "jc_scen1_question_9"), JC_SCEN2 = c("-jc_scen2_question_1", "-jc_scen2_question_2", "-jc_scen2_question_3", "-jc_scen2_question_4", "-jc_scen2_question_5", "-jc_scen2_question_6", "jc_scen2_question_7", "jc_scen2_question_8", "jc_scen2_question_9"))

scoreItems(schluesselliste, raw_short)

scores <- scoreItems(schluesselliste, raw_short, missing = TRUE, min = 1, max = 6)

raw_short %>% bind_cols(as_tibble(scores$scores))

datensatz <- raw_short %>% bind_cols(as_tibble(scores$scores))

datensatz <- datensatz %>%
  select(-starts_with("regfoc", ignore.case = F)) %>%
  select(-starts_with("jc_scen1", ignore.case = F)) %>%
  select(-starts_with("jc_scen2", ignore.case = F))

saveRDS(datensatz, "data/datensatz.rds")
# Statistische Analyse und Grafiken:----

# Errechnen des Medians, um den regulatorischen Fokus in zwei Gruppen (prevention/promotion) aufzuteilen
median(datensatz$REGFOC)

#Der Median liegt bei 3.125. Das heißt wir erstellen anhand dessen zwei Gruppen (aufgeteilt in promotion/prevention). Also von 1 bis 3.125 prevention und von 3.125 bis 6 promotion. 

#Für die Unterschiedshypothesen: Filtern des REGFOC in prev/prom, um eine faktorielle Variable zu erhalten.
#ich habe prev größer gleich 3.125 gesetzt, denn wenn man es andersherum macht (prom kleiner gleich 3.125) sind die Gruppen sehr unterschiedlich groß.


# HYPOTHESE 1 - unverbundener t-test
t.test(filter(datensatz, REGFOC >= 3.125)$JC_SCEN2, filter(datensatz, REGFOC < 3.125)$JC_SCEN2)

#Bericht: t =  3.5491, df = 211.49, p-value = 0.0004761
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# 0.05751134 0.20120697
#sample estimates:
#mean of x mean of y 
#3.687764  3.558405 

# da p = .0004 ist, kann H1 angenommen werden (Es gibt einen signifikanten Unterschied im JC von prev/prom bei schlechter Veränderungskommunikation)

# Visualisierung Hypothese 1 - regfoc in zwei Gruppen teilen für Visualisierung

regfoc_groups <- group_by(datensatz, REGFOC >= 3.125)

as.factor(regfoc_groups)

# x-Achse FALSE = prevention, TRUE = promotion

library(ggplot2)
library(plotrix)
library(devtools)

regfoc_groups %>%
  group_by(`REGFOC >= 3.125`) %>%
  summarise(Mean = mean(JC_SCEN2, na.rm = TRUE)-1, sem = std.error(JC_SCEN2))%>%
  ggplot() +
  aes(x = `REGFOC >= 3.125`, weight = Mean, ymin = Mean - sem, ymax = Mean + sem) +
  geom_bar(fill = "#0c4c8a") +
  geom_errorbar(width = 0.2) +
  scale_y_continuous(limits = c(0,5)) +
  labs(x = "regulatorischer Fokus größer gleich 3.125", 
       y = "Job Crafting bei schlechter Kommunikation", 
       title = "title", 
       subtitle = "subtitle", 
       caption = "caption") +
 theme_minimal() +
NULL

# diese andere Variante des t-test ergibt den gleichen p-Wert 
regfoc_groups %>%
  t.test(data = ., JC_SCEN2~`REGFOC >= 3.125`)

