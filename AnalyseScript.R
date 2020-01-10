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
median(datensatz$REGFOC)

#Der Median liegt bei 3.5. Das heißt wir erstellen anhand dessen zwei Gruppen (aufgeteilt in promotion/prevention). Also von 1 bis 3.5 prevention und von 3.5 bis 6 promotion. 


