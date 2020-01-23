# AnalyseScript.R
## Teamname: Job Crafting

# Pakete laden: ----
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
source("data/qualtricshelpers.R") 
library(psych)
library(ggplot2)
library(jmv)
library(devtools)
library(plotrix)
library(ggthemes)
library(questionr)

devtools::install_github("HCIC/r-tools")

rwthfarben <- hcictools::rwth.colorpalette()
# Rohdaten laden: ----
raw <- read_csv("job_crafting.csv")

# DataCleaning und Skalenberechnung: ----

raw <- read_csv("job_crafting.csv")

filename <- "job_crafting.csv" 

names(raw)

raw <- filter(raw, Status == "0")

raw_short <- raw %>% select(gender, age, education, activity,
                            regfoc_1, regfoc_2, regfoc_3, regfoc_4, regfoc_5, regfoc_6, regfoc_7,
                            jc_scen1_question_1, jc_scen1_question_2, jc_scen1_question_3, jc_scen1_question_4, jc_scen1_question_5, jc_scen1_question_6, jc_scen1_question_7, jc_scen1_question_8, jc_scen1_question_9,
                            jc_scen2_question_1, jc_scen2_question_2, jc_scen2_question_3, jc_scen2_question_4, jc_scen2_question_5, jc_scen2_question_6, jc_scen2_question_7, jc_scen2_question_8, jc_scen2_question_9)

raw_short$education <- ordered(raw_short$education, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

raw_short$activity <- ordered(raw_short$activity, levels = c(1, 2, 3, 4, 5, 6, 7))

raw_short$age <- as.numeric(raw_short$age)

raw_short$regfoc_1 <- as.numeric(raw_short$regfoc_1)
raw_short$regfoc_2 <- as.numeric(raw_short$regfoc_2)
raw_short$regfoc_3 <- as.numeric(raw_short$regfoc_3)
raw_short$regfoc_4 <- as.numeric(raw_short$regfoc_4)
raw_short$regfoc_5 <- as.numeric(raw_short$regfoc_5)
raw_short$regfoc_6 <- as.numeric(raw_short$regfoc_6)
raw_short$regfoc_7 <- as.numeric(raw_short$regfoc_7)

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

scale.zustimmung <- c(1, 2, 3, 4, 5, 6)

raw_short$regfoc_1 <- ordered(raw_short$regfoc_1, levels = scale.zustimmung)
raw_short$regfoc_2 <- ordered(raw_short$regfoc_2, levels = scale.zustimmung)
raw_short$regfoc_3 <- ordered(raw_short$regfoc_3, levels = scale.zustimmung)
raw_short$regfoc_4 <- ordered(raw_short$regfoc_4, levels = scale.zustimmung)
raw_short$regfoc_5 <- ordered(raw_short$regfoc_5, levels = scale.zustimmung)
raw_short$regfoc_6 <- ordered(raw_short$regfoc_6, levels = scale.zustimmung)
raw_short$regfoc_7 <- ordered(raw_short$regfoc_7, levels = scale.zustimmung)

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

schluesselliste <- list(REGFOC = c("-regfoc_1", "-regfoc_2", "-regfoc_3", "-regfoc_4", "regfoc_5", "regfoc_6", "regfoc_7"),
                        JC_SCEN1 = c("-jc_scen1_question_1", "-jc_scen1_question_2", "-jc_scen1_question_3", "-jc_scen1_question_4", "-jc_scen1_question_5", "-jc_scen1_question_6", "jc_scen1_question_7", "jc_scen1_question_8", "jc_scen1_question_9"),
                        JC_SCEN2 = c("-jc_scen2_question_1", "-jc_scen2_question_2", "-jc_scen2_question_3", "-jc_scen2_question_4", "-jc_scen2_question_5", "-jc_scen2_question_6", "jc_scen2_question_7", "jc_scen2_question_8", "jc_scen2_question_9"),
                        HILFE_JC_SCEN1 = c("-jc_scen1_question_1", "-jc_scen1_question_2", "-jc_scen1_question_3"),
                        HILFE_JC_SCEN2 = c("-jc_scen2_question_1", "-jc_scen2_question_2", "-jc_scen2_question_3"),
                        HERAUSF_JC_SCEN1 = c("-jc_scen1_question_4", "-jc_scen1_question_5", "-jc_scen1_question_6"),
                        HERAUSF_JC_SCEN2 = c("-jc_scen2_question_4", "-jc_scen2_question_5", "-jc_scen2_question_6"),
                        ANFORD_JC_SCNEN1 = c("jc_scen1_question_7", "jc_scen1_question_8", "jc_scen1_question_9"),
                        ANFORD_JC_SCNEN2 = c("jc_scen2_question_7", "jc_scen2_question_8", "jc_scen2_question_9"),
                        PRO = c("-regfoc_1", "-regfoc_2", "-regfoc_3", "-regfoc_4"),
                        PRE = c("-regfoc_5", "-regfoc_6", "-regfoc_7"))

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

# Deskriptive Statistik zum Alter und Geschlecht:
datensatz %>% select(age, gender) %>% psych::describe()

#            n      mean      sd     median   min   max   range    se
#age        399     31.01   12.95     25       18    81     63    0.65
#gender     399     1.62     0.49      2        1    3      2     0.02

# Histogramm zur Altersverteilung:
datensatz %>%
  select(age) %>%
  ggplot() +
  aes(x = age) +
  geom_histogram(binwidth = 1) +
  labs(x = "Alter in Jahren", 
       y = "Häufigkeit", 
       title = "Junge, leicht bimodal verteilte Stichprobe", 
       subtitle = "Histogramm zur Altersverteilung (n=399)", 
       caption = "binwidth = 1") +
  theme_minimal() +
  NULL

# Der folgende Code zählt, wie viele Personen männlich bzw. weiblich waren:

datensatz %>% 
  group_by(gender) %>%
  summarise(gender_count = n())

#Code zählt höchsten Bildungsabschluss
datensatz%>%
  group_by(education) %>%
  summarise(education_count = n())

#Code zählt Tätigkeit
datensatz%>%
  group_by(activity) %>%
  summarise(activity_count = n())

# Der folgende Code zählt, wie viele Leute einen Prevention Focus bzw. keinen Prevention Focus haben:
datensatz %>% 
  group_by(prevention_category) %>%
  summarise(prevention_category_count = n())

# Der folgende Code zählt, wie viele Leute einen Promotion Focus bzw. keinen Promotion Focus haben:
datensatz %>% 
  group_by(promotion_category) %>%
  summarise(promotion_category_count = n())

# ALT Hypothese 1
# Formulierung: Prevention focussed Menschen haben mehr Job Crafting bei schlechter Kommunikation als nicht-prevention focussed Menschen

## Cutting datensatz$PRO into datensatz$PRO_mediansplit
# datensatz$PRO_mediansplit <- cut(datensatz$PRO, include.lowest=TRUE,  right=TRUE,
#                                 breaks=c(1.25, 4.75, 6))
## Cutting datensatz$PRE into datensatz$PRE_mediansplit
# datensatz$PRE_mediansplit <- cut(datensatz$PRE, include.lowest=TRUE,  right=TRUE,
#                                 breaks=c(1, 5.66666666666667, 6))

# res2 <- ANOVA(datensatz, dep = "JC_SCEN2", factors = c("PRE_mediansplit", "PRO_mediansplit"),
#              emMeans = list(c("PRE_mediansplit", "PRO_mediansplit")))
# res2$main
# res2$emm
# datensatz%>%
#  group_by(PRO_mediansplit, PRE_mediansplit) 
# ggplot()+
#  aes(x = PRO_mediansplit)


# ANOVA(datensatz, dep = "JC_SCEN2", factors ="PRE_mediansplit", 
#      postHoc = JC_SCEN2 ~ PRE_mediansplit)

# der mittlere Unterschied von -0.124 ist signifikant

# NEU Mediansplit:

median(datensatz$PRE)
median(datensatz$PRO)

datensatz <- datensatz %>%
  mutate(prevention_category = case_when(PRE > 5.666667 ~ "Prevention Focus",
                                          TRUE ~ "Kein Prevention Focus"))

datensatz <- datensatz %>%
  mutate(promotion_category = case_when(PRO > 4.75 ~ "Promotion Focus",
                                         TRUE ~ "Kein Promotion Focus"))

# NEU Hypothese 1: Personen mit einem prevention focus haben ein höheres Job Crafting bei qualitativ minderwertiger 
# Kommunikation von organisatorischen Veränderungen in einem Unternehmen als Personen ohne prevention focus.

t.test(datensatz$JC_SCEN2 ~datensatz$prevention_category)

# Bericht Hypothese 1: In der Stichprobe haben Personen mit Prevention Focus (M=3.8) ein höherens Job Crafting als 
# Personen ohne Prevention Focus (M=3.68). Dieser Unterschied ist signifikant (t(391.53)=-2.14, p < .05) und liegt 
# mit 95% Sicherheit zwischen -0.24 und -0.01 Punkten einer 6-stufigen Skala.

# Visualisierung:
  
datensatz %>%
  group_by(prevention_category) %>%
  summarise(JC_SCEN2_m = mean(JC_SCEN2)-1, JC_SCEN2_sem = std.error(JC_SCEN2)) %>%
  ggplot() +
  aes(x = prevention_category, weight = JC_SCEN2_m, ymin = JC_SCEN2_m - JC_SCEN2_sem, ymax = JC_SCEN2_m + JC_SCEN2_sem, fill = prevention_category) +
  geom_bar(fill = c(rwthfarben$lightblue, rwthfarben$red), width = 0.4) +
  geom_errorbar(width = 0.2) +
  scale_y_continuous(limits = c(0,5)) +
  labs(x = "Prevention Focus",
       y = "Job Crafting bei \nschlechter Kommunikation [0-5]",
       title = "Personen mit Prevention Focus haben ein \nleicht höheres Job Crafting als Personen \nohne Prevention Focus.",
       subtitle = "Unterschiede im Säulendiagramm [n=433]",
       caption = "Fehlerindikatoren zeigen Standardfehler des Mittelwerts.") +
  theme_minimal() +
  NULL

# NEU Hypothese 2: Personen mit einem Promotion Focus haben ein höheres Job Crafting bei qualitativ 
# hochwertiger Kommunikation von organisatorischen Veränderungen als Personen ohne Promotion Focus.

t.test(datensatz$JC_SCEN1 ~datensatz$promotion_category)

# Bericht: In der Stichprobe haben Personen mit Promotion Focus (M=3.98) ein höheres Job Crafting als
# Personen ohne Promotion Focus (M=3.83). Dieser Unterschied ist signifikant (t(345.65)=-2.66, p < .01)
# und liegt mit 95% Sicherheit zwischen -0.26 und -0.04 Punkten einer 6-stufigen Skala.


# Visualisierung:

datensatz %>%
  group_by(promotion_category) %>%
  summarise(JC_SCEN1_m = mean(JC_SCEN1)-1, JC_SCEN1_sem = std.error(JC_SCEN1)) %>%
  ggplot() +
  aes(x = promotion_category, weight = JC_SCEN1_m, ymin = JC_SCEN1_m - JC_SCEN1_sem, ymax = JC_SCEN1_m + JC_SCEN1_sem, fill = promotion_category) +
  geom_bar(fill = c(rwthfarben$lightblue, rwthfarben$red), width = 0.4) +
  geom_errorbar(width = 0.2) +
  scale_y_continuous(limits = c(0,5)) +
  labs(x = "Promotion Focus",
       y = "Job Crafting bei \nguter Kommunikation [0-5]",
       title = "Personen mit Promotion Focus haben ein \nhöheres Job Crafting als Personen \nohne Promotion Focus.",
       subtitle = "Unterschiede im Säulendiagramm [n=433]",
       caption = "Fehlerindikatoren zeigen Standardfehler des Mittelwerts.") +
  theme_minimal() +
  NULL

# Hypothese 6: Je höher der prevention Fokus, desto höher ist das Job Crafting bei qualitativ minderwertiger Kommunikation
# von organisatorischen Veränderungen in einem Unternehmen.

cor(datensatz$PRE, datensatz$JC_SCEN2, method = "pearson")
cor.test(datensatz$PRE, datensatz$JC_SCEN2)

# Visualisierung Hypothese 6:
datensatz %>%
  ggplot() +
  aes(x = JC_SCEN2, y = PRE) +
  geom_jitter(alpha = 0.25) +
  geom_smooth(method = lm, color = "black") +
 theme_minimal() +
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Job Crafting bei schlechter Kommunikation",
       y = "Prevention Focus" [1-6],
       title = "Es gibt einen schwachen Zusammenhang \nzwischen dem Job Crafting und dem \nPrevention Focus.",
       subtitle = "Pearson-Korrelation im Streudiagramm") +
  NULL

# Bericht Hypothese 6: Es gibt einen signifikanten Zusammenhang zwischen dem Prevention focus und dem Job Crafting bei
# qualitativ minderweriger Kommunikation ($r = 0.102$, $p<.05$). Das bedeutet: Je höher der Prevention focus, desto 
# höher das Job Crafting bei qualitativ minderwertiger Kommunikation.

# Hypothese 7: Je höher der promotion Fokus, desto höher ist das Job Crafting bei qualitativ hochwertiger 
# Kommunikation von Veränderungen in einem Unternehmen.

cor(datensatz$PRO, datensatz$JC_SCEN1, method = "pearson")
cor.test(datensatz$PRO, datensatz$JC_SCEN1)

# Visualisierung Hypothese 7:
datensatz %>%
  ggplot() +
  aes(x = JC_SCEN1, y = PRO) +
  geom_jitter(alpha = 0.25, width = 0.1, height = 0.1) +
  geom_smooth(method = lm, color = "black") +
  theme_minimal() +
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Job Crafting bei guter Kommunikation",
       y = "Promotion Focus" [1-6],
       title = "Es gibt einen Zusammenhang zwischen dem \nJob Crafting und dem Promotion Focus.",
       subtitle = "Pearson-Korrelation im Streudiagramm") +
  NULL

# Bericht Hypothese 7: Es gibt einen signifikanten Zusammenhang zwischen dem Promotion focus und dem Job Crafting bei
# qualitativ hochwertiger Kommunikation ($r = 0.216$, $p<.001$). Das bedeutet: Je höher der Promotion focus, desto 
# höher das Job Crafting bei qualitativ hochwertiger Kommunikation.

#Hypothese 9 - Job Crafting bei qualitativ minderwertiger Kommunikation von organisatorischen Veränderungen 
#in einem Unternehmen ist abhängig von prevention Fokus und promotion Fokus.

jmv::linReg(datensatz, dep = "JC_SCEN2", covs = c("PRO", "PRE"),
            blocks = list("PRO", "PRE"),
            norm = TRUE,
            r2Adj = TRUE, 
            anova = TRUE,
            stdEst = TRUE,
            qqPlot = TRUE)


#Die multiple lineare Regression liefert zwei Modelle mit einem Prädiktoren
# (siehe Tabelle, $F(1, 430) = 16.07$, $p<.05$). 
#Dabei wird nur der promotion Fokus signifikant (siehe Tabelle). 

#Tabelle 1: Prädiktor der multiplen linearen Regression.

#Predictor    Estimate    SE        t        p         Stand. Estimate  
----------    --------    ------    -----   --------   ----------------
  #Intercept      2.9450    0.2170    13.57    < .001                      
  #PRO            0.1260    0.0314     4.01    < .001             0.1951   
  #PRE            0.0405    0.0372     1.09     0.278             0.0529 
  
  
  # Visualisierung Hypothese 9
  
  datensatz %>% ggplot() + aes(x = PRO, y = JC_SCEN2) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", color = "black") +
                theme_minimal() + 
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Promotion Focus",
       y = "Job Crafting bei \nschlechter Kommunikation" [1-6],
       title = "Job Crafting bei qualitativ minderwertiger Kommunikation ist abhängig vom Promotion Focus.",
       subtitle = "Lineare Regression im Streudiagramm") +
  NULL
  


#Hypothese 10 - Job Crafting bei qualitativ hochwertiger Kommunikation von organisatorischen Veränderungen
#in einem Unternehmen ist abhängig von prevention Fokus und promotion Fokus.

jmv::linReg(datensatz, dep = "JC_SCEN1", covs = c("PRO", "PRE"),
            blocks = list("PRO", "PRE"),
            norm = TRUE,
            r2Adj = TRUE, 
            anova = TRUE,
            stdEst = TRUE,
            qqPlot = TRUE)

#Die multiple lineare Regression liefert zwei Modelle mit zwei Prädiktoren (siehe Tabelle, $F(2, 430) = 24.5$, $p<.01$). 
#Dabei werden sowohl prevention Fokus als auch promotion Fokus signifikant (siehe Tabelle). 

#Visualisierung Hypothese 10 
datensatz %>% ggplot() + aes(x = PRO, y = JC_SCEN1) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", color = "black") +
                theme_minimal() +
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Promotion Focus",
       y = "Job Crafting bei \nguter Kommunikation" [1-6],
       title = "Job Crafting bei qualitativ hochwertiger Kommunikation ist abhängig vom Promotion Focus.",
       subtitle = "Lineare Regression im Streudiagramm") +
  NULL


datensatz %>% ggplot() + aes(x = PRE, y = JC_SCEN1) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", color = "black") +
                theme_minimal() +
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Prevention Focus",
       y = "Job Crafting bei \nguter Kommunikation" [1-6],
       title = "Job Crafting bei qualitativ hochwertiger Kommunikation ist abhängig vom Prevention Focus.",
       subtitle = "Lineare Regression im Streudiagramm") +
  NULL

#Model Coefficients - JC_SCEN1                                             
#Predictor    Estimate    SE        t        p         Stand. Estimate   
# ---------    --------    ------    -----    ------    ---------------  
  #Intercept       2.749    0.2076    13.24    < .001                      
  #PRO             0.110    0.0301     3.66    < .001              0.176   
  #PRE             0.119    0.0356     3.34    < .001              0.160 

