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
  aes(x= age) +
  geom_histogram(binwidth = 1) +
  labs(x = "Alter in Jahren", 
       y = "Häufigkeit", 
       title = "Junge, leicht bimodal verteilte Stichprobe", 
       subtitle = "Histogramm zur Altersverteilung (n=399)", 
       caption = "binwidth = 1") +
  theme_minimal() +
  NULL

# Histogramm zur Geschlechtsverteilung:
datensatz %>%
  filter(gender != 3) %>%
  select(gender) %>%
  ggplot() +
  aes(x= gender) +
  geom_histogram(bins = 2) +
  labs(x = "Geschlecht", 
       y = "Häufigkeit", 
       title = "Überwiegend weibliche Stichprobe", 
       subtitle = "Histogramm zur Geschlechterverteilung (n=399)", 
       caption =(" ")) +
  theme_minimal() +

  NULL

<<<<<<< HEAD
# Hypothese 1
# Formulierung: Prevention focussed Menschen haben mehr Job Crafting bei schlechter Kommunikation als nicht-prevention focussed Menschen
library(questionr)

## Cutting datensatz$PRO into datensatz$PRO_mediansplit
datensatz$PRO_mediansplit <- cut(datensatz$PRO, include.lowest=TRUE,  right=TRUE,
                                 breaks=c(1.25, 4.75, 6))
## Cutting datensatz$PRE into datensatz$PRE_mediansplit
datensatz$PRE_mediansplit <- cut(datensatz$PRE, include.lowest=TRUE,  right=TRUE,
                                 breaks=c(1, 5.66666666666667, 6))

ANOVA(datensatz, dep = "JC_SCEN2", factors ="PRE_mediansplit", 
      postHoc = JC_SCEN2 ~ PRE_mediansplit)

# der mittlere Unterschied von -0.124 ist signifikant

#  Hypothese 2

ANOVA(datensatz, dep = "JC_SCEN1", factors ="PRO_mediansplit", 
      postHoc = JC_SCEN1 ~ PRO_mediansplit)

# ANOVA Tabelle sagt uns: verwirf die Hypothese H0 (signifikant)
# der mittlere Unterschied von -0.149 ist signifikant. Promotion focussed und nicht promotion-focussed unterscheiden sich im Job Crafting bei schlechter Kommunikation (Unterschied M = -0.149)
=======
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
  cowplot::theme_half_open() +
  theme(plot.title = element_text(size=12),
        axis.title = element_text(size=10)) +
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Job Crafting bei schlechter Kommunikation",
       y = "Prevention focus",
       title = "Es gibt einen schwachen Zusammenhang \nzwischen dem Job Crafting und dem \nPrevention focus.",
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
  cowplot::theme_half_open() +
  theme(plot.title = element_text(size=12),
        axis.title = element_text(size=10)) +
  scale_y_continuous(breaks = c(1:6), limits = c(1, 6)) +
  scale_x_continuous(breaks = c(1:6), limits = c(1, 6)) +
  labs(x = "Job Crafting bei guter Kommunikation",
       y = "Promotion focus",
       title = "Es gibt einen Zusammenhang zwischen dem \nJob Crafting und dem Promotion focus.",
       subtitle = "Pearson-Korrelation im Streudiagramm") +
  NULL

# Bericht Hypothese 7: Es gibt einen signifikanten Zusammenhang zwischen dem Promotion focus und dem Job Crafting bei
# qualitativ hochwertiger Kommunikation ($r = 0.216$, $p<.001$). Das bedeutet: Je höher der Promotion focus, desto 
# höher das Job Crafting bei qualitativ hochwertiger Kommunikation.
>>>>>>> 7476e612d1c2ea2c84baa2b4f53061a2e0ce3f7a
