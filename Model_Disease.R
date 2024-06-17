library(sjstats) #ponderation
library(effectsize)
library(tidyverse) 

rm(list=ls())
setwd("C:/_MY FOLDERS/RECHERCHE/HIVE")
HIVE <- read.csv("HIVE_1KM.csv")

HIVE <- HIVE %>%
  mutate(DiseaseStatus = factor(ifelse(Cause.of.death == "Beekeeping Pest/Disease (AFB, EFB, Varroa, Disentery, SHB, Other)", 1, 0),
                                levels = c(0, 1),
                                labels = c("NORMAL", "DISEASE")))


ggplot(data = HIVE) +
  geom_violin(aes(x=DiseaseStatus, y=MaxHive, fill=DiseaseStatus),width=0.5)+
  theme(legend.position="none")+
  labs(title="Boxplot",
       x ="DiseaseStatus",
       y ="MaxHive")


t.test(MaxHive ~ DiseaseStatus, data = HIVE)

