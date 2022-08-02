
# import Package ----------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# import datasets ---------------------------------------------------------

setwd("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Baseline 2018/")

List_data <- dir_ls(regexp = ".sav") %>% 
  map(read_sav)
  nom_base <- names(List_data)
  nom_base <- str_replace(string = nom_base,pattern = ".sav", replacement = "")
  names(List_data) <- nom_base
  list2env(List_data, .GlobalEnv)
BaseMenageNER_2018 <- read_sav("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/BaseMenageNER_2018.sav",
                               encoding="latin1")


# Burkina Faso ------------------------------------------------------------

BaseMenageBFA_2018<- to_factor(BaseMenageBFA_2018)
codebook_BFA2018 <- var_label(BaseMenageBFA_2018)
codebook_BFA2018 <- as.data.frame(do.call(rbind,codebook_BFA2018))
codebook_BFA2018 <- codebook_BFA2018 %>% rownames_to_column()

write_xlsx(codebook_BFA2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFA2018.xlsx")


# Mali --------------------------------------------------------------------

BaseMenageBMZ_MLI2018<- to_factor(BaseMenageBMZ_MLI2018)
codebook_MLI2018 <- var_label(BaseMenageBMZ_MLI2018)
codebook_MLI2018 <- as.data.frame(do.call(rbind,codebook_MLI2018))
codebook_MLI2018 <- codebook_MLI2018 %>% rownames_to_column()

write_xlsx(codebook_MLI2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MLI2018.xlsx")
