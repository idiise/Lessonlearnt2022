
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


# codebook Burkina Faso ------------------------------------------------------------

BaseMenageBFA_2018<- to_factor(BaseMenageBFA_2018)
codebook_BFA2018 <- var_label(BaseMenageBFA_2018)
codebook_BFA2018 <- as.data.frame(do.call(rbind,codebook_BFA2018))
codebook_BFA2018 <- codebook_BFA2018 %>% rownames_to_column()

write_xlsx(codebook_BFA2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFA2018.xlsx")


# FCS Burkina -------------------------------------------------------------

# Rename variables using standardized variables namefor FCS
# supprimer les lignes 255, 631,633,650 pour la variables céréales 
# BaseMenageBFA_2018 <- BaseMenageBFA_2018[-c(255,631,633,650),]
# trabsformer la variable céréales en numerique
# BaseMenageBFA_2018$Cereale7_3_2Num <- as.numeric(BaseMenageBFA_2018$Cereale7_3_2Num)
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  FCSStap1 =  replace_na(as.numeric(Cereale7_3_2Num),0),
  FCSStap2 = replace_na(as.numeric(Tubercule7_3_2Num),0),
  FCSPulse = replace_na(as.numeric(Legumineuses7_3_2Num), 0),
  FCSVegOrg = replace_na(as.numeric(LegumeOrange7_3_2Num),0),
  FCSVegGre = replace_na(as.numeric(LegumFeuilleVert7_3_2Num), 0),
  FCSVegOth = replace_na(as.numeric(Autreslegum7_3_2Num), 0),
  FCSFruitOth = replace_na(as.numeric(Autrefruits7_3_2Num),0),
  FSCFruitOrg = replace_na(as.numeric(Orangefruit7_3_2Num),0),
  FCSPrMeatF = replace_na(as.numeric(Viande7_3_2Num), 0),
  FCSPrFish = replace_na(as.numeric(Poissons7_3_2Num), 0),
  FCSPrEgg = replace_na(as.numeric(Oeufs7_3_2Num),0),
  FCSDairy = replace_na(as.numeric(Lait7_3_2Num), 0),
  FCSFat = replace_na(as.numeric(Huilegrais7_3_2Num),0),
  FCSSugar = replace_na(as.numeric(SSucre7_3_2Num),0),
  FCSCond = replace_na(as.numeric(Condiments7_3_2Num), 0)
  
) %>% mutate(
  FCSSTap = FCSStap1 + FCSStap2,
  FCSVeg = FCSVegOrg + FCSVegGre + FCSVegOth,
  FCSPr = FCSPrMeatF + FCSPrFish + FCSPrEgg,
  FCSFruit = FSCFruitOrg + FCSFruitOth
) %>% mutate(
  FCSSTap = ifelse(FCSSTap > 7 , 7 , FCSSTap),
  FCSVeg = ifelse(FCSVeg > 7 , 7 , FCSVeg),
  FCSPr = ifelse(FCSPr > 7 , 7 , FCSPr),
  FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
  FCSPulse = ifelse(FCSPulse > 7 , 7 , FCSPulse),
  FCSDairy = ifelse(FCSDairy > 7 , 7 , FCSDairy),
  FCSSugar = ifelse(FCSSugar > 7 , 7 , FCSSugar),
  FCSFat = ifelse(FCSFat > 7 , 7 , FCSFat),
  FCSCond = ifelse(FCSCond > 7 , 7 , FCSCond)
)

#  FCS
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  FCS = (2*FCSSTap)+(3*FCSPulse) + FCSVeg + (4*FCSPr) + (4*FCSDairy) + FCSFruit +
    (0.5*FCSFat) + (0.5*FCSSugar)
) %>% mutate( FCSCat28 = case_when(
  FCS <= 28 ~ "Poor",
  between(FCS, 28.5, 42) ~ "Borderline",
  FCS > 42 ~ "Acceptable"
))

funModeling::freq(BaseMenageBFA_2018, "FCSCat28")
mean(BaseMenageBFA_2018$FCSSugar)


# HDDS Burkina ------------------------------------------------------------


BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  HDDSStapCer  = case_when(Cereale7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSStapRoot = case_when(Tubercule7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSPulse = case_when(Legumineuses7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSVeg = case_when(LegumeOrange7_3_1Num == "Oui" | LegumFeuilleVert7_3_1Num == "Oui"  | Autreslegum7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrMeat = case_when(Viande7_3_1Num == "Oui"  | Foie7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrEgg = case_when(Oeufs7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrFish = case_when(Poissons7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSDairy = case_when(Lait7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSSugar = case_when(Sucre7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSFat = case_when(Huilegrais7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSFruit = case_when(Orangefruit7_3_1Num == "Oui"  | Autrefruits7_3_1Num == "Oui" ~ 1, TRUE ~ 0),
  HDDSCond = case_when(Condiments7_3_1Num == "Oui" ~ 1, TRUE ~ 0)
 ) %>% mutate(
   HDDS = HDDSStapCer + HDDSStapRoot + HDDSPulse + HDDSVeg + HDDSDairy + HDDSPrMeat +
     HDDSPrEgg + HDDSPrFish + HDDSSugar + HDDSFat + HDDSFruit + HDDSCond
) %>% mutate( HDDS_CH = case_when(
  HDDS >= 5 ~ "Phase1",
  HDDS == 4 ~ "Phase2",
  HDDS == 3 ~ "Phase3",
  HDDS == 2 ~ "Phase4",
  HDDS < 2 ~ "Phase5"
)
)

plotly::ggplotly(funModeling::freq(BaseMenageBFA_2018, "HDDS_CH") )




# rCSI Burkina ------------------------------------------------------------
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  rCSILessQlty = replace_na(q00811Num,0),
  rCSIBorrow = replace_na(q00812Num, 0),
  rCSIMealSize = replace_na(q00813Num,0),
  rCSIMealAdult = replace(q00814Num,0),
  rCSIMealNb = replace_na(q00815Num,0)
) %>% 

class(BaseMenageBFA_2018$q00811Num)
# Mali --------------------------------------------------------------------

BaseMenageBMZ_MLI2018<- to_factor(BaseMenageBMZ_MLI2018)
codebook_MLI2018 <- var_label(BaseMenageBMZ_MLI2018)
codebook_MLI2018 <- as.data.frame(do.call(rbind,codebook_MLI2018))
codebook_MLI2018 <- codebook_MLI2018 %>% rownames_to_column()

write_xlsx(codebook_MLI2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MLI2018.xlsx")
# labeClled::
  
