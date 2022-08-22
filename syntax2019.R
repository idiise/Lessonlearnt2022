
# import package ----------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)
library(expss)

# import dataset ----------------------------------------------------------

setwd("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Baseline 2019/")

List_data <- dir_ls(regexp = ".sav") %>% 
  map(read_sav)
nom_base <- names(List_data)
nom_base <- str_replace(string = nom_base,pattern = ".sav", replacement = "")
names(List_data) <- nom_base
list2env(List_data, .GlobalEnv)
BaseMenageMLI_2019 <- read_sav("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/other data/BaseMenageMLI_2019.sav",encoding = "latin1")
BaseMenageMRT_2019 <- read_stata("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/other data/BaselineMRT_2019.dta")


# Codebook Burkina 2019 ----------------------------------------------------

BaseMenageBFA_2019<- to_factor(BaseMenageBFA_2019)
codebook_BFA2019 <- var_label(BaseMenageBFA_2019)
codebook_BFA2019 <- as.data.frame(do.call(rbind,codebook_BFA2019))
codebook_BFA2019 <- codebook_BFA2019 %>% rownames_to_column()

write_xlsx(codebook_BFA2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFA2019.xlsx")


# FCS Burkina 2019 --------------------------------------------------------
# on calcul le FCS avant de faire  to_factor dans la partie Codebook Burkina

BaseMenageBFA_2019 <- BaseMenageBFA_2019 %>% mutate(
  FCSStapCer = Q_10_4_1,
  FCSStapRoot = Q_10_4_2,
  FCSPulse = Q_10_4_3,
  FCSVegOrg = Q_10_4_4,
  FCSVegGre = Q_10_4_5,
  FCSVegOth = Q_10_4_6,
  FCSFruitOrg = Q_10_4_7,
  FCSFruitOth = Q_10_4_8,
  FCSPrMeatF = Q_10_4_9,
  FCSPrMeatO = Q_10_4_10,
  FCSPrFish = Q_10_4_11,
  FCSPrEgg = Q_10_3_12,
  FCSDairy = Q_10_4_13,
  FCSFat = Q_10_4_14,
  FCSSugar = Q_10_4_15,
  FCSCond = Q_10_4_16
) %>% mutate(
  FCSStap = as.numeric(FCSStapCer) + as.numeric(FCSStapRoot),
  FCSVeg = as.numeric(FCSVegOrg) + as.numeric(FCSVegGre) + as.numeric(FCSVegOth),
  FCSPr = as.numeric(FCSPrMeatF) + as.numeric(FCSPrFish) + as.numeric(FCSPrEgg) + as.numeric(FCSPrMeatO),
  FCSFruit = as.numeric(FCSFruitOrg) + as.numeric(FCSFruitOth),
  FCSDairy = as.numeric(FCSDairy) + 0,
  FCSPulse = as.numeric(FCSPulse) + 0,
  FCSFat = as.numeric(FCSFat) + 0,
  FCSSugar = as.numeric(FCSSugar) + 0,
  FCSCond = as.numeric(FCSCond) + 0
) %>% mutate(
  FCSStap = ifelse(FCSStap > 7 , 7 , FCSStap),
  FCSVeg = ifelse(FCSVeg > 7 , 7 , FCSVeg),
  FCSPr = ifelse(FCSPr > 7 , 7 , FCSPr),
  FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
  FCSPulse = ifelse(FCSPulse > 7 , 7 , FCSPulse),
  FCSDairy = ifelse(FCSDairy > 7 , 7 , FCSDairy),
  FCSSugar = ifelse(FCSSugar > 7 , 7 , FCSSugar),
  FCSFat = ifelse(FCSFat > 7 , 7 , FCSFat),
  FCSCond = ifelse(FCSCond > 7 , 7 , FCSCond),
) %>% mutate(
  FCS = (FCSStap * 2) + (FCSPulse * 3) + FCSVeg + (FCSPr * 4)+
    (FCSDairy * 4) + FCSFruit + (0.5 * FCSFat) + (0.5 * FCSSugar)
) %>% mutate(
  FCSCat28 = case_when(
    FCS <= 28 ~ "Poor",
    between(FCS, 28.5,42) ~ "Bordeline",
    FCS > 42 ~ "Acceptable"
  )
)

funModeling::freq(BaseMenageBFA_2019, "FCSCat28")


# HDDS Burkina 2019 -------------------------------------------------------

BaseMenageBFA_2019 <- BaseMenageBFA_2019 %>% mutate(
  HDDSStapCer  = case_when(Q_10_3_1 == "Oui" ~ 1, TRUE ~ 0),
  HDDSStapRoot = case_when(Q_10_3_2 == "Oui" ~ 1, TRUE ~ 0),
  HDDSPulse = case_when(Q_10_3_3 == "Oui" ~ 1, TRUE ~ 0),
  HDDSVeg = case_when(Q_10_3_4 == "Oui" | Q_10_3_5 == "Oui"  | Q_10_3_6 == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrMeat = case_when(Q_10_3_9 == "Oui"  | Q_10_3_10 == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrEgg = case_when(Q_10_3_12 == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrFish = case_when(Q_10_3_11 == "Oui" ~ 1, TRUE ~ 0),
  HDDSDairy = case_when(Q_10_3_13 == "Oui" ~ 1, TRUE ~ 0),
  HDDSSugar = case_when(Q_10_3_15 == "Oui" ~ 1, TRUE ~ 0),
  HDDSFat = case_when(Q_10_3_14 == "Oui" ~ 1, TRUE ~ 0),
  HDDSFruit = case_when(Q_10_3_7 == "Oui"  | Q_10_3_8 == "Oui" ~ 1, TRUE ~ 0),
  HDDSCond = case_when(Q_10_3_16 == "Oui" ~ 1, TRUE ~ 0)
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

funModeling::freq(BaseMenageBFA_2019, "HDDS_CH")


# rCSI Burkina 2019 -------------------------------------------------------

BaseMenageBFA_2019 <- BaseMenageBFA_2019 %>% mutate(
  rCSILessQlty = replace_na(Q_11_1_1,0),
  rCSIBorrow = replace_na(Q_11_1_2,0),
  rCSIMealSize = replace_na(Q_11_1_3,0),
  rCSIMealAdult  = replace_na(Q_11_1_4,0),
  rCSIMealNb = replace_na(Q_11_1_5,0)
) %>% mutate(
  rCSI = rCSILessQlty + (2 * rCSIBorrow) + (3 * rCSIMealAdult) + rCSIMealSize + rCSIMealNb
) %>% mutate(
  rCSI_CH = case_when(
    rCSI <= 3 ~ "Phase1",
    between(rCSI, 4,18) ~ "Phase2",
    rCSI >= 19 ~ "Phase3"
  )
)

funModeling::freq(BaseMenageBFA_2019, "rCSI_CH")


# HHS Burkina 2019 --------------------------------------------------------

BaseMenageBFA_2019 <- BaseMenageBFA_2019 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)


# LCS Burkina 2019 --------------------------------------------------------

BaseMenageBFA_2019 <- BaseMenageBFA_2019 %>% mutate(
  LhCSIStress1 = Q_11_2_1_1,
  LhCSIStress2 = Q_11_2_5_1,
  LhCSIStress3 = Q_11_2_4_1,
  LhCSIStress4 = Q_11_2_10_1,
  LhCSICrisis1 = Q_11_2_2_1,
  LhCSICrisis2 = Q_11_2_3_1,
  LhCSICrisis3 = Q_11_2_7_1,
  LhCSIEmergency1 = Q_11_2_6_1, 
  LhCSIEmergency2 = Q_11_2_9_1,
  LhCSIEmergency3 = Q_11_2_8_1
) %>% mutate(
  stress_coping = case_when(
    LhCSIStress1 == "Oui" | str_detect(LhCSIStress1,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSIStress2 == "Oui" | str_detect(LhCSIStress2,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSIStress3 == "Oui" | str_detect(LhCSIStress3,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSIStress4 == "Oui" | str_detect(LhCSIStress4,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    TRUE ~ "Non"
  )
) %>% 
  mutate(crisis_coping = case_when(
    LhCSICrisis1 == "Oui" | str_detect(LhCSICrisis1,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSICrisis2 == "Oui" | str_detect(LhCSICrisis2,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSICrisis3 == "Oui" | str_detect(LhCSICrisis3,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    TRUE ~ "Non")) %>% 
  mutate(emergency_coping = case_when(
    LhCSIEmergency1 == "Oui" | str_detect(LhCSIEmergency1,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSIEmergency2 == "Oui" | str_detect(LhCSIEmergency2,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    LhCSIEmergency3 == "Oui" | str_detect(LhCSIEmergency3,"Non, j’ai déjà vendu ces avoirs ou mené cette activité et je ne peux pas continuer à le faire") ~ "Oui",
    TRUE ~ "Non")) %>% mutate(LhCSICat = case_when(
      emergency_coping == "Oui" ~ "StrategiesdeUrgence",
      crisis_coping == "Oui" ~ "StrategiesdeCrise",
      stress_coping == "Oui" ~ "StrategiesdeStress",
      TRUE ~ "Pasdestrategies")) %>% 
  mutate(LhCSICat = fct_relevel(LhCSICat, c("Pasdestrategies", "StrategiesdeStress", "StrategiesdeCrise", "StrategiesdeUrgence")))

funModeling::freq(BaseMenageBFA_2019, "LhCSICat")

# Codebook Mali 2019 ------------------------------------------------------

BaseMenageMLI_2019<- to_factor(BaseMenageMLI_2019)
codebook_MLI2019 <- var_label(BaseMenageMLI_2019)
codebook_MLI2019 <- as.data.frame(do.call(rbind,codebook_MLI2019))
codebook_MLI2019 <- codebook_MLI2019 %>% rownames_to_column()

write_xlsx(codebook_MLI2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MLI2019.xlsx")

