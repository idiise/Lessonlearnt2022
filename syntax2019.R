
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

# write_xlsx(codebook_BFA2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFA2019.xlsx")


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

# write_xlsx(codebook_MLI2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MLI2019.xlsx")


# FCS MaAli 2019 --------------------------------------------------------

BaseMenageMLI_2019 <- BaseMenageMLI_2019 %>% mutate(
  FCSStapCer = Q101_1b_Cereales_Nbre_jours_conso_7_Jours,
  FCSStapRoot = Q101_2b_Racines_Tubercules_Nbre_jours_conso_7_Jours,
  FCSPulse = Q101_3b_Legumineuse_Noix_Nbre_jours_conso_7_Jours,
  FCSVeg = Q101_6b_LegumesFeuillesVertes_Nbre_jours_conso_7_Jours,
  FCSVegOrg = Q101_61b_Legumes_Vit_A_Nbre_jours_conso_7_Jours,
  FCSVegGre = Q101_62b_Legumes_FerNonHem_Nbre_jours_conso_7_Jours,
  FCSVegOth = NA,
  FCSFruit = Q101_7b_Fruits_Nbre_jours_conso_7_Jours,
  FCSFruitOrg = Q101_71b_Fruits_Vit_A_Nbre_jours_conso_7_Jours,
  FCSFruitOth = NA,
  FCSPr =  Q101_5b_ViandePoissonOEuf_Nbre_jours_conso_7_Jours,
  FCSPrMeatF = Q101_51b_Viandes_Nbre_jours_conso_7_Jours,
  FCSPrMeatO = Q101_52b_Abats_rouges_Nbre_jours_conso_7_Jours,
  FCSPrFish = Q101_53b_Poissons_Nbre_jours_conso_7_Jours,
  FCSPrEgg = Q101_54b_OEufs_Nbre_jours_conso_7_Jours,
  FCSDairy = Q101_4b_Lait_Nbre_jours_conso_7_Jours,
  FCSFat = Q101_8b_Huile_Gras_Beurre_Nbre_jours_conso_7_Jours,
  FCSSugar = Q101_9b_Sucres_Nbre_jours_conso_7_Jours,
  FCSCond = Q101_10b_Epices_Condiments_Nbre_jours_conso_7_Jours
) %>% mutate(
  FCSStap = FCSStapCer + FCSStapRoot,
  
) %>% mutate(
  FCSStap = ifelse(FCSStap > 7 , 7 , FCSStap)
) %>% mutate(
  FCS = SCA,
  FCSCat28 = FCG_2842
)

funModeling::freq(BaseMenageMLI_2019, "FCSCat28")

# HDDS Mali 2019 ----------------------------------------------------------

BaseMenageMLI_2019 <- BaseMenageMLI_2019 %>% mutate(
  HDDSStapCer  = SDAM_CEREALES,
  HDDSStapRoot = SDAM_TUBERCULES,
  HDDSPulse = SDAM_LEGUMINEUSES,
  HDDSVeg = SDAM_LEGUMES,
  HDDSPrMeat = SDAM_VIANDES,
  HDDSPrEgg = SDAM_OEUFS,
  HDDSPrFish = SDAM_POISSONS,
  HDDSDairy = SDAM_LAIT,
  HDDSSugar = SDAM_SUCRE,
  HDDSFat = SDAM_HUILE,
  HDDSFruit = SDAM_FRUITS,
  HDDSCond = SDAM_CONDIMENTS
) %>% mutate(
  HDDS = SDAM
) %>% mutate( HDDS_CH = case_when(
  HDDS >= 5 ~ "Phase1",
  HDDS == 4 ~ "Phase2",
  HDDS == 3 ~ "Phase3",
  HDDS == 2 ~ "Phase4",
  HDDS < 2 ~ "Phase5"
)
)

funModeling::freq(BaseMenageBFA_2019, "HDDS_CH")

# rCSI Mali 2019 ----------------------------------------------------------

BaseMenageMLI_2019 <- BaseMenageMLI_2019 %>% mutate(
  rCSILessQlty = replace_na(Q11_1_rCSI_Substitution,0),
  rCSIBorrow = replace_na(Q11_2_rCSI_Emprunt_Aide,0),
  rCSIMealSize = replace_na(Q11_5_rCSI_Limitation_Portion_Repas,0),
  rCSIMealAdult  = replace_na(Q11_4_rCSI_Diminution_Conso_Adultes,0),
  rCSIMealNb = replace_na(Q11_3_rCSI_Reduction_Nbre_Repas,0)
) %>% mutate(
  rCSI_CH = case_when(
    rCSI <= 3 ~ "Phase1",
    between(rCSI, 4,18) ~ "Phase2",
    rCSI >= 19 ~ "Phase3"
  ) 
)
  
funModeling::freq(BaseMenageMLI_2019, "rCSI_CH")


# HHS Mali 2019 -----------------------------------------------------------
# Il n'ya pas de HHS dans la Base du Mali
BaseMenageMLI_2019 <- BaseMenageMLI_2019 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)

# LCS Mali 2019 -----------------------------------------------------------

BaseMenageMLI_2019 <- BaseMenageMLI_2019 %>% mutate(
  LhCSIStress1 = Q11_8_LCS_Vente_Biens_NonProd,
  LhCSIStress2 = Q11_17_LCS_Destockage,
  LhCSIStress3 = Q11_11_LCS_Depense_Epargne,
  LhCSIStress4 = Q11_13_LCS_Emprunt_Argent,
  LhCSICrisis1 = Q11_10_LCS_Reduction_Dep_NAE,
  LhCSICrisis2 = Q11_9_LCS_Vente_Actifs_Prod,
  LhCSICrisis3 = Q11_18_LCS_Retirer_Enfants_Ecole,
  LhCSIEmergency1 = Q11_21_LCS_Mendier, 
  LhCSIEmergency2 = Q11_14_LCS_Vente_Immobilier,
  LhCSIEmergency3 = Q11_16_LCS_Vente_Femelle_Repro
) %>% mutate(
  stress_coping = case_when(
    LhCSIStress1 == "Oui" | str_detect(LhCSIStress1,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSIStress2 == "Oui" | str_detect(LhCSIStress2,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSIStress3 == "Oui" | str_detect(LhCSIStress3,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSIStress4 == "Oui" | str_detect(LhCSIStress4,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    TRUE ~ "Non"
  )
) %>% 
  mutate(crisis_coping = case_when(
    LhCSICrisis1 == "Oui" | str_detect(LhCSICrisis1,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSICrisis2 == "Oui" | str_detect(LhCSICrisis2,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSICrisis3 == "Oui" | str_detect(LhCSICrisis3,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    TRUE ~ "Non")) %>% 
  mutate(emergency_coping = case_when(
    LhCSIEmergency1 == "Oui" | str_detect(LhCSIEmergency1,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSIEmergency2 == "Oui" | str_detect(LhCSIEmergency2,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    LhCSIEmergency3 == "Oui" | str_detect(LhCSIEmergency3,"Non, parce que jâ\u0080\u0099ai dÃ©jÃ  vendu ces avoirs ou menÃ© cette activitÃ©") ~ "Oui",
    TRUE ~ "Non")) %>% mutate(LhCSICat = case_when(
      emergency_coping == "Oui" ~ "StrategiesdeUrgence",
      crisis_coping == "Oui" ~ "StrategiesdeCrise",
      stress_coping == "Oui" ~ "StrategiesdeStress",
      TRUE ~ "Pasdestrategies")) %>% 
  mutate(LhCSICat = fct_relevel(LhCSICat, c("Pasdestrategies", "StrategiesdeStress", "StrategiesdeCrise", "StrategiesdeUrgence")))

funModeling::freq(BaseMenageMLI_2019, "LhCSICat")


# Codebook Mauritania 2019 ------------------------------------------------

BaseMenageMRT_2019<- to_factor(BaseMenageMRT_2019)
codebook_MRT2019 <- var_label(BaseMenageMRT_2019)
codebook_MRT2019 <- as.data.frame(do.call(rbind,codebook_MRT2019))
codebook_MRT2019 <- codebook_MRT2019 %>% rownames_to_column()

# write_xlsx(codebook_MRT2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MRT2019.xlsx")

# FCS Mauritania 2019 -----------------------------------------------------
# pas de condiment

BaseMenageMRT_2019 <- BaseMenageMRT_2019 %>% mutate(
  FCSStap = FCS1,
  FCSPulse = FCS2,
  FCSDairy = FCS3,
  FCSPr = FCS4,
  FCSVeg = FCS5,
  FCSFruit = FCS6,
  FCSFat = FCS7,
  FCSSugar = FCS8,
  FCSCond = NA,
  FCSCat28 = FCG
)

funModeling::freq(BaseMenageMRT_2019, "FCSCat28")

# HDDS Mauritania 2019 ----------------------------------------------------

BaseMenageMRT_2019 <- BaseMenageMRT_2019 %>% mutate(
  HDDSStapCer = as.numeric(DDS1),
  HDDSStapRoot = NA,
  HDDSPulse = as.numeric(DDS2),
  HDDSDairy = as.numeric(DDS3),
  HDDSPrMeat = as.numeric(DDS4),
  HDDSPrFish = NA,
  HDDSPrEgg = NA,
  HDDSVeg = as.numeric(DDS5),
  HDDSFruit = as.numeric(DDS6),
  HDDSFat = as.numeric(DDS7),
  HDDSSugar = as.numeric(DDS8),
  HDDSCond = NA,
  HDDS = dds
  # ) %>% mutate(
  #   HDDS = HDDSStapCer  + HDDSPulse + HDDSVeg + HDDSDairy + HDDSPrMeat +
  #       HDDSFruit + HDDSFat  + HDDSSugar
) %>% mutate(
  HDDS_CH = case_when(HDDS >= 5 ~ "Phase1",
                      HDDS == 4 ~ "Phase2",
                      HDDS == 3 ~ "Phase3",
                      HDDS == 2 ~ "Phase4",
                      HDDS < 2 ~ "Phase5")
)

funModeling::freq(BaseMenageMRT_2019, "HDDS_CH")



# rCSI Mauritania 2019 --------------------------------------------------------------------

BaseMenageMRT_2019 <- BaseMenageMRT_2019 %>% mutate(
  rCSILessQlty = SS_1_1a,
  rCSIBorrow = SS_1_1b,
  rCSIMealSize = SS_1_1c,
  rCSIMealAdult  = SS_1_1d,
  rCSIMealNb = SS_1_1e
  # rCSI2 = rCSI
) %>% 
# %>%   mutate(
# rCSI = rCSILessQlty + (2 * rCSIBorrow) + (3 * rCSIMealAdult) + rCSIMealSize + rCSIMealNb
# ) %>% 
 mutate(
  rCSI_CH = case_when(
    rCSI <= 3 ~ "Phase1",
    between(rCSI, 4,18) ~ "Phase2",
    rCSI >= 19 ~ "Phase3"
  )
)

funModeling::freq(BaseMenageMRT_2019, "rCSI_CH")


# HHS Mauritania 2019 -----------------------------------------------------

BaseMenageMRT_2019 <- BaseMenageMRT_2019 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)

# LCS Mauritania 2019 -----------------------------------------------------

BaseMenageMRT_2019 <- BaseMenageMRT_2019 %>% mutate(
  LhCSIStress1 = SS_stress1,
  LhCSIStress2 = SS_stress2,
  LhCSIStress3 = SS_stress3,
  LhCSIStress4 = SS_stress4,
  LhCSICrisis1 = SS_crise1,
  LhCSICrisis2 = SS_crise2,
  LhCSICrisis3 = SS_crise3,
  LhCSIEmergency1 = SS_urgence1,
  LhCSIEmergency2 = SS_urgence2,
  LhCSIEmergency3 = SS_urgence3
) %>% mutate(
  stress_coping = case_when(
    LhCSIStress1 == "Oui" | str_detect(LhCSIStress1,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSIStress2 == "Oui" | str_detect(LhCSIStress2,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSIStress3 == "Oui" | str_detect(LhCSIStress3,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSIStress4 == "Oui" | str_detect(LhCSIStress4,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    TRUE ~ "Non")) %>% 
  mutate(crisis_coping = case_when(
    LhCSICrisis1 == "Oui" | str_detect(LhCSICrisis1,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSICrisis2 == "Oui" | str_detect(LhCSICrisis2,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSICrisis3 == "Oui" | str_detect(LhCSICrisis3,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    TRUE ~ "Non")) %>% 
  mutate(emergency_coping = case_when(
    LhCSIEmergency1 == "Oui" | str_detect(LhCSIEmergency1,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSIEmergency2 == "Oui" | str_detect(LhCSIEmergency2,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    LhCSIEmergency3 == "Oui" | str_detect(LhCSIEmergency3,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours de") ~ "Oui",
    TRUE ~ "Non")) %>% mutate(
      LhCSICat = case_when(
        emergency_coping == "Oui" ~ "StrategiesdeUrgence",
        crisis_coping == "Oui" ~ "StrategiesdeCrise",
        stress_coping == "Oui" ~ "StrategiesdeStress",
        TRUE ~ "PasdeStrategies"
      )
    ) %>% mutate(
      LhCSICat = fct_relevel(LhCSICat,c("Pasdestrategies", "StrategiesdeStress", "StrategiesdeCrise", "StrategiesdeUrgence"))
    )
funModeling::freq(BaseMenageMRT_2019, "LhCSICat")


# Codebook Niger 2019 -----------------------------------------------------

BaseMenageNER_2019<- to_factor(BaseMenageNER_2019)
codebook_NER2019 <- var_label(BaseMenageNER_2019)
codebook_NER2019 <- as.data.frame(do.call(rbind,codebook_NER2019))
codebook_NER2019 <- codebook_NER2019 %>% rownames_to_column()

# write_xlsx(codebook_NER2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_NER2019.xlsx")

# FCS Niger 2019 ----------------------------------------------------------

BaseMenageNER_2019 <- BaseMenageNER_2019 %>% mutate(
  FCSStapCer = replace_na(CERB,0),
  FCSStapRoot = replace_na(RACTUBB,0),
  FCSPulse = replace_na(LEGUMINOIXB,0),
  FCSVegOrg = replace_na(LEGUMORANB,0),
  FCSVegGre = replace_na(LEGUMVERTB,0),
  FCSVegOth = replace_na(AUTRLEGUMB,0),
  FCSFruitOrg = replace_na(FRUITORANB,0),
  FCSFruitOth = replace_na(AUTRFRUITB,0),
  FCSPrMeatF = replace_na(VIANDEB,0),
  FCSPrMeatO = replace_na(FOIROGNONB,0),
  FCSPrFish = replace_na(POISSONB,0),
  FCSPrEgg = replace_na(OEUFB,0),
  FCSDairy = replace_na(LAITB,0),
  FCSFat = replace_na(HUILEB,0),
  FCSSugar = replace_na(SUCREB,0),
  FCSCond = replace_na(EPICEB,0)
  # FCS2 = FCS
) %>% mutate(
  FCSStap = FCSStapCer + FCSStapRoot,
  FCSVeg = FCSVegOrg + FCSVegGre + FCSVegOth,
  FCSPr = FCSPrMeatF + FCSPrFish + FCSPrEgg + FCSPrMeatO,
  FCSFruit = FCSFruitOrg + FCSFruitOth
) %>% mutate(
  FCSStap = ifelse(FCSStap > 7 , 7 , FCSStap),
  FCSVeg = ifelse(FCSVeg > 7 , 7 , FCSVeg),
  FCSPr = ifelse(FCSPr > 7 , 7 , FCSPr),
  FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
  FCSPulse = ifelse(FCSPulse > 7 , 7 , FCSPulse),
  FCSDairy = ifelse(FCSDairy > 7 , 7 , FCSDairy),
  FCSSugar = ifelse(FCSSugar > 7 , 7 , FCSSugar),
  FCSFat = ifelse(FCSFat > 7 , 7 , FCSFat),
  FCSCond = ifelse(FCSCond > 7 , 7 , FCSCond)
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

funModeling::freq(BaseMenageNER_2019, "FCSCat28")

# HDDS Niger 2019 ---------------------------------------------------------
 # Pas de HDDS en 2019 pour le Niger

BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
  HDDSStapCer = NA,
  HDDSStapRoot = NA,
  HDDSPulse = NA,
  HDDSDairy = NA,
  HDDSPrMeat = NA,
  HDDSPrFish = NA,
  HDDSPrEgg = NA,
  HDDSVeg = NA,
  HDDSFruit = NA,
  HDDSFat = NA,
  HDDSSugar = NA,
  HDDSCond = NA,
  HDDS = NA,
  HDDS_CH = NA
)


# rCSI Niger 2019 ---------------------------------------------------------

BaseMenageNER_2019 <- BaseMenageNER_2019 %>% mutate(
  rCSILessQlty = replace_na(SS2,0),
  rCSIBorrow = replace_na(SS3,0),
  rCSIMealSize = replace_na(SS4,0),
  rCSIMealAdult = replace_na(SS5,0),
  rCSIMealNb = replace_na(SS6,0)
) %>% mutate(
  rCSI = rCSILessQlty + (2 * rCSIBorrow) + (3 * rCSIMealAdult) +
    rCSIMealNb + rCSIMealSize
) %>% mutate(
  rCSI_CH = case_when(
    rCSI <= 3 ~ "Phase1",
    between(rCSI, 4,18) ~ "Phase2",
    rCSI >= 19 ~ "Phase3"
  )
)

funModeling::freq(BaseMenageNER_2019, "rCSI_CH")


# HHS Niger 2019 ----------------------------------------------------------
# pas de HHS au niger en 2019
BaseMenageNER_2019 <- BaseMenageNER_2019 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)

# LCS Niger 2019 ----------------------------------------------------------

BaseMenageNER_2019 <- BaseMenageNER_2019 %>%  mutate(
  LhCSIStress1 = ss9x1,
  LhCSIStress2 = ss13x1,
  LhCSIStress3 = ss15x1,
  LhCSIStress4 = ss17x1,
  LhCSICrisis1 = ss10x1,
  LhCSICrisis2 = ss11x1,
  LhCSICrisis3 = ss12x1,
  LhCSIEmergency1 = ss14x1,
  LhCSIEmergency2 = ss16x1,
  LhCSIEmergency3 = ss18x1
) %>% mutate(
  stress_coping = case_when(
    LhCSIStress1 == "Oui" | str_detect(LhCSIStress1,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSIStress2 == "Oui" | str_detect(LhCSIStress2,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSIStress3 == "Oui" | str_detect(LhCSIStress3,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSIStress4 == "Oui" | str_detect(LhCSIStress4,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    TRUE ~ "Non")) %>% 
  mutate(crisis_coping = case_when(
    LhCSICrisis1 == "Oui" | str_detect(LhCSICrisis1,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSICrisis2 == "Oui" | str_detect(LhCSICrisis2,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSICrisis3 == "Oui" | str_detect(LhCSICrisis3,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    TRUE ~ "Non")) %>% 
  mutate(emergency_coping = case_when(
    LhCSIEmergency1 == "Oui" | str_detect(LhCSIEmergency1,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSIEmergency2 == "Oui" | str_detect(LhCSIEmergency2,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    LhCSIEmergency3 == "Oui" | str_detect(LhCSIEmergency3,"Non, parce que j'ai déjà vendu ces avoirs ou mené cette activité au cours des 12 derniers mois et je ne peux pas con") ~ "Oui",
    TRUE ~ "Non")) %>% mutate(
      LhCSICat = case_when(
        emergency_coping == "Oui" ~ "StrategiesdeUrgence",
        crisis_coping == "Oui" ~ "StrategiesdeCrise",
        stress_coping == "Oui" ~ "StrategiesdeStress",
        TRUE ~ "PasdeStrategies"
      )
    )

funModeling::freq(BaseMenageNER_2019, "LhCSICat")
