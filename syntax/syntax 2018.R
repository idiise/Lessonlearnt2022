
# import Package ----------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)
library(expss)

# import datasets ---------------------------------------------------------

setwd("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Baseline 2018/")

List_data <- dir_ls(regexp = ".sav") %>% 
  map(read_sav)
  nom_base <- names(List_data)
  nom_base <- str_replace(string = nom_base,pattern = ".sav", replacement = "")
  names(List_data) <- nom_base
  list2env(List_data, .GlobalEnv)
BaseMenageNER_2018 <- read_sav("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/other data/BaseMenageNER_2018.sav",
                               encoding="latin1")


# codebook Burkina Faso ------------------------------------------------------------
# Pour le Burkina pas de HHS ni de LCS

BaseMenageBFA_2018<- BaseMenageBFA_2018 %>%  to_factor()
# codebook_BFA2018 <- var_label(BaseMenageBFA_2018)
# codebook_BFA2018 <- as.data.frame(do.call(rbind,codebook_BFA2018))
# codebook_BFA2018 <- codebook_BFA2018 %>% rownames_to_column()

# write_xlsx(codebook_BFA2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFA2018.xlsx")
# BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% rename(
#   ADMIN1Name = q001_RegionNum
# )
oldBFA <- c("q001_RegionNum", "q002_ProvinceNum", "q0016_sexCMNum", "q0017_statutNum", "q0018_AgeCMnum", "q0019Num")
newBFA <- c("ADMIN1Name", "ADMIN2Name", "HHHSex", "RelationHHH", "HHHAge", "HHHEduc")

data.table::setnames(BaseMenageBFA_2018, old = oldBFA, new = newBFA)
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  HHSize = NA
)
# FCS Burkina 2018 -------------------------------------------------------------

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
  FCSStap = FCSStap1 + FCSStap2,
  FCSVeg = FCSVegOrg + FCSVegGre + FCSVegOth,
  FCSPr = FCSPrMeatF + FCSPrFish + FCSPrEgg,
  FCSFruit = FSCFruitOrg + FCSFruitOth
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
)

#  FCS
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  FCS = (2*FCSStap)+(3*FCSPulse) + FCSVeg + (4*FCSPr) + (4*FCSDairy) + FCSFruit +
    (0.5*FCSFat) + (0.5*FCSSugar)
) %>% mutate( FCSCat28 = case_when(
  FCS <= 28 ~ "Poor",
  between(FCS, 28.5, 42) ~ "Borderline",
  FCS > 42 ~ "Acceptable"
))

funModeling::freq(BaseMenageBFA_2018, "FCSCat28")
mean(BaseMenageBFA_2018$FCSSugar)


# HDDS Burkina 2018 ------------------------------------------------------------


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

funModeling::freq(BaseMenageBFA_2018, "HDDS_CH")


# rCSI Burkina 2018 ------------------------------------------------------------
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  rCSILessQlty = case_when(q00811Num == "1 jour dans la semaine" ~ 1,
                           q00811Num == "2 jours dans la semaine" ~ 2,
                           q00811Num == "3 jours dans la semaine" ~ 3,
                           q00811Num == "4 jours dans la semaine" ~ 4,
                           q00811Num == "5 jours dans la semaine" ~ 5,
                           q00811Num == "6 jours dans la semaine" ~ 6,
                           q00811Num == "7 jours dans la semaine" ~ 7,
                           TRUE ~ 0
                           ),
  rCSIBorrow = case_when(q00812Num == "1 jour dans la semaine" ~ 1,
                         q00812Num == "2 jours dans la semaine" ~ 2,
                         q00812Num == "3 jours dans la semaine" ~ 3,
                         q00812Num == "4 jours dans la semaine" ~ 4,
                         q00812Num == "5 jours dans la semaine" ~ 5,
                         q00812Num == "6 jours dans la semaine" ~ 6,
                         q00812Num == "7 jours dans la semaine" ~ 7,
                         TRUE ~ 0
                          ),
  rCSIMealSize = case_when(q00813Num == "1 jour dans la semaine" ~ 1,
                           q00813Num == "2 jours dans la semaine" ~ 2,
                           q00813Num == "3 jours dans la semaine" ~ 3,
                           q00813Num == "4 jours dans la semaine" ~ 4,
                           q00813Num == "5 jours dans la semaine" ~ 5,
                           q00813Num == "6 jours dans la semaine" ~ 6,
                           q00813Num == "7 jours dans la semaine" ~ 7,
                           TRUE ~ 0
                          ),
  rCSIMealAdult = case_when(q00814Num == "1 jour dans la semaine" ~ 1,
                            q00814Num == "2 jours dans la semaine" ~ 2,
                            q00814Num == "3 jours dans la semaine" ~ 3,
                            q00814Num == "4 jours dans la semaine" ~ 4,
                            q00814Num == "5 jours dans la semaine" ~ 5,
                            q00814Num == "6 jours dans la semaine" ~ 6,
                            q00814Num == "7 jours dans la semaine" ~ 7,
                            TRUE ~ 0
                            ),
  rCSIMealNb = case_when(q00815Num == "1 jour dans la semaine" ~ 1,
                         q00815Num == "2 jours dans la semaine" ~ 2,
                         q00815Num == "3 jours dans la semaine" ~ 3,
                         q00815Num == "4 jours dans la semaine" ~ 4,
                         q00815Num == "5 jours dans la semaine" ~ 5,
                         q00815Num == "6 jours dans la semaine" ~ 6,
                         q00815Num == "7 jours dans la semaine" ~ 7,
                         TRUE ~ 0
                          )) %>% mutate(rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealSize + (3 * rCSIMealAdult) + rCSIMealNb) %>% 
  mutate(rCSI_CH = case_when(rCSI <= 3 ~ "Phase1",
                             between(rCSI,4,18) ~ "Phase2",
                             rCSI >= 19 ~ "Phase3"
                             )
         )

funModeling::freq(BaseMenageBFA_2018, "rCSI_CH")



# HHS burkina 2018 ---------------------------------------------------------
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)

# lCS Burkina -------------------------------------------------------------

BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  LhCSIStress1 = NA,
  LhCSIStress2 = NA,
  LhCSIStress3 = NA,
  LhCSIStress4 = NA,
  LhCSICrisis1 = NA,
  LhCSICrisis2 = NA,
  LhCSICrisis3 = NA,
  LhCSIEmergency1 = NA,
  LhCSIEmergency2 = NA,
  LhCSIEmergency3 = NA,
  crisis_coping = NA,
  emergency_coping = NA,
  stress_coping = NA,
  LhCSICat = NA
)

# Codebook Mali 2018 --------------------------------------------------------------------

BaseMenageBMZ_MLI2018<- to_factor(BaseMenageBMZ_MLI2018)
# codebook_MLI2018 <- var_label(BaseMenageBMZ_MLI2018)
# codebook_MLI2018 <- as.data.frame(do.call(rbind,codebook_MLI2018))
# codebook_MLI2018 <- codebook_MLI2018 %>% rownames_to_column()

# write_xlsx(codebook_MLI2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MLI2018.xlsx")
oldMLI <- c("consentidentificationregion", "consentidentificationcercle", "Taille_MenageT_M", "COM11_sexe_chef_menage", "COM12_statut_matrimonial_chef_menage", "COM14_niveau_instruction_chef_menage")
newMLI <- c("ADMIN1Name", "ADMIN2Name", "HHSize", "HHHSex", "RelationHHH", "HHHEduc")
data.table::setnames(BaseMenageBMZ_MLI2018, old = oldMLI,
                     new = newMLI)
BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  HHHAge = NA
)

# FCS Mali 2018 -----------------------------------------------------------

BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  FCSStapCer = replace_na(as.numeric(Score_conso_alimcerealeSCA_1_consom_dernier7_jours_cereal), 0),
  FCSStapRoot = replace_na(as.numeric(Score_conso_alimracines_tuberculeSCA_2_consom_dernier7_jo), 0),
  FCSPulse = replace_na(as.numeric(Score_conso_alimlegumineuses_noixSCA_3_consom_dernier7_jo), 0),
  FCSDairy = replace_na(as.numeric(Score_conso_alimlait_autres_laitSCA_4_consom_dernier7_jou), 0),
  FCSPr = replace_na(as.numeric(Score_conso_alimviande_poisson_oeufSCA_5_consom_dernier7), 0),
  FCSVeg = replace_na(as.numeric(Score_conso_alimlegumes_feuillesSCA_6_consom_dernier7_jou), 0),
  FCSFruit = replace_na(as.numeric(Score_conso_alimSCA_7_fruitsSCA_7_consom_dernier7_jours_f), 0),
  FCSFat = replace_na(as.numeric(Score_conso_alimSCA_8_huile_gras_beurreSCA_8_consom_derni), 0),
  FCSSugar = replace_na(as.numeric(Score_conso_alimSCA_9_sucre_produit_sucreSCA_9_consom_der), 0),
  FCSCond = replace_na(as.numeric(Score_conso_alimSCA_10_epice_condimentSCA_10_consom_derni), 0)
) %>% mutate(
  FCSStap = FCSStapCer + FCSStapRoot
) %>% mutate(
  FCSStap = ifelse(FCSStap > 7, 7, FCSStap),
  FCSVeg = ifelse(FCSVeg > 7 , 7 , FCSVeg),
  FCSPr = ifelse(FCSPr > 7 , 7 , FCSPr),
  FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
  FCSPulse = ifelse(FCSPulse > 7 , 7 , FCSPulse),
  FCSDairy = ifelse(FCSDairy > 7 , 7 , FCSDairy),
  FCSSugar = ifelse(FCSSugar > 7 , 7 , FCSSugar),
  FCSFat = ifelse(FCSFat > 7 , 7 , FCSFat),
  FCSCond = ifelse(FCSCond > 7 , 7 , FCSCond)
) %>% mutate(
  FCS = (2 * FCSStap) + (3 *  FCSPulse) + FCSVeg + (4 * FCSPr) + FCSFruit + (4 + FCSDairy) + (0.5 * FCSSugar) + (0.5 * FCSFat)
) %>% mutate(
  FCSCat28 = case_when(
    FCS <= 28 ~ "Poor",
    between(FCS, 28.5,42) ~ "Bordeline",
    FCS > 42 ~ "Accepatble"
  )
)
  
funModeling::freq(BaseMenageBMZ_MLI2018, "FCSCat28")


# HDDS Mali 2018 ----------------------------------------------------------

BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  HDDSStapCer = case_when(Score_conso_alimcerealeSCA_1_mnag_atil_consom_hier_cereal == "Oui" ~ 1, TRUE ~ 0),
  HDDSStapRoot = case_when(Score_conso_alimracines_tuberculeSCA_2_mnag_atil_consom_h == "Oui" ~ 1, TRUE ~ 0),
  HDDSPulse = case_when(Score_conso_alimlegumineuses_noixSCA_3_mnag_atil_consom_h == "Oui" ~ 1, TRUE ~ 0),
  HDDSDairy = case_when(Score_conso_alimlait_autres_laitSCA_4_mnag_atil_consom_hi == "Oui" ~ 1, TRUE ~ 0 ),
  HDDSPrMeat = case_when(
  Score_conso_alimviandeSCA_51_mnag_atil_consom_hier_viand == "Oui" | Score_conso_alimfoie_rognonSCA_52_mnag_atil_consom_hier_f == "Oui" ~ 1, TRUE ~ 0
  ),
  HDDSPrEgg = case_when(Score_conso_alimoeufsSCA_54_mnag_atil_consom_hier_oeufs == "Oui" ~ 1, TRUE ~ 0 ),
  HDDSPrFish =  case_when(Score_conso_alimpoisson_eau_douce_merSCA_53_mnag_atil_con == "Oui" ~ 1, TRUE ~ 0 ),
  HDDSVeg = case_when(
    Score_conso_alimlegumes_feuillesSCA_6_mnag_atil_consom_hi == "Oui" ~ 1, TRUE ~ 0
  ),
  HDDSFruit = case_when(Score_conso_alimSCA_7_fruitsSCA_7_mnag_atil_consom_hier_f == "Oui" ~ 1, TRUE ~ 0),
  HDDSFat = case_when(Score_conso_alimSCA_8_huile_gras_beurreSCA_8_mnag_atil_co == "Oui" ~ 1, TRUE ~ 0 ),
  HDDSSugar = case_when(Score_conso_alimSCA_9_sucre_produit_sucreSCA_9_mnag_atil == "Oui" ~ 1, TRUE ~ 0),
  HDDSCond = case_when(Score_conso_alimSCA_10_epice_condimentSCA_10_mnag_atil_co == "Oui" ~ 1, TRUE ~ 0)
) %>% mutate(
  HDDS = HDDSStapCer + HDDSStapRoot + HDDSPulse + HDDSVeg + HDDSDairy + HDDSPrMeat +
    HDDSPrFish + HDDSPrEgg + HDDSFruit + HDDSFat + HDDSCond + HDDSSugar
) %>% mutate(
  HDDS_CH = case_when(
    HDDS >= 5 ~ "Phase1",
    HDDS == 4 ~ "Phase2",
    HDDS == 3 ~ "Phase3",
    HDDS == 2 ~ "Phase4",
    HDDS < 2 ~ "phase 5"
  )
)

funModeling::freq(BaseMenageBMZ_MLI2018, "HDDS_CH")


# rCSI 2018 Mali ----------------------------------------------------------

BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  rCSILessQlty = replace_na(Strategie_adaptation_alimentairestrategies_alimentairesCS, 0),
  rCSIBorrow = replace_na(Strategie_adaptation_alimentairestrategies_alimentaires_A,0),
  rCSIMealSize =replace_na(Strategie_adaptation_alimentairestrategies_alimentaires_D, 0),
  rCSIMealAdult = replace_na(Strategie_adaptation_alimentairestrategies_alimentaires_C, 0),
  rCSIMealNb = replace_na(Strategie_adaptation_alimentairestrategies_alimentaires_E, 0)
) %>% mutate(
  rCSI = rCSILessQlty + (2 *  rCSIBorrow) + (3 * rCSIMealAdult) +
    rCSIMealSize +  rCSIMealNb
) %>% mutate(
  rCSI_CH = case_when(
    rCSI <= 3 ~ "Phase1",
    between(rCSI, 4, 18) ~ "Phase2",
    rCSI >= 19 ~ "Phase3"
    
  )
)

funModeling::freq(BaseMenageBMZ_MLI2018, "rCSI_CH")

# HHS Mali 2018 -----------------------------------------------------------

BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  HHhSNoFood_FR = IDF2_nombre_manque_nourriture,
  HHhSBedHung_FR = IDF4_nombre_dormir_affame,
  HHhSNotEat_FR = IDF6_jour_nuit_affame) %>% mutate(
    HHhSNoFood_FR_r = case_when(
      HHhSNoFood_FR == "Rarement (une ou deux fois)" ~ 1,
      HHhSNoFood_FR == "Parfois (3-10 fois)" ~ 1,
      HHhSNoFood_FR == "Souvent (plus de 10 fois)" ~ 2,
      TRUE ~ 0
    ),
    HHhSBedHung_FR_r = case_when(
      HHhSBedHung_FR == "Rarement (1 à 2 fois)" ~ 1,
      HHhSBedHung_FR == "Parfois (3 à 10 fois)" ~ 1,
      HHhSBedHung_FR == "Souvent (plus de 10 fois)" ~ 2,
      TRUE ~0
    ),
    HHhSNotEat_FR_r = case_when(
      HHhSNotEat_FR == "Rarement (1 à 2 fois)" ~ 1,
      HHhSNotEat_FR == "Parfois (3 à 10 fois)" ~ 1,
      HHhSNotEat_FR == "Souvent (plus de 10 fois)" ~ 2,
      TRUE ~0
    )
  ) %>% mutate(
    HHhS = HHhSNoFood_FR_r + HHhSBedHung_FR_r + HHhSNotEat_FR_r
  ) %>% mutate(
    HHhS_CH = case_when(
      HHhS == 0 ~ "Phase1",
      HHhS == 1 ~ "Phase2",
      HHhS %in% c(2,3) ~ "Phase3",
      HHhS == 4 ~ "Phase4",
      HHhS >= 5 ~ "Phase5"
    )
  )
  
funModeling::freq(BaseMenageBMZ_MLI2018, "HHhS_CH")

# LCS Mali 2018 -----------------------------------------------------------

BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  LhCSIStress1 = SAN1_strategie_adap_A,
  LhCSIStress2 = SAN1_strategie_adap_D,
  LhCSIStress3 = SAN1_strategie_adap_E,
  LhCSIStress4 = SAN1_strategie_adap_G,
  LhCSICrisis1 = SAN1_strategie_adap_B,
  LhCSICrisis2 = SAN1_strategie_adap_C,
  LhCSICrisis3 = SAN1_strategie_adap_K,
  LhCSIEmergency1 = SAN1_strategie_adap_L, 
  LhCSIEmergency2 = SAN1_strategie_adap_N,
  LhCSIEmergency3 = SAN1_strategie_adap_H
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

funModeling::freq(BaseMenageBMZ_MLI2018, "LhCSICat")

# Codebook Mauritania 2018 ------------------------------------------------
# Le cari ainsi que les indicateurs et dépenses sont disponibles dans cette base
# pas de HHS ni de condiment ni des 12 groupes HDDS

BaseMenageMRT_2018<- to_factor(BaseMenageMRT_2018)
# codebook_MRT2018 <- var_label(BaseMenageMRT_2018)
# codebook_MRT2018 <- as.data.frame(do.call(rbind,codebook_MRT2018))
# codebook_MRT2018 <- codebook_MRT2018 %>% rownames_to_column()

# write_xlsx(codebook_MRT2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MRT2018.xlsx")

oldMRT <- c("IG.3", "IG.4", "CCM.3", "CCM.4", "CCM.5", "CCM.8", "totalhh")
newMRT <- c("ADMIN1Name", "ADMIN2Name", "HHHSex", "HHHAge", "RelationHHH", "HHHEduc", "HHSize")
data.table::setnames(BaseMenageMRT_2018, old = oldMRT,
                     new = newMRT)

# FCS 2018 Mauritania -----------------------------------------------------

BaseMenageMRT_2018 <- BaseMenageMRT_2018 %>% mutate(
  FCSStap = FCS1,
  FCSPulse = FCS2,
  FCSDairy = FCS3,
  FCSPr = FCS4,
  FCSVeg = FCS5,
  FCSFruit = FCS6,
  FCSFat = FCS7,
  FCSSugar = FCS8,
  FCSCat28 = FCG
) %>% mutate(
  FCSCond = NA
)

funModeling::freq(BaseMenageMRT_2018, "FCSCat28")


# HDDS Mauritanie 2018 ----------------------------------------------------
BaseMenageMRT_2018 <- BaseMenageMRT_2018 %>% mutate(
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

funModeling::freq(BaseMenageMRT_2018, "HDDS_CH")

# rCSI Mauritania 2018 ----------------------------------------------------

BaseMenageMRT_2018 <- BaseMenageMRT_2018 %>% mutate(
  rCSILessQlty = SS.1.1a,
  rCSIBorrow = SS.1.1b,
  rCSIMealSize = SS.1.1c,
  rCSIMealAdult  = SS.1.1d,
  rCSIMealNb = SS.1.1e,
  rCSI2 = rCSI
) %>% mutate(
  rCSI = rCSILessQlty + (2 * rCSIBorrow) + (3 * rCSIMealAdult) + rCSIMealSize + rCSIMealNb
) %>% mutate(
  rCSI_CH = case_when(
    rCSI <= 3 ~ "Phase1",
    between(rCSI, 4,18) ~ "Phase2",
    rCSI >= 19 ~ "Phase3"
  )
)

funModeling::freq(BaseMenageMRT_2018, "rCSI_CH")


# HHS 2018 Mauritania -----------------------------------------------------

BaseMenageMRT_2018 <- BaseMenageMRT_2018 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)

# LCS 2018 RT -------------------------------------------------------------

BaseMenageMRT_2018 <- BaseMenageMRT_2018 %>% mutate(
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
      ) %>% mutate(
        LhCSICat = fct_relevel(LhCSICat,c("Pasdestrategies", "StrategiesdeStress", "StrategiesdeCrise", "StrategiesdeUrgence"))
      )
funModeling::freq(BaseMenageMRT_2018, "lcsi")


# Codebook 2018 Niger -----------------------------------------------------
# pas de hhs 
BaseMenageNER_2018<- to_factor(BaseMenageNER_2018)
# codebook_NER2018 <- var_label(BaseMenageNER_2018)
# codebook_NER2018 <- as.data.frame(do.call(rbind,codebook_NER2018))
# codebook_NER2018 <- codebook_NER2018 %>% rownames_to_column()

# write_xlsx(codebook_NER2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_NER2018.xlsx")

oldNER <- c("id1", "id3b", "rc6", "rc7", "rc8", "rc9")
newNER <- c("ADMIN1Name", "ADMIN2Name", "HHSize", "HHHSex", "RelationHHH", "HHHEduc")
data.table::setnames(BaseMenageNER_2018, old = oldNER,
                     new = newNER)
BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
  HHHAge = NA
)

# FCS 2018 Niger ----------------------------------------------------------

BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
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
  FCSCond = replace_na(EPICEB,0),
  FCS2 = FCS
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

funModeling::freq(BaseMenageNER_2018, "FCSCat28")


# HDDS 2018 Niger ---------------------------------------------------------

BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
  HDDSStapCer = CERA,
  HDDSStapRoot = RACTUBA,
  HDDSPulse = LEGUMINOIXA,
  HDDSVegOrg = LEGUMORANA,
  HDDSVegGre = LEGUMVERTA,
  HDDSVegOth = AUTRLEGUMA,
  HDDSFruitOrg = FRUITORANA,
  HDDSFruitOth = AUTRFRUITA,
  HDDSPrMeatO = VIANDEA,
  HDDSPrMeatF = FOIROGNONA,
  HDDSPrFish = POISSONA,
  HDDSPrEgg = OEUFA,
  HDDSDairy = LAITA,
  HDDSFat = HUILEA,
  HDDSSugar  = SUCREA,
  HDDSCond = EPICEA,
  HDDS2 = HDDS
) %>% mutate(
  HDDSVeg = case_when(HDDSVegOrg == "Oui" | HDDSVegGre == "Oui" | HDDSVegOth == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrMeat = case_when(HDDSPrMeatF == "Oui" | HDDSPrMeatO == "Oui" ~ 1, TRUE ~ 0),
  HDDSFruit = case_when(HDDSFruitOrg == "Oui" | HDDSFruitOth == "Oui" ~ 1, TRUE ~ 0),
  HDDSStapCer = case_when(HDDSStapCer == "Oui" ~ 1, TRUE ~ 0),
  HDDSStapRoot = case_when(HDDSStapRoot  == "Oui" ~ 1, TRUE ~ 0),
  HDDSPulse = case_when(HDDSPulse == "Oui" ~ 1, TRUE ~ 0),
  HDDSDairy = case_when(HDDSDairy == "Oui" ~ 1, TRUE ~ 0),
  HDDSFat = case_when(HDDSFat == "Oui" ~ 1, TRUE ~ 0),
  HDDSSugar = case_when(HDDSSugar == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrEgg = case_when(HDDSPrEgg  == "Oui" ~ 1, TRUE ~ 0),
  HDDSPrFish = case_when(HDDSPrFish == "Oui" ~ 1, TRUE ~ 0),
  HDDSCond = case_when(HDDSCond == "Oui"~ 1, TRUE ~ 0) 
) %>% mutate(
  HDDS = HDDSStapCer + HDDSStapRoot + HDDSVeg + HDDSFruit +
    HDDSPrMeat + HDDSPrEgg + HDDSPrFish + HDDSPulse + HDDSDairy + HDDSFat + HDDSSugar + HDDSCond) %>% 
  mutate(HDDS_CH = case_when(
    HDDS >= 5 ~ "Phase1",
    HDDS == 4 ~ "Phase2",
    HDDS == 3 ~ "Phase3",
    HDDS == 2 ~ "Phase4",
    HDDS < 2 ~ "Phase5")
)

funModeling::freq(BaseMenageNER_2018, "HDDS_CH")


# rCSI 2018 Niger ---------------------------------------------------------

BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
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


funModeling::freq(BaseMenageNER_2018, "rCSI_CH")

# HHS NER 2018 -------------------------------------------------------------

BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)


# LCS NER 2018 -----------------------------------------------------------------

BaseMenageNER_2018 <- BaseMenageNER_2018 %>%  mutate(
  LhCSIStress1 = SS19A,
  LhCSIStress2 = SS25A,
  LhCSIStress3 = SS30A,
  LhCSIStress4 = SS31A,
  LhCSICrisis1 = SS20A,
  LhCSICrisis2 = SS18A,
  LhCSICrisis3 = SS22A,
  LhCSIEmergency1 = SS21A,
  LhCSIEmergency2 = SS23A,
  LhCSIEmergency3 = SS34A
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

funModeling::freq(BaseMenageMRT_2018, "LhCSICat")

# BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% select(FCSStap, FCSStapCer, FCSStapRoot, FCSPulse, FCS)

# Codebook Tchad 2018 -----------------------------------------------------
BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>%  to_factor()
# codebook_TCD2018 <- var_label(BaseMenageTCD_2018)
# codebook_TCD2018 <- as.data.frame(do.call(rbind,codebook_TCD2018))
# codebook_TCD2018 <- codebook_TCD2018 %>% rownames_to_column()

# write_xlsx(codebook_TCD2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_TCD2018.xlsx")

oldTCD <- c("region", "departement", "Taille_Globale_Menage", "sexe_cm", "age_CM", "instruction_cm", "statut_matrimonial_cm")
newTCD <- c("ADMIN1Name", "ADMIN2Name", "HHSize", "HHHSex", "HHHAge", "HHHEduc", "RelationHHH")

data.table::setnames(BaseMenageTCD_2018, old = oldTCD,
                     new = newTCD)

# FCS Tchad 2018 ----------------------------------------------------------

BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>% mutate(
  FCSStapCer = jour_conso_cereale,
  FCSStapRoot = jour_conso_tuberc,
  FCSPulse = jour_conso_legumine,
  FCSDairy = jour_conso_laitprod,
  FCSPr = jour_gpe_viande,
  FCSVeg = jour_gpe_legume,
  FCSFruit = jour_gpe_fruit,
  FCSFat = jour_conso_huile,
  FCSSugar = jour_conso_sucre,
  FCSCond = jour_conso_epice
) %>% mutate(
  FCSStap = FCSStapCer + FCSStapRoot
) %>% mutate(
  FCSStap = ifelse( FCSStap > 7, 7, FCSStap)
) %>% mutate(
  FCS = (FCSStap * 2) + (FCSPulse * 3) + FCSVeg + (FCSPr * 4)+
    (FCSDairy * 4) + FCSFruit + (0.5 * FCSFat) + (0.5 * FCSSugar)
) %>% mutate(
  FCSCat28 = case_when(
    FCS <= 28 ~ "Poor",
    between(FCS,28.5,42) ~ "Bordeline",
    FCS > 42 ~ "Acceptable"
  )
)

funModeling::freq(BaseMenageTCD_2018, "FCSCat28")
# HDDS Tchad 2018 ---------------------------------------------------------

BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>% mutate(
  HDDSStapCer = DDSCereal_Tuberc,
  HDDSStapRoot = NA,
  HDDSPrMeat = DDSProteine,
  HDDSPrFish = NA,
  HDDSPrEgg = NA,
  HDDSPulse = DDSLegumin,
  HDDSVeg = DDSLegum,
  HDDSDairy = DDSLait,
  HDDSFat = DDSHuile,
  HDDSFruit = DDSFruit,
  HDDSSugar = DDSSucre,
  HDDSCond = NA
) %>% mutate(
  HDDS = HDDSStapCer + HDDSVeg + HDDSFruit  + 
    HDDSPulse + HDDSDairy + HDDSFat + HDDSSugar
) %>% mutate(
  HDDS_CH = case_when(HDDS >= 5 ~ "Phase1",
                      HDDS == 4 ~ "Phase2",
                      HDDS == 3 ~ "Phase3",
                      HDDS == 2 ~ "Phase4",
                      HDDS < 2 ~ "Phase5")
)

funModeling::freq(BaseMenageTCD_2018, "HDDS_CH")

# rCSI Tchad 2018 ---------------------------------------------------------

BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>% mutate(
  rCSILessQlty = Q06_1,
  rCSIBorrow = Q06_2,
  rCSIMealSize = Q06_3,
  rCSIMealAdult = Q06_4,
  rCSIMealNb = Q06_5
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

funModeling::freq(BaseMenageTCD_2018, "rCSI_CH")

# HHS Tchad 2018--------------------------------------------------------------------
BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)


# LCS Tchad 2018 ----------------------------------------------------------

BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>% mutate(
  LhCSIStress1 = Q06_9,
  LhCSIStress2 = Q06_10,
  LhCSIStress3 = Q06_11,
  LhCSICrisis1 = Q06_13,
  LhCSIStress4 = Q06_12,
  LhCSICrisis1 = Q06_13,
  LhCSICrisis2 = Q06_14,
  LhCSICrisis3 = Q06_15,
  LhCSIEmergency1 = Q06_16,
  LhCSIEmergency2 = Q06_17,
  LhCSIEmergency3 = Q06_18,
) %>%  mutate(
  stress_coping = case_when(
    LhCSIStress1 == "Oui" | str_detect(LhCSIStress1,"Non") ~ "Oui",
    LhCSIStress2 == "Oui" | str_detect(LhCSIStress2,"Non") ~ "Oui",
    LhCSIStress3 == "Oui" | str_detect(LhCSIStress3,"Non") ~ "Oui",
    LhCSIStress4 == "Oui" | str_detect(LhCSIStress4,"Non") ~ "Oui",
    TRUE ~ "Non")) %>% 
    
    mutate(crisis_coping = case_when(
      LhCSICrisis1 == "Oui" | str_detect(LhCSICrisis1,"Non") ~ "Oui",
      LhCSICrisis2 == "Oui" | str_detect(LhCSICrisis2,"Non") ~ "Oui",
      LhCSICrisis3 == "Oui" | str_detect(LhCSICrisis3,"Non") ~ "Oui",
      TRUE ~ "Non")) %>% 
      mutate(emergency_coping = case_when(
        LhCSIEmergency1 == "Oui" | str_detect(LhCSIEmergency1,"Non") ~ "Oui",
        LhCSIEmergency2 == "Oui" | str_detect(LhCSIEmergency2,"Non") ~ "Oui",
        LhCSIEmergency3 == "Oui" | str_detect(LhCSIEmergency3,"Non") ~ "Oui",
        TRUE ~ "Non")) %>% mutate(
      LhCSICat = case_when(
        emergency_coping == "Oui" ~ "StrategiesdeUrgence",
        crisis_coping == "Oui" ~ "StrategiesdeCrise",
        stress_coping == "Oui" ~ "StrategiesdeStress",
        TRUE ~ "PasdeStrategies"
      )
    )
funModeling::freq(BaseMenageTCD_2018, "LhCSICat")

# Vérification variables et compilation -----------------------------------
variables <- c("ADMIN1Name", "ADMIN2Name", "HHSize", "HHHSex", 
               "HHHAge", "HHHEduc", "RelationHHH",
                "FCSStap", "FCSPulse", "FCSDairy", 
               "FCSPr", "FCSVeg", "FCSFruit", "FCSFat", 
               "FCSSugar","FCS", "FCSCat28", "FCSCond", "HDDSStapCer", 
               "HDDSStapRoot", "HDDSPulse", "HDDSDairy", 
               "HDDSPrMeat", "HDDSPrFish", "HDDSPrEgg", 
               "HDDSVeg", "HDDSFruit", "HDDSFat", "HDDSSugar", 
               "HDDSCond", "HDDS", "HDDS_CH", "rCSILessQlty", 
               "rCSIBorrow", "rCSIMealSize", "rCSIMealAdult", 
               "rCSIMealNb", "rCSI", "rCSI_CH", "LhCSIStress1", 
               "LhCSIStress2", "LhCSIStress3", "LhCSIStress4", 
               "LhCSICrisis1", "LhCSICrisis2", "LhCSICrisis3", 
               "LhCSIEmergency1", "LhCSIEmergency2", 
               "LhCSIEmergency3", "stress_coping", 
               "crisis_coping", "emergency_coping", 
               "LhCSICat", "HHhSNoFood_FR", "HHhSBedHung_FR", "HHhSNotEat_FR", "HHhS", "HHhS_CH")
data <- as.data.frame(variables)
writexl::write_xlsx(data, "data.xlsx")
# Burkina
setdiff(variables,names(BaseMenageBFA_2018))
BaseMenageBFA_2018 <- BaseMenageBFA_2018 %>% mutate(
  ADMIN0Name = "Burkina Fasso",
  Année = 2018,
  Survey = "Baseline",
  SurveyId = 1,
  Commentaire = "Base Ménage 24 décembre 2018"
)  %>% select(ADMIN0Name,Survey, SurveyId,Année, Commentaire, which(names(BaseMenageBFA_2018) %in% variables))

# Mali
setdiff(variables,names(BaseMenageBMZ_MLI2018))
BaseMenageBMZ_MLI2018 <- BaseMenageBMZ_MLI2018 %>% mutate(
  ADMIN0Name = "Mali",
  Année = 2018,
  Survey = "Baseline BMZ",
  SurveyId = 1,
  Commentaire = "BMZ Base Ménage 2018"
) %>% select(ADMIN0Name,Survey, SurveyId,Année, Commentaire, which(names(BaseMenageBMZ_MLI2018) %in% variables))

# Mauritanie
setdiff(variables,names(BaseMenageMRT_2018))
BaseMenageMRT_2018 <- BaseMenageMRT_2018 %>% mutate(
  ADMIN0Name = "Mauritania",
  Année = 2018,
  Survey = "Baseline",
  SurveyId = 1,
  Commentaire = "Base Ménage FFA 2018"
)  %>% select(ADMIN0Name,Survey, SurveyId,Année, Commentaire, which(names(BaseMenageMRT_2018) %in% variables))

# Niger
setdiff(variables,names(BaseMenageNER_2018))
BaseMenageNER_2018 <- BaseMenageNER_2018 %>% mutate(
  ADMIN0Name = "Niger",
  Année = 2018,
  Survey = "Baseline",
  SurveyId = 1,
  Commentaire = "Base Ménage TICSP Novembre 2018"
)  %>% select(ADMIN0Name,Survey, SurveyId,Année, Commentaire, which(names(BaseMenageNER_2018) %in% variables))

# Tchad
setdiff(variables,names(BaseMenageTCD_2018))
BaseMenageTCD_2018 <- BaseMenageTCD_2018 %>% mutate(
  ADMIN0Name = "Chad",
  Année = 2018,
  Survey = "Baseline",
  SurveyId = 1,
  Commentaire = "Base Ménage  2018"
)  %>% select(ADMIN0Name,Survey, SurveyId,Année, Commentaire, which(names(BaseMenageTCD_2018) %in% variables))

Baseline_regionale2018 <- rbind(BaseMenageBMZ_MLI2018, BaseMenageBFA_2018, BaseMenageMRT_2018,
                                BaseMenageNER_2018, BaseMenageTCD_2018)

Baseline_regionale2018 <- Baseline_regionale2018 %>% apply_labels(
  ADMIN1Name = "Région/Wilaya",
  ADMIN2Name = "Département/MOUGHTAA",
  HHHSex = "Sexe du chef de ménage",
  HHHAge = "Age du chef de ménage",
  HHHEduc = "Niveau d'instruction du chef de ménage",
  RelationHHH = "Situation matrimoniale du chef de ménage",
  HHSize = "Taille du ménage",
  FCSStap = "Consommation de céréal au cours des 7 derniers jours", 
  FCSPulse = "Consommation de légumineuse au cours des 7 derniers jours", 
  FCSDairy = "Consommation de Lait et Produits laitier au cours des 7 derniers jours",
  FCSPr = "Consommation de viande, poisson oeuf au cours des 7 derniers jours", 
  FCSVeg = "Consommation de Légumes au cours des 7 derniers jours", 
  FCSFruit = "Consommation de Fruit au cours des 7 derniers jours", 
  FCSFat = "Consommation de Huile et matières grasse au cours des 7 derniers jours", 
  FCSSugar = "Consommation de Sucre au cours des 7 derniers jours", 
  FCS = "Score de Consommation Alimentaire", 
  FCSCat28 = "Groupe/Catégorie Score de consommation alimentaire (SCA)", 
  FCSCond = "Consommation de Condiment  au cours des 7 derniers jours", 
  HDDSStapCer = "Hier, consommation de céréales", 
  HDDSStapRoot = "Hier, consommation de tubercules", 
  HDDSPulse = "Hier, consommation de légumineuses", 
  HDDSDairy = "Hier, consommation de Lait et produits laitiers", 
  HDDSPrMeat = "Hier, consommation de viande", 
  HDDSPrFish = "Hier, consommation de poisson", 
  HDDSPrEgg = "Hier, consommation de œuf", 
  HDDSVeg = "Hier, consommation de légumes", 
  HDDSFruit = "Hier, consommation de Fruit", 
  HDDSFat = "Hier, consommation de Huile et matères grasse", 
  HDDSSugar = "Hier, consommation de Sucre", 
  HDDSCond = "Hier, consommation de Condiment", 
  HDDS = "Score de Diversité Alimentaires  des Ménages (HDDS)", 
  HDDS_CH = "Groupe/Catégorie Score de Diversité Alimentaire des ménages", 
  rCSILessQlty = "Consommation des aliments moins préférés et moins chers au cours des 7 derniers jours", 
  rCSIBorrow = "Emprunter de la nourriture ou compter sur l'aide des parents au cours des 7 derniers jours", 
  rCSIMealSize = "Diminuer la quantité consommé pendant les repas au cours des 7 derniers jours", 
  rCSIMealAdult = "Restreindre la consommation des adultes pour nourrir les enfants au cours des 7 derniers jours", 
  rCSIMealNb = "Diminuer le nombre de repas par jour au cours des 7 derniers jours", 
  rCSI = "Indice Réduit des Stratégie de Survie", 
  rCSI_CH = "Groupe/Catégorie rCSI", 
  LhCSIStress1 = "Stratégie de Stress 1 au cours des 30 derniers jours", 
  LhCSIStress2 = "Stratégie de Stress 2 au cours des 30 derniers jours", 
  LhCSIStress3 = "Stratégie de Stress 3 au cours des 30 derniers jours", 
  LhCSIStress4 = "Stratégie de Stress 4 au cours des 30 derniers jours", 
  LhCSICrisis1 = "Stratégie de Crise 1 au cours des 30 derniers jours", 
  LhCSICrisis2 = "Stratégie de Crise 2 au cours des 30 derniers jours", 
  LhCSICrisis3 = "Stratégie de Crise 3 au cours des 30 derniers jours", 
  LhCSIEmergency1 = "Stratégie d'urgence 1 au cours des 30 derniers jours", 
  LhCSIEmergency2 = "Stratégie d'urgence 2 au cours des 30 derniers jours", 
  LhCSIEmergency3 = "Stratégie d'urgence 3 au cours des 30 derniers jours", 
  stress_coping = "Stratégie de Stress", 
  crisis_coping = "Stratégie de Crise", 
  emergency_coping = "Stratégie d'urgence", 
  LhCSICat = "Groupe/Catégorie Statégie d'adaption aux moyens d'existence (LCS)", 
  HHhSNoFood_FR = "Fréquence manque de nourriture dûe à une manque de ressources au cours des 30 derniers jours", 
  HHhSBedHung_FR = "Fréquence dormir affamé au cours des 30 derniers jours", 
  HHhSNotEat_FR = "Fréquence rien manger jour  et nuit au cours des 30 derniers jours", 
  HHhS = "Indice  Domestique de la Faim", "HHhS_CH = Groupe/Catégorie HHS",
  HHhS_CH = "Groupe/Catégorie HHS"
)
Baseline_regionale2018 <- Baseline_regionale2018 %>% rename(
  Annee = Année
)

write_sav(Baseline_regionale2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Processed/Last/Baseline_regionale2018.sav")

# setdiff(variables,names(Baseline_regionale2019))


