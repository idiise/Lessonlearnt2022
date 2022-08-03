
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

BaseMenageBFA_2018<- BaseMenageBFA_2018 %>%  to_factor()
codebook_BFA2018 <- var_label(BaseMenageBFA_2018)
codebook_BFA2018 <- as.data.frame(do.call(rbind,codebook_BFA2018))
codebook_BFA2018 <- codebook_BFA2018 %>% rownames_to_column()

write_xlsx(codebook_BFA2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFA2018.xlsx")


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


# Codebook Mali 2018 --------------------------------------------------------------------

BaseMenageBMZ_MLI2018<- to_factor(BaseMenageBMZ_MLI2018)
codebook_MLI2018 <- var_label(BaseMenageBMZ_MLI2018)
codebook_MLI2018 <- as.data.frame(do.call(rbind,codebook_MLI2018))
codebook_MLI2018 <- codebook_MLI2018 %>% rownames_to_column()

write_xlsx(codebook_MLI2018, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_MLI2018.xlsx")


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
