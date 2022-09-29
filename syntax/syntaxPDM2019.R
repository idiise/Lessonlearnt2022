# import package ----------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)
library(expss)

# import Datasets ---------------------------------------------------------

setwd("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/PDM 2019/")

List_data <- dir_ls(regexp = ".dta") %>% 
  map(read_stata)
nom_base <- names(List_data)
nom_base <- str_replace(string = nom_base,pattern = ".dta", replacement = "")
names(List_data) <- nom_base
list2env(List_data, .GlobalEnv)

# Codebook Mauritanie 2019 PDM --------------------------------------------

BasePDMMRT_2019 <- to_factor(BasePDMMRT_2019)
codebook_BFAPDM2019 <- var_label(BasePDMMRT_2019)
codebook_BFAPDM2019 <- as.data.frame(do.call(rbind,codebook_BFAPDM2019))
codebook_BFAPDM2019 <- codebook_BFAPDM2019 %>% rownames_to_column()

write_xlsx(codebook_BFAPDM2019, "C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/Codebook/codebook_BFAPDM2019.xlsx")
oldMRTPdm2019 <- c("IG_3", "IG_4", "CCM_3", "CCM_4", "CCM_5", "CCM_8", "totalhh")
newMRTPdm2019 <- c("ADMIN1Name", "ADMIN2Name", "HHHSex", "HHHAge", "RelationHHH", "HHHEduc", "HHSize")
data.table::setnames(BasePDMMRT_2019, old = oldMRTPdm2019,
                     new = newMRTPdm2019)

# FCS Mauritanie PDM 2019 -------------------------------------------------

BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate(
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

funModeling::freq(BasePDMMRT_2019, "FCSCat28")

# HDDS Mauritanie 2019 PDM ------------------------------------------------

BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate(
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

funModeling::freq(BasePDMMRT_2019, "HDDS_CH")

# rCSI Mauritanie PDM 2019 ------------------------------------------------

BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate(
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

funModeling::freq(BasePDMMRT_2019, "rCSI_CH")

# HHS Mauritania 2019 PDM --------------------------------------------------

 # pas de HHS dans la base
BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate(
  HHhSNoFood_FR = NA,
  HHhSBedHung_FR = NA,
  HHhSNotEat_FR = NA,
  HHhS = NA,
  HHhS_CH = NA 
)


# Lcs Mauritania PDM 2019 -------------------------------------------------

BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate(
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
funModeling::freq(BasePDMMRT_2019, "LhCSICat")


# verification et Merging -------------------------------------------------
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
# Mrt
setdiff(variables,names(BasePDMMRT_2019))
BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate(
  ADMIN0Name = "Mauritania",
  Annee = 2019,
  Survey = "PDM",
  SurveyId = 1,
  Commentaire = "Base ménage PDM 2019 "
)  %>% select(ADMIN0Name,Survey, SurveyId,Annee, Commentaire, which(names(BasePDMMRT_2019) %in% variables))

Baseline_regionale2018 <- read_sav("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Processed/Last/Baseline_regionale2018.sav")
Baseline_regionale2019 <- read_sav("C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Processed/Last/Baseline_regionale2019.sav")
setdiff(names(Baseline_regionale2018),names(Baseline_regionale2019))
setdiff(names(Baseline_regionale2019),names(Baseline_regionale2018))
Baseline_regionale2018_2019 <- rbind.data.frame(Baseline_regionale2018, Baseline_regionale2019)
Baseline_regionale2018 <- Baseline_regionale2018 %>% mutate_if(
  is.numeric, as.character
)
Baseline_regionale2019 <- Baseline_regionale2019 %>% mutate_if(
  is.numeric, as.character
)

BasePDMMRT_2019 <- BasePDMMRT_2019 %>% mutate_if(
  is.numeric, as.character
)

Baseline_regionale2018_2019_final <- rbind(Baseline_regionale2018_2019,BasePDMMRT_2019)
setdiff(names(Baseline_regionale2018_2019),names(BasePDMMRT_2019))
setdiff(names(BasePDMMRT_2019),names(Baseline_regionale2018_2019))





write_sav(Baseline_regionale2018_2019_final,"C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/data/Processed/Last/Baseline_regionale2018_2019_final.sav" )
write_xlsx(class_2019,"C:/Users/USER MSI/Documents/R Project/Lessonlearnt2022/class_2019.xlsx" )


