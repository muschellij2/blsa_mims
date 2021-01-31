#' This script generates "Table 1" for the paper. 
#' 
#' It only summarizes (participant, visit) pairs for which we have data in 
#' the masterfile.
#' 
#' Input: 
#' > /data_processed/2021-01-18-measures_masterfile.rds

rm(list = ls())
library(tidyverse)

# ------------------------------------------------------------------------------
# read data 

# measures_masterfile -- file with minute-level PA measures 
measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
dim(measures_masterfile)
# [1] 6147240      10
measures_masterfile %>% select(subj_id, visit_id) %>% distinct() %>% nrow()
# [1] 721
length(unique(measures_masterfile$subj_id))
# [1] 721

# mastervisit -- visit-specific participant's: comorbidities, age, BMI from mastervisit.
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) %>%
  rename(idno = IDNo, visit = Visit, dov = DOV)
dim(mastervisit)
# [1] 25571   113

# masterdemog -- participant's: gender, race, and years of education
masterdemog_fpath <- paste0(here::here(), "/covariates/2021-01-19-masterdemog.rdata")
masterdemog <- get(load(masterdemog_fpath, ex <- new.env()), envir = ex) 
dim(masterdemog)
# [1] 3445   15

# interview -- visit-specific participant's: emplyment 
interview_fpath <- paste0(here::here(), "/covariates/2021-01-19-blsa_interview_mdhx_teleform.rdata")
interview <- get(load(interview_fpath, ex <- new.env()), envir = ex)
dim(interview)
# [1] 7197  760


# ------------------------------------------------------------------------------
# generate subsets of these data 

mastervisit_SUB <- 
  measures_masterfile %>%
  select(subj_id, visit_id) %>% 
  distinct() %>%
  left_join(mastervisit, by = c("subj_id" = "idno", visit_id = "visit")) 

masterdemog_SUB <- 
  measures_masterfile %>%
  select(subj_id, visit_id) %>% 
  distinct() %>%
  left_join(masterdemog, by = c("subj_id" = "idno")) %>%
  select(subj_id, BLSA_Race, gender, educ_years, educat, Educ_edited) %>%
  as.data.frame()

interview_SUB <- 
  measures_masterfile %>%
  select(subj_id, visit_id) %>% 
  distinct() %>%
  left_join(interview, by = c("subj_id" = "idno", visit_id = "visit")) %>%
  rename(reported_race = DEMO09) 



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# generate aggregates

# objects to store the values
table1_var  <- rep(NA, 100)
table1_val1 <- rep(NA, 100)
table1_val2 <- rep(NA, 100)
table1_val3 <- rep(NA, 100)
table1_val4 <- rep(NA, 100)
table1_val5 <- rep(NA, 100)

# age 
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Age"
table1_val1[i] <- mean(mastervisit_SUB$Age)
table1_val2[i] <- sd(mastervisit_SUB$Age)
table1_val3[i] <- median(mastervisit_SUB$Age)
table1_val4[i] <- min(mastervisit_SUB$Age)
table1_val5[i] <- max(mastervisit_SUB$Age)

# weight
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Weight [kg]"
table1_val1[i] <- mean(mastervisit_SUB$WtKg)
table1_val2[i] <- sd(mastervisit_SUB$WtKg)
table1_val3[i] <- median(mastervisit_SUB$WtKg)
table1_val4[i] <- min(mastervisit_SUB$WtKg)
table1_val5[i] <- max(mastervisit_SUB$WtKg)

# height
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Height [cm]"
table1_val1[i] <- mean(mastervisit_SUB$HtCm)
table1_val2[i] <- sd(mastervisit_SUB$HtCm)
table1_val3[i] <- median(mastervisit_SUB$HtCm)
table1_val4[i] <- min(mastervisit_SUB$HtCm)
table1_val5[i] <- max(mastervisit_SUB$HtCm)

# BMI
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "BMI"
table1_val1[i] <- mean(mastervisit_SUB$BMI)
table1_val2[i] <- sd(mastervisit_SUB$BMI)
table1_val3[i] <- median(mastervisit_SUB$BMI)
table1_val4[i] <- min(mastervisit_SUB$BMI)
table1_val5[i] <- max(mastervisit_SUB$BMI)


# -----------------------------------------------------------------------------
# race 

# race mapping
masterdemog %>%
  select(racecd, BLSA_Race) %>%
  filter(BLSA_Race != "NA") %>%
  distinct() %>%
  arrange(racecd) %>%
  as.data.frame() 
#    racecd                             BLSA_Race
# 1       0 Other Asian or Other Pacific Islander
# 2       1                                 White
# 3       2                                 Black
# 4       3      American Indian or Alaska Native
# 5       4                               Chinese
# 6       5                              Japanese
# 7       6                              Hawaiian
# 8       7                        Other NonWhite
# 9       8                              Filipino
# 10      9                      Not Classifiable

denom_cnt <- length(unique(measures_masterfile$subj_id))

#' Make table with minimum value of "reported race" (reported_race_REPL)
#' for each participant based on their ALL visits. 
#' This value (if and only if unique across all non-NA race values for a participant) 
#' is then used to replace missing race value in visit-specific race field. 
interview_SUB_race0 <- interview %>% 
  select(idno, reported_race = DEMO09) %>%
  group_by(idno) %>%
  summarise(reported_race_REPL = min(reported_race, na.rm = TRUE),
            n_dist = n_distinct(reported_race, na.rm = TRUE)) %>%
  mutate(reported_race_REPL = ifelse(n_dist != 1, NA, reported_race_REPL)) %>%
  select(-n_dist)
interview_SUB_race <- measures_masterfile %>%
  select(subj_id, visit_id) %>% 
  distinct() %>%
  left_join(interview, by = c("subj_id" = "idno", visit_id = "visit")) %>%
  select(subj_id, visit_id, reported_race = DEMO09) %>%
  left_join(interview_SUB_race0, by = c("subj_id" = "idno")) %>%
  mutate(reported_race_FINAL = ifelse(is.na(reported_race), reported_race_REPL, reported_race))

# race
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Race: White"
table1_val1[i] <- sum(interview_SUB_race$reported_race_FINAL == 1, na.rm = TRUE)
table1_val2[i] <- sum(interview_SUB_race$reported_race_FINAL == 1, na.rm = TRUE)/denom_cnt

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Race: Black"
table1_val1[i] <- sum(interview_SUB_race$reported_race_FINAL == 2, na.rm = TRUE)
table1_val2[i] <- sum(interview_SUB_race$reported_race_FINAL == 2, na.rm = TRUE)/denom_cnt

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Race: Not Other"
table1_val1[i] <- sum(interview_SUB_race$reported_race_FINAL %in% c(0, 3:9), na.rm = TRUE)
table1_val2[i] <- sum(interview_SUB_race$reported_race_FINAL %in% c(0, 3:9), na.rm = TRUE)/denom_cnt

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Race: Not reported"
table1_val1[i] <- sum(is.na(interview_SUB_race$reported_race_FINAL))
table1_val2[i] <- sum(is.na(interview_SUB_race$reported_race_FINAL))/denom_cnt


# -----------------------------------------------------------------------------
# self-rated health 

table(mastervisit_SUB$SFHealth)

# Self-reported good/very good/excellent health
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Self-reported health: good/very good/excellent"
table1_val1[i] <- sum(mastervisit_SUB$SFHealth %in% 1:3, na.rm = TRUE)
table1_val2[i] <- sum(mastervisit_SUB$SFHealth %in% 1:3, na.rm = TRUE)/denom_cnt

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Self-reported health: fair/poor"
table1_val1[i] <- sum(mastervisit_SUB$SFHealth %in% 4:5, na.rm = TRUE)
table1_val2[i] <- sum(mastervisit_SUB$SFHealth %in% 4:5, na.rm = TRUE)/denom_cnt

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Self-reported health: Not reported"
table1_val1[i] <- sum(is.na(mastervisit_SUB$SFHealth))
table1_val2[i] <- sum(is.na(mastervisit_SUB$SFHealth))/denom_cnt


# -----------------------------------------------------------------------------
# comorbidities

# 59         SMDHX3                MEDH: To date summary - Doctor told heart attack
# 60         SMDHX4                         MEDH: To date summary - Doctor told CHF
# 61         SMDHX5                      MEDH: To date summary - Doctor told angina
# 62         SMDHX6                  MEDH: To date summary - Doctor told bronchitis
# 63         SMDHX7                      MEDH: To date summary - Doctor told asthma
# 64         SMDHX8                   MEDH: To date summary - Doctor told cirrhosis
# 65         SMDHX9                   MEDH: To date summary - Doctor told hepatitis
# 66        SMDHX10                 MEDH: To date summary - Doctor told HIV or AIDS
# 67        SMDHX11              MEDH: To date summary - Doctor told kidney disease
# 68        SMDHX12                      MEDH: To date summary - Doctor told stroke
# 69        SMDHX13                         MEDH: To date summary - Doctor told TIA
# 70        SMDHX14       MEDH: To date summary - Doctor told peripheral neuropathy
# 71        SMDHX15                  MEDH: To date summary - Ever told hypertensive
# 72        SMDHX16                    MEDH: To date summary - Doctor told diabetes
# 73        SMDHX17                  MEDH: To date summary - High blood cholesterol
# 74        SMDHX18        MEDH: To date summary - Ever had any vascular procedures
# 75        SMDHX19                      MEDH: To date summary - Doctor told cancer
# 76        SMDHX20              MEDH: To date summary - Doctor told osteoarthritis
# 77        SMDHX21             MEDH: To date summary - Doctor told spinal stenosis
# 78        SMDHX22                  MEDH: To date summary - Ever told osteoporosis
# 79        SMDHX23          MEDH: To date summary - Doctor told had tissue disease
# 80        SMDHX24              MEDH: To date summary - Doctor told had Parkinsons
# 81        SMDHX25                         MEDH: To date summary - Doctor told PAD
# 82        SMDHX26                         MEDH: To date summary - Doctor told PVD
# 83       SMDHX27A       MEDH: To date summary - Doctor told had cataract in 1 eye
# 84       SMDHX27B  MEDH: To date summary - Doctor told had cataracts in both eyes
# 85       SMDHX27F        MEDH: To date summary - Doctor told had cataract surgery
# 86        SMDHX28                       MEDH: To date summary - Doctor told ulcer
# 87        SMDHX29                  MEDH: To date summary - Doctor told depression


# heart attack, CHF, angina (ischemic chest pain), vascular procedure, Peripheral artery disease
# (MI/CHF/angina/vascular procedure/PAD)
table(mastervisit_SUB$SMDHX3); sum(is.na(table(mastervisit_SUB$SMDHX3)))
table(mastervisit_SUB$SMDHX4); sum(is.na(table(mastervisit_SUB$SMDHX4)))
table(mastervisit_SUB$SMDHX5); sum(is.na(table(mastervisit_SUB$SMDHX5)))
table(mastervisit_SUB$SMDHX18); sum(is.na(table(mastervisit_SUB$SMDHX18)))
table(mastervisit_SUB$SMDHX25); sum(is.na(table(mastervisit_SUB$SMDHX25)))

# hypertensive (Hyperlipidemia) 
table(mastervisit_SUB$SMDHX15); sum(is.na(table(mastervisit_SUB$SMDHX15)))

# High blood cholesterol
table(mastervisit_SUB$SMDHX17); sum(is.na(table(mastervisit_SUB$SMDHX17)))

# Stroke, TIA
table(mastervisit_SUB$SMDHX12); sum(is.na(table(mastervisit_SUB$SMDHX12)))
table(mastervisit_SUB$SMDHX13); sum(is.na(table(mastervisit_SUB$SMDHX13)))

# bronchitis, asthma (Pulmonary disease)
table(mastervisit_SUB$SMDHX6); sum(is.na(table(mastervisit_SUB$SMDHX6)))
table(mastervisit_SUB$SMDHX7); sum(is.na(table(mastervisit_SUB$SMDHX7)))

# Diabetes
table(mastervisit_SUB$SMDHX16); sum(is.na(table(mastervisit_SUB$SMDHX16)))

# Cancer
table(mastervisit_SUB$SMDHX19); sum(is.na(table(mastervisit_SUB$SMDHX19)))

# Osteoarthritis
table(mastervisit_SUB$SMDHX20); sum(is.na(table(mastervisit_SUB$SMDHX20)))







i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Self-reported health: good/very good/excellent"
table1_val1[i] <- sum(mastervisit_SUB$SFHealth %in% 1:3, na.rm = TRUE)
table1_val2[i] <- sum(mastervisit_SUB$SFHealth %in% 1:3, na.rm = TRUE)/denom_cnt












