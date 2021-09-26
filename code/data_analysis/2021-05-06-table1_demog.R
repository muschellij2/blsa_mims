#' This script generates "Table 1" for the paper. 
#' 
#' Input: 
#' - /data_processed/2021-05-06-measures_masterfile_winsorized.rds
#' 
#' Notes: 
#' - This Table 1 is based on Table 1 from 
#'   https://academic.oup.com/biomedgerontology/article/73/5/630/4168567?login=true

rm(list = ls())
library(tidyverse)

# ------------------------------------------------------------------------------
# read data 

# measures_masterfile -- file with minute-level PA measures 
measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
dim(measures_masterfile)
# [1]  5566920      12
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

# interview -- visit-specific participant's: employment 
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

denom_cnt <- length(unique(measures_masterfile$subj_id))
denom_cnt

range(as.Date(measures_masterfile$HEADER_TIME_STAMP))
# [1] "2015-07-18" "2019-01-17"


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
# accelerometry wear

mm_agg <- 
  measures_masterfile %>% 
  mutate(HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  group_by(subj_id) %>%
  summarise(
    validdays_cnt = n_distinct(HEADER_TIME_STAMP_date),
    wearflag_sum         = sum(wear_flag),
    validflag_sum        = sum(valid_flag),
    wearandvalidflag_sum = sum(wear_and_valid_flag)
    ) %>%
  mutate(
    wearminutes_perday  = wearflag_sum / validdays_cnt,
    validminutes_perday = validflag_sum / validdays_cnt,
    wearandvalidminutes_perday = wearandvalidflag_sum / validdays_cnt
  ) %>%
  as.data.frame()

# accelerometry wear: valid days
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Accelerometry wear: valid days"
table1_val1[i] <- mean(mm_agg$validdays_cnt)
table1_val2[i] <- sd(mm_agg$validdays_cnt)
table1_val3[i] <- median(mm_agg$validdays_cnt)
table1_val4[i] <- min(mm_agg$validdays_cnt)
table1_val5[i] <- max(mm_agg$validdays_cnt)

# accelerometry wear: wear minutes / valid days 
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Accelerometry wear: wear mins/valid days"
table1_val1[i] <- mean(mm_agg$wearminutes_perday)
table1_val2[i] <- sd(mm_agg$wearminutes_perday)
table1_val3[i] <- median(mm_agg$wearminutes_perday)
table1_val4[i] <- min(mm_agg$wearminutes_perday)
table1_val5[i] <- max(mm_agg$wearminutes_perday)

# accelerometry wear: non-flagged minutes / valid days 
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Accelerometry wear: non-flagged mins/valid days"
table1_val1[i] <- mean(mm_agg$validminutes_perday)
table1_val2[i] <- sd(mm_agg$validminutes_perday)
table1_val3[i] <- median(mm_agg$validminutes_perday)
table1_val4[i] <- min(mm_agg$validminutes_perday)
table1_val5[i] <- max(mm_agg$validminutes_perday)

# accelerometry wear: valid minutes /  valid days 
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Accelerometry wear: valid mins/valid days"
table1_val1[i] <- mean(mm_agg$wearandvalidminutes_perday)
table1_val2[i] <- sd(mm_agg$wearandvalidminutes_perday)
table1_val3[i] <- median(mm_agg$wearandvalidminutes_perday)
table1_val4[i] <- min(mm_agg$wearandvalidminutes_perday)
table1_val5[i] <- max(mm_agg$wearandvalidminutes_perday)


# -----------------------------------------------------------------------------
# sex

# check agreement between sex and gender
masterdemog %>%
  select(gender, sex) %>%
  distinct()
#   gender sex
# 1      M   1
# 2      F   0

# check uniqueness of gender & sex variables per person
tbl_test <- masterdemog %>%
  select(idno, gender, sex) %>%
  distinct() %>%
  group_by(idno) %>%
  summarise(cnt = n())
table(tbl_test$cnt)

masterdemog_SUB_sex <- measures_masterfile %>%
  select(subj_id) %>% 
  distinct() %>%
  left_join(masterdemog, by = c("subj_id" = "idno")) %>%
  select(subj_id, sex) 

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Sex: Female"
table1_val1[i] <- sum(masterdemog_SUB_sex$sex == 0, na.rm = TRUE)
table1_val2[i] <- sum(masterdemog_SUB_sex$sex == 0, na.rm = TRUE)/denom_cnt

i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Sex: Male"
table1_val1[i] <- sum(masterdemog_SUB_sex$sex == 1, na.rm = TRUE)
table1_val2[i] <- sum(masterdemog_SUB_sex$sex == 1, na.rm = TRUE)/denom_cnt


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
table1_var[i]  <- "Race: Other"
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

# hypertensive 
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


## -----------------------------------------------------------------------------

# function to convert coding for answers for medical history (MEDH) questions 
# from "mastervisit" file into cases (1) or non-cases (0)
# 1=no, 2=yes, 3=no to yes (incident cases), and 4=yes to no (changed their mind or forgot!)
is_case <- function(val){
  if (val %in% c(2,3)){
    return(1)
  } else {
    return(0)
  }
}

# heart attack, CHF, angina (ischemic chest pain), vascular procedure, Peripheral artery disease
# (MI/CHF/angina/vascular procedure/PAD)
MDH_var_tmp <- c('SMDHX3', 'SMDHX4', 'SMDHX5', 'SMDHX18', 'SMDHX25') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "MI/CHF/angina/vascular procedure/PAD"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# hypertensive 
MDH_var_tmp <- c('SMDHX15') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Hypertension"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# High blood cholesterol 
MDH_var_tmp <- c('SMDHX17') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "High blood cholesterol"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# Stroke, TIA
MDH_var_tmp <- c('SMDHX12', 'SMDHX13') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Stroke/TIA"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# bronchitis, asthma (Pulmonary disease)
MDH_var_tmp <- c('SMDHX6', 'SMDHX7') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Pulmonary disease"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# Diabetes
MDH_var_tmp <- c('SMDHX16') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Diabetes"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# Cancer
MDH_var_tmp <- c('SMDHX19') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Cancer"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt

# Osteoarthritis
MDH_var_tmp <- c('SMDHX20') 
dat_tmp <- 
  mastervisit_SUB %>% 
  select(all_of(MDH_var_tmp)) %>%
  rowwise() %>%
  mutate_all(~ is_case(.))  %>%
  mutate(max_c_across = max(c_across(everything())))
i <- sum(!is.na(table1_var)) + 1
table1_var[i]  <- "Osteoarthritis"
table1_val1[i] <- sum(dat_tmp$max_c_across)
table1_val2[i] <- sum(dat_tmp$max_c_across)/denom_cnt




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# construct final table

# combine vectors of data into data frame 
table1_df <- data.frame(
  var_name = table1_var,
  var_val1 = table1_val1,
  var_val2 = table1_val2,
  var_val3 = table1_val3,
  var_val4 = table1_val4,
  var_val5 = table1_val5
) %>%
  filter(!is.na(var_name))

# subset of two table parts which will be summarized differently 
table1_df_A_nrow <- sum(!is.na(table1_df$var_val3))
table1_df_A <- table1_df[1 : table1_df_A_nrow, ]
table1_df_B <- table1_df[(table1_df_A_nrow + 1) : nrow(table1_df), ]

table1_df_A_form <- 
  table1_df_A %>%
  mutate(
    val_mean_f   = sprintf("%.1f", var_val1),
    val_sd_f     = sprintf("%.1f", var_val2),
    val_median_f = sprintf("%.1f", var_val3),
    val_min_f    = sprintf("%.1f", var_val4),
    val_max_f    = sprintf("%.1f", var_val5)
    # val_max_f    = sprintf("% 06.3f", var_val5)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(var_name, 
         col1 = val_mean_sd, 
         col2 = val_median_min_max)

table1_df_B_form <- 
  table1_df_B %>%
  mutate(
    val_cnt_f   = sprintf("%.0f", var_val1),
    val_pct_f     = sprintf("%.1f", var_val2 * 100)
    # val_max_f    = sprintf("% 06.3f", var_val5)
  ) %>%
  mutate(
    val_cnt_pct = paste0(val_cnt_f, " (", val_pct_f, ")")
  ) %>%
  select(var_name, 
         col1 = val_cnt_pct)
table1_df_B_form$col2 = NA


View(table1_df_A_form)
View(table1_df_B_form)

# to Latex 
stargazer::stargazer(table1_df_A_form, summary = FALSE)
stargazer::stargazer(table1_df_B_form, summary = FALSE)




