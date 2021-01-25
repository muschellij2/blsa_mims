#' This script generates "meta" data for covaroates data files in BLSA. 
#' 
#' Some helpful commands in the comment to upload/download data sets to/from 
#' JHPCE are below in the comment.

# get /dcl01/smart/data/activity/blsa_mims/covariates/mastervisit.rdata /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/mastervisit.rdata
# get /dcl01/smart/data/activity/blsa_mims/covariates/mastervisit.sas7bdat /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/mastervisit.sas7bdat

# get /dcl01/smart/data/activity/blsa_mims/covariates/masterdemog_no\ dob.rdata  /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/masterdemog_no\ dob.rdata 

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-mastervisit.rdata /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-mastervisit.rdata
# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-mastervisit.sas7bdat /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-mastervisit.sas7bdat

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-masterdemog.rdata /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-masterdemog.rdata
# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-masterdemog.dta /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-masterdemog.dta

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.rdata /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.rdata
# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.dta /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.dta

# get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-01-18-measures_masterfile.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-01-18-measures_masterfile.rds


rm(list = ls())
library(tidyverse)
library(haven)
library(utils)


# mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.sas7bdat")
mastervisit <- sas7bdat::read.sas7bdat(mastervisit_fpath)
# pull column info attribute 
mastervisit_colinfo <- attr(mastervisit, "column.info")
names(mastervisit_colinfo[[1]])
# define mastervisit_meta data 
mastervisit_meta <- data.frame(
  var_name = sapply(mastervisit_colinfo, function(ll) ll$name),
  var_label = sapply(mastervisit_colinfo, function(ll) ll$label)
)
# save mastervisit_meta data to file
utils::capture.output(
  mastervisit_meta,
  file = paste0(here::here(), "/covariates/mastervisit_meta.txt")
)
rm(mastervisit_fpath, mastervisit, mastervisit_colinfo, mastervisit_meta)


# interview
# read interview 
interview_fpath <- paste0(here::here(), "/covariates/2021-01-19-blsa_interview_mdhx_teleform.dta")
interview <- haven::read_dta(interview_fpath)
# list attributes 
attributes(interview[[1]])
# produce the meta info data 
interview_meta <- data.frame(
  var_name = names(interview),
  var_label = unlist(lapply(interview, function(ll){val <- attr(ll, "label"); ifelse(is.null(val), NA, val)}))
)
rownames(interview_meta) <- NULL
# save the meta info data 
utils::capture.output(
  interview_meta,
  file = paste0(here::here(), "/covariates/interview_meta.txt")
)
rm(interview_fpath, interview, interview_meta)


# masterdemog 
masterdemog_fpath <- paste0(here::here(), "/covariates/2021-01-19-masterdemog.dta")
masterdemog <- haven::read_dta(masterdemog_fpath)
dim(masterdemog)
names(masterdemog)
masterdemog_meta <- data.frame(
  var_name = names(masterdemog)
)
# check the range of values 
range(masterdemog$LastVisit_Date)
# [1] "1958-02-06" "2019-12-18"
# save the meta info data 
utils::capture.output(
  masterdemog_meta,
  file = paste0(here::here(), "/covariates/masterdemog_meta.txt")
)
