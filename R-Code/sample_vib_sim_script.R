# Script for the assessment of sampling vibration on simulated data

library(survival)
library(mvtnorm)
library(GenOrd)

load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))

mainTab$current_past_smoking[which(mainTab$current_past_smoking == 2)] <- 1
mainTab$current_past_smoking[which(mainTab$current_past_smoking == 3)] <- 2

source('sample_vib_sim_fun.R')
source('data_gen_functions.R')


adj_var_full <- c('physical_activity', 'RIDRETH1', 'RIDAGEYR', 'male', 
                  'any_cancer_self_report', 'current_past_smoking', 
                  'bmi', 'any_ht', 'any_diabetes','LBXTC', 'drink_five_per_day', 
                  'education', 'SES_LEVEL', 'any_family_cad', 'any_cad')

adj_type_full <- c("Ord", "Num", "Num", "Bin", "Bin", "Bin", "Ord", "Bin", "Bin", "Num",
                   "Bin", "Ord", "Ord", "Bin", "Bin")


adj_var_sub <- c("male", "RIDAGEYR")


fac_ind_full <- c(T,T,F,T,T,T,T,T,T,F,T,T,T,T,T)
fac_ind_sub <- c(T,F)


mainTab$physical_activity <- mainTab$physical_activity + 1
mainTab$male <- mainTab$male + 1
mainTab$any_cancer_self_report <- mainTab$any_cancer_self_report + 1
mainTab$any_ht <- mainTab$any_ht + 1
mainTab$any_diabetes <- mainTab$any_diabetes + 1
mainTab$drink_five_per_day <- mainTab$drink_five_per_day + 1
mainTab$any_cad <- mainTab$any_cad + 1
mainTab$any_family_cad <- mainTab$any_family_cad + 1


set.seed(1234)
seeds <- sample(10000:10000000, size = 30)


########################
###   Main results   ###
########################


### any_diabetes


set.seed(seeds[1])
data_any_diabetes <- data_for_mv(mainTab, voi = "any_diabetes", voi_type = "Bin", adj_type = adj_type_full,
                                 adj_var = adj_var_full)

any_diabetes_05 <- vib_sim_summary(data_new = data_any_diabetes, voi = "any_diabetes", B = 1000, replace = F,
                                   exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(any_diabetes_05, file = "Results/sample_sim/any_diabetes.RData")



### any_cad

set.seed(seeds[2])
data_any_cad <- data_for_mv(mainTab, voi = "any_cad", voi_type = "Bin", adj_type = adj_type_full,
                            adj_var = adj_var_full)

any_cad_05 <- vib_sim_summary(data_new = data_any_cad, voi = "any_cad", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(any_cad_05, file = "Results/sample_sim/any_cad.RData")


### BMXTHICR

set.seed(seeds[3])
data_BMXTHICR <- data_for_mv(mainTab, voi = "BMXTHICR", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

BMXTHICR_05 <- vib_sim_summary(data_new = data_BMXTHICR, voi = "BMXTHICR", B = 1000, replace = F,
                               exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(BMXTHICR_05, file = "Results/sample_sim/BMXTHICR.RData")


### LBDHDL

set.seed(seeds[4])
data_LBDHDL <- data_for_mv(mainTab, voi = "LBDHDL", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

LBDHDL_05 <- vib_sim_summary(data_new = data_LBDHDL, voi = "LBDHDL", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBDHDL_05, file = "Results/sample_sim/LBDHDL.RData")


################################
###   Supplemental results   ###
################################


### any_ht

set.seed(seeds[5])
data_any_ht <- data_for_mv(mainTab, voi = "any_ht", voi_type = "Bin", adj_type = adj_type_full,
                           adj_var = adj_var_full)

any_ht_05 <- vib_sim_summary(data_new = data_any_ht, voi = "any_ht", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(any_ht_05, file = "Results/sample_sim/any_ht.RData")


### any_cancer_self_report


set.seed(seeds[6])
data_any_cancer_self_report <- data_for_mv(mainTab, voi = "any_cancer_self_report", voi_type = "Bin", adj_type = adj_type_full,
                                           adj_var = adj_var_full)

any_cancer_self_report_05 <- vib_sim_summary(data_new = data_any_cancer_self_report,
                                             voi = "any_cancer_self_report", B = 1000, replace = F,
                                             split = 0.5,exactPvalue = T, family = "cox", outcome = "Y",
                                             adj_var = adj_var_full)

save(any_cancer_self_report_05, file = "Results/sample_sim/any_cancer_self_report.RData")


### any_family_cad

set.seed(seeds[7])
data_any_family_cad <- data_for_mv(mainTab, voi = "any_family_cad", voi_type = "Bin", adj_type = adj_type_full,
                                   adj_var = adj_var_full)

any_family_cad_05 <- vib_sim_summary(data_new = data_any_family_cad, voi = "any_family_cad", B = 1000, replace = F,
                                     exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(any_family_cad_05, file = "Results/sample_sim/any_family_cad.RData")



### LBDNENO

set.seed(seeds[8])
data_LBDNENO <- data_for_mv(mainTab, voi = "LBDNENO", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

LBDNENO_05 <- vib_sim_summary(data_new = data_LBDNENO, voi = "LBDNENO", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBDNENO_05, file = "Results/sample_sim/LBDNENO.RData")


### BMXCALF

set.seed(seeds[9])
data_BMXCALF <- data_for_mv(mainTab, voi = "BMXCALF", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

BMXCALF_05 <- vib_sim_summary(data_new = data_BMXCALF, voi = "BMXCALF", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(BMXCALF_05, file = "Results/sample_sim/BMXCALF.RData")


### BMXHT

set.seed(seeds[10])
data_BMXHT <- data_for_mv(mainTab, voi = "BMXHT", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

BMXHT_05 <- vib_sim_summary(data_new = data_BMXHT, voi = "BMXHT", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(BMXHT_05, file = "Results/sample_sim/BMXHT.RData")


### BMXWAIST


set.seed(seeds[11])
data_BMXWAIST <- data_for_mv(mainTab, voi = "BMXWAIST", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

BMXWAIST_05 <- vib_sim_summary(data_new = data_BMXWAIST, voi = "BMXWAIST", B = 1000, replace = F,
                               exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(BMXWAIST_05, file = "Results/sample_sim/BMXWAIST.RData")



### BMXWT

set.seed(seeds[12])
data_BMXWT <- data_for_mv(mainTab, voi = "BMXWT", voi_type = "Num", adj_type = adj_type_full,
                          adj_var = adj_var_full)

BMXWT_05 <- vib_sim_summary(data_new = data_BMXWT, voi = "BMXWT", B = 1000, replace = F,
                            exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(BMXWT_05, file = "Results/sample_sim/BMXWT.RData")



## BPXPLS

set.seed(seeds[13])
data_BPXPLS <- data_for_mv(mainTab, voi = "BPXPLS", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

BPXPLS_05 <- vib_sim_summary(data_new = data_BPXPLS, voi = "BPXPLS", B = 1000, replace = F,
                               exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(BPXPLS_05, file = "Results/sample_sim/BPXPLS.RData")


### DSDCOUNT

set.seed(seeds[14])
data_DSDCOUNT <- data_for_mv(mainTab, voi = "DSDCOUNT", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

DSDCOUNT_05 <- vib_sim_summary(data_new = data_DSDCOUNT, voi = "DSDCOUNT", B = 1000, replace = F,
                                exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(DSDCOUNT_05, file = "Results/sample_sim/DSDCOUNT.RData")


### LBXHA

set.seed(seeds[15])
data_LBXHA <- data_for_mv(mainTab, voi = "LBXHA", voi_type = "Bin", adj_type = adj_type_full,
                          adj_var = adj_var_full)

LBXHA_05 <- vib_sim_summary(data_new = data_LBXHA, voi = "LBXHA", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXHA_05, file = "Results/sample_sim/LBXHA.RData")


### LBXHBC

set.seed(seeds[16])
data_LBXHBC <- data_for_mv(mainTab, voi = "LBXHBC", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

LBXHBC_05 <- vib_sim_summary(data_new = data_LBXHBC, voi = "LBXHBC", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXHBC_05, file = "Results/sample_sim/LBXHBC.RData")


### LBXHBS

set.seed(seeds[17])
data_LBXHBS <- data_for_mv(mainTab, voi = "LBXHBS", voi_type = "Bin", adj_type = adj_type_full,
                           adj_var = adj_var_full)

LBXHBS_05 <- vib_sim_summary(data_new = data_LBXHBS, voi = "LBXHBS", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXHBS_05, file = "Results/sample_sim/LBXHBS.RData")


### LBXLYPCT

set.seed(seeds[18])
data_LBXLYPCT <- data_for_mv(mainTab, voi = "LBXLYPCT", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

LBXLYPCT_05 <- vib_sim_summary(data_new = data_LBXLYPCT, voi = "LBXLYPCT", B = 1000, replace = F,
                               exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXLYPCT_05, file = "Results/sample_sim/LBXLYPCT.RData")


### LBXMPSI

set.seed(seeds[19])
data_LBXMPSI <- data_for_mv(mainTab, voi = "LBXMPSI", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

LBXMPSI_05 <- vib_sim_summary(data_new = data_LBXMPSI, voi = "LBXMPSI", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXMPSI_05, file = "Results/sample_sim/LBXMPSI.RData")


### LBXSCA

set.seed(seeds[20])
data_LBXSCA <- data_for_mv(mainTab, voi = "LBXSCA", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

LBXSCA_05 <- vib_sim_summary(data_new = data_LBXSCA, voi = "LBXSCA", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXSCA_05, file = "Results/sample_sim/LBXSCA.RData")


### LBXSNASI

set.seed(seeds[21])
data_LBXSNASI <- data_for_mv(mainTab, voi = "LBXSNASI", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

LBXSNASI_05 <- vib_sim_summary(data_new = data_LBXSNASI, voi = "LBXSNASI", B = 1000, replace = F,
                               exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXSNASI_05, file = "Results/sample_sim/LBXSNASI.RData")


## LBXSOSSI

set.seed(seeds[22])
data_LBXSOSSI <- data_for_mv(mainTab, voi = "LBXSOSSI", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

LBXSOSSI_05 <- vib_sim_summary(data_new = data_LBXSOSSI, voi = "LBXSOSSI", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXSOSSI_05, file = "Results/sample_sim/LBXSOSSI.RData")


### LBXSPH

set.seed(seeds[23])
data_LBXSPH <- data_for_mv(mainTab, voi = "LBXSPH", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

LBXSPH_05 <- vib_sim_summary(data_new = data_LBXSPH, voi = "LBXSPH", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(LBXSPH_05, file = "Results/sample_sim/LBXSPH.RData")


### male

set.seed(seeds[24])
data_male <- data_for_mv(mainTab, voi = "male", voi_type = "Bin", adj_type = adj_type_full,
                         adj_var = adj_var_full)

male_05 <- vib_sim_summary(data_new = data_male, voi = "male", B = 1000, replace = F,
                           exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(male_05, file = "Results/sample_sim/male.RData")



### pest_control_home_last_month

set.seed(seeds[25])
data_pest_control_home_last_month <- data_for_mv(mainTab, voi = "pest_control_home_last_month", voi_type = "Bin", adj_type = adj_type_full,
                                                 adj_var = adj_var_full)

pest_control_home_last_month_05 <- vib_sim_summary(data_new = data_pest_control_home_last_month, voi = "pest_control_home_last_month", B = 1000, replace = F,
                                                   exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(pest_control_home_last_month_05, file = "Results/sample_sim/pest_control_home_last_month.RData")



###   pneu

set.seed(seeds[26])
data_pneu <- data_for_mv(mainTab, voi = "pneu", voi_type = "Bin", adj_type = adj_type_full,
                         adj_var = adj_var_full)

pneu_05 <- vib_sim_summary(data_new = data_pneu, voi = "pneu", B = 1000, replace = F,
                              exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(pneu_05, file = "Results/sample_sim/pneu.RData")


### private_water_source

set.seed(seeds[27])
data_private_water_source <- data_for_mv(mainTab, voi = "private_water_source", voi_type = "Bin", adj_type = adj_type_full,
                                         adj_var = adj_var_full)

private_water_source_05 <- vib_sim_summary(data_new = data_private_water_source, voi = "private_water_source", B = 1000, replace = F,
                                           exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(private_water_source_05, file = "Results/sample_sim/private_water_source.RData")


### use_water_treatment

set.seed(seeds[28])
data_use_water_treatment <- data_for_mv(mainTab, voi = "use_water_treatment", voi_type = "Bin", adj_type = adj_type_full,
                                        adj_var = adj_var_full)

use_water_treatment_05 <- vib_sim_summary(data_new = data_use_water_treatment, voi = "use_water_treatment", B = 1000, replace = F,
                                   exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(use_water_treatment_05, file = "Results/sample_sim/use_water_treatment.RData")


### RIDAGEYR

set.seed(seeds[29])
data_RIDAGEYR <- data_for_mv(mainTab, voi = "RIDAGEYR", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

RIDAGEYR_05 <- vib_sim_summary(data_new = data_RIDAGEYR, voi = "RIDAGEYR", B = 1000, replace = F,
                               exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(RIDAGEYR_05, file = "Results/sample_sim/RIDAGEYR.RData")




### SMD410

set.seed(seeds[30])
data_SMD410 <- data_for_mv(mainTab, voi = "SMD410", voi_type = "Bin", adj_type = adj_type_full,
                           adj_var = adj_var_full)

SMD410_05 <- vib_sim_summary(data_new = data_SMD410, voi = "SMD410", B = 1000, replace = F,
                             exactPvalue = T, family = "cox", outcome = "Y", adj_var = adj_var_full)

save(SMD410_05, file = "Results/sample_sim/SMD410.RData")



