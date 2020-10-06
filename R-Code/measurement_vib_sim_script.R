# Script for the assessment of measurement vibration on simulated data

source("data_gen_functions.R")
source("measurement_vib_ordinal.R")
source("measurement_vib_sim_fun.R")

load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))

mainTab$current_past_smoking[which(mainTab$current_past_smoking == 2)] <- 1
mainTab$current_past_smoking[which(mainTab$current_past_smoking == 3)] <- 2

library(survival)
library(mvtnorm)
library(GenOrd)
library(ggplot2)

variable <- "BPXPLS"
adj_var_full <- c('physical_activity', 'RIDRETH1', 'RIDAGEYR', 'male', 
                  'any_cancer_self_report', 'current_past_smoking', 
                  'bmi', 'any_ht', 'any_diabetes','LBXTC', 'drink_five_per_day', 
                  'education', 'SES_LEVEL', 'any_family_cad', 'any_cad')

adj_type_full <- c("Ord", "Num", "Num", "Bin", "Bin", "Bin", "Ord", "Bin", "Bin", "Num",
              "Bin", "Ord", "Ord", "Bin", "Bin")

### For simulation purposes:

mainTab$physical_activity <- mainTab$physical_activity + 1
mainTab$male <- mainTab$male + 1
mainTab$any_cancer_self_report <- mainTab$any_cancer_self_report + 1
mainTab$any_ht <- mainTab$any_ht + 1
mainTab$any_diabetes <- mainTab$any_diabetes + 1
mainTab$drink_five_per_day <- mainTab$drink_five_per_day + 1
mainTab$any_cad <- mainTab$any_cad + 1
mainTab$any_family_cad <- mainTab$any_family_cad + 1


### Search for parameters of the normal distribution (ordinal variables) through a grid search

# set.seed(1234)
# bmi_ord <- me_ordinal_gs(data = mainTab, var_name = "bmi")
# physical_activity_ord <- me_ordinal_gs(data = mainTab, var_name = "physical_activity")
# education_ord <- me_ordinal_gs(data = mainTab, var_name = "education")
# SES_LEVEL_ord <- me_ordinal_gs(data = mainTab, var_name = "SES_LEVEL")
# 
# ord_var = list(bmi_ord = bmi_ord,
#                physical_activity_ord = physical_activity_ord,
#                education_ord = education_ord,
#                SES_LEVEL_ord = SES_LEVEL_ord)
# 
# save(ord_var, file = "Results/me_sim_ord_var.RData")

load("Results/me_sim_ord_var.RData")

me_mix <- list(correlation = c(0.73, 0.9),
               sensitivity = c(0.56, 0.85),
               specificity = c(0.73, 0.98))

# correlation: 0.73, 0.9
# sensitivity: 0.56, 0.85
# specificity: 0.73, 0.98

set.seed(1234)
seeds <- sample(10000:10000000, size = 30)


########################
###   Main results   ###
########################

###   any_diabetes

set.seed(seeds[1])
data_any_diabetes <- data_for_mv(mainTab, voi = "any_diabetes", voi_type = "Bin", adj_type = adj_type_full,
                                 adj_var = adj_var_full)

data_ord_any_diabetes <- ord_data_for_mv(data = data_any_diabetes, ord_var = ord_var)

any_diabetes_me <- me_vib_sim(data = data_any_diabetes, ord_data = data_ord_any_diabetes, 
                              voi = "any_diabetes", voi_type = "Bin", adj_var = adj_var_full,
                              adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(any_diabetes_me, file = "Results/measurement_sim/any_diabetes_me.RData")


###   any_cad

set.seed(seeds[2])
data_any_cad <- data_for_mv(mainTab, voi = "any_cad", voi_type = "Bin", adj_type = adj_type_full,
                            adj_var = adj_var_full)

data_ord_any_cad <- ord_data_for_mv(data = data_any_cad, ord_var = ord_var)

any_cad_me <- me_vib_sim(data = data_any_cad, ord_data = data_ord_any_cad, voi = "any_cad", voi_type = "Bin", adj_var = adj_var_full,
                         adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(any_cad_me, file = "Results/measurement_sim/any_cad_me.RData")


###   BMXTHICR

set.seed(seeds[3])
data_BMXTHICR <- data_for_mv(mainTab, voi = "BMXTHICR", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

data_ord_BMXTHICR <- ord_data_for_mv(data = data_BMXTHICR, ord_var = ord_var)


BMXTHICR_me <- me_vib_sim(data = data_BMXTHICR, voi = "BMXTHICR", ord_data = data_ord_BMXTHICR, voi_type = "Num", adj_var = adj_var_full,
                          adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(BMXTHICR_me, file = "Results/measurement_sim/BMXTHICR_me.RData")


##   LBDHDL

set.seed(seeds[4])
data_LBDHDL <- data_for_mv(mainTab, voi = "LBDHDL", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

data_ord_LBDHDL <- ord_data_for_mv(data = data_LBDHDL, ord_var = ord_var)

        
LBDHDL_me <- me_vib_sim(data = data_LBDHDL, ord_data = data_ord_LBDHDL, voi = "LBDHDL", voi_type = "Num", adj_var = adj_var_full,
                        adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(LBDHDL_me, file = "Results/measurement_sim/LBDHDL_me.RData")


################################
###   Supplemental results   ###
################################


###   any_ht

set.seed(seeds[5])
data_any_ht <- data_for_mv(mainTab, voi = "any_ht", voi_type = "Bin", adj_type = adj_type_full,
                           adj_var = adj_var_full)

data_ord_any_ht <- ord_data_for_mv(data = data_any_ht, ord_var = ord_var)

any_ht_me <- me_vib_sim(data = data_any_ht, ord_data = data_ord_any_ht, voi = "any_ht", voi_type = "Bin", adj_var = adj_var_full,
                        adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(any_ht_me, file = "Results/measurement_sim/any_ht_me.RData")


###   any_cancer_self_report

set.seed(seeds[6])
data_any_cancer_self_report <- data_for_mv(mainTab, voi = "any_cancer_self_report", voi_type = "Bin", adj_type = adj_type_full,
                                           adj_var = adj_var_full)

data_ord_any_cancer_self_report <- ord_data_for_mv(data = data_any_cancer_self_report, ord_var = ord_var)

any_cancer_self_report_me <- me_vib_sim(data = data_any_cancer_self_report, ord_data = data_ord_any_cancer_self_report, voi = "any_cancer_self_report", voi_type = "Bin", adj_var = adj_var_full,
                                        adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(any_cancer_self_report_me, file = "Results/measurement_sim/any_cancer_self_report_me.RData")


###   any_family_cad

set.seed(seeds[7])
data_any_family_cad <- data_for_mv(mainTab, voi = "any_family_cad", voi_type = "Bin", adj_type = adj_type_full, 
                                   adj_var = adj_var_full)

data_ord_any_family_cad <- ord_data_for_mv(data = data_any_family_cad, ord_var = ord_var)

any_family_cad_me <- me_vib_sim(data = data_any_family_cad, ord_data = data_ord_any_family_cad, voi = "any_family_cad", voi_type = "Bin", adj_var = adj_var_full,
                                adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(any_family_cad_me, file = "Results/measurement_sim/any_family_cad_me.RData")


##   LBDNENO

set.seed(seeds[8])
data_LBDNENO <- data_for_mv(mainTab, voi = "LBDNENO", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

data_ord_LBDNENO <- ord_data_for_mv(data = data_LBDNENO, ord_var = ord_var)

LBDNENO_me <- me_vib_sim(data = data_LBDNENO, ord_data = data_ord_LBDNENO, voi = "LBDNENO",
                              voi_type = "Num", adj_var = adj_var_full, adj_type = adj_type_full,
                              me_range = me_mix, B = 1000)


save(LBDNENO_me, file = "Results/measurement_sim/LBDNENO_me.RData")


###   BMXCALF

set.seed(seeds[9])
data_BMXCALF <- data_for_mv(mainTab, voi = "BMXCALF", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

data_ord_BMXCALF <- ord_data_for_mv(data = data_BMXCALF, ord_var = ord_var)


BMXCALF_me <- me_vib_sim(data = data_BMXCALF, ord_data = data_ord_BMXCALF, voi = "BMXCALF", voi_type = "Num", adj_var = adj_var_full,
                         adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(BMXCALF_me, file = "Results/measurement_sim/BMXCALF_me.RData")


###   BMXHT

set.seed(seeds[10])
data_BMXHT <- data_for_mv(mainTab, voi = "BMXHT", voi_type = "Num", adj_type = adj_type_full,
                          adj_var = adj_var_full)

data_ord_BMXHT <- ord_data_for_mv(data = data_BMXHT, ord_var = ord_var)

BMXHT_me <- me_vib_sim(data = data_BMXHT, ord_data = data_ord_BMXHT, voi = "BMXHT", voi_type = "Num", adj_var = adj_var_full,
                       adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(BMXHT_me, file = "Results/measurement_sim/BMXHT_me.RData")

##   BMXWAIST

set.seed(seeds[11])
data_BMXWAIST <- data_for_mv(mainTab, voi = "BMXWAIST", voi_type = "Num", adj_type = adj_type_full, 
                             adj_var = adj_var_full)

data_ord_BMXWAIST <- ord_data_for_mv(data = data_BMXWAIST, ord_var = ord_var)

BMXWAIST_me <- me_vib_sim(data = data_BMXWAIST, ord_data = data_ord_BMXWAIST, voi = "BMXWAIST", voi_type = "Num", adj_var = adj_var_full,
                          adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(BMXWAIST_me, file = "Results/measurement_sim/BMXWAIST_me.RData")


##   BMXWT

set.seed(seeds[12])
data_BMXWT <- data_for_mv(mainTab, voi = "BMXWT", voi_type = "Num", adj_type = adj_type_full, 
                          adj_var = adj_var_full)

data_ord_BMXWT <- ord_data_for_mv(data = data_BMXWT, ord_var = ord_var)

BMXWT_me <- me_vib_sim(data = data_BMXWT, ord_data = data_ord_BMXWT, voi = "BMXWT", voi_type = "Num", adj_var = adj_var_full,
                       adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(BMXWT_me, file = "Results/measurement_sim/BMXWT_me.RData")


###   BPXPLS

set.seed(seeds[13])
data_BPXPLS <- data_for_mv(mainTab, voi = "BPXPLS", voi_type = "Num", adj_type = adj_type_full,
                            adj_var = adj_var_full)

data_ord_BPXPLS <- ord_data_for_mv(data = data_BPXPLS, ord_var = ord_var)

BPXPLS_me <- me_vib_sim(data = data_BPXPLS, ord_data = data_ord_BPXPLS, voi = "BPXPLS", voi_type = "Num", adj_var = adj_var_full,
                             adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(BPXPLS_me, file = "Results/measurement_sim/BPXPLS_me.RData")


##   DSDCOUNT

set.seed(seeds[14])
data_DSDCOUNT <- data_for_mv(mainTab, voi = "DSDCOUNT", voi_type = "Num", adj_type = adj_type_full, 
                             adj_var = adj_var_full)

data_ord_DSDCOUNT <- ord_data_for_mv(data = data_DSDCOUNT, ord_var = ord_var)

DSDCOUNT_me <- me_vib_sim(data = data_DSDCOUNT, ord_data = data_ord_DSDCOUNT, voi = "DSDCOUNT", voi_type = "Num", adj_var = adj_var_full,
                          adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(DSDCOUNT_me, file = "Results/measurement_sim/DSDCOUNT_me.RData")



###   LBXHA

set.seed(seeds[15])
data_LBXHA <- data_for_mv(mainTab, voi = "LBXHA", voi_type = "Bin", adj_type = adj_type_full,
                          adj_var = adj_var_full)

data_ord_LBXHA <- ord_data_for_mv(data = data_LBXHA, ord_var = ord_var)

LBXHA_me <- me_vib_sim(data = data_LBXHA, ord_data = data_ord_LBXHA, voi = "LBXHA", voi_type = "Bin", adj_var = adj_var_full,
                       adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(LBXHA_me, file = "Results/measurement_sim/LBXHA_me.RData")


###   LBXHBC

set.seed(seeds[16])
data_LBXHBC <- data_for_mv(mainTab, voi = "LBXHBC", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

data_ord_LBXHBC <- ord_data_for_mv(data = data_LBXHBC, ord_var = ord_var)

LBXHBC_me <- me_vib_sim(data = data_LBXHBC, ord_data = data_ord_LBXHBC, voi = "LBXHBC", voi_type = "Num", adj_var = adj_var_full,
                        adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(LBXHBC_me, file = "Results/measurement_sim/LBXHBC.RData")


###   LBXHBS

set.seed(seeds[17])
data_LBXHBS <- data_for_mv(mainTab, voi = "LBXHBS", voi_type = "Bin", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

data_ord_LBXHBS <- ord_data_for_mv(data = data_LBXHBS, ord_var = ord_var)

LBXHBS_me <- me_vib_sim(data = data_LBXHBS, ord_data = data_ord_LBXHBS, voi = "LBXHBS", voi_type = "Bin", adj_var = adj_var_full,
                        adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(LBXHBS_me, file = "Results/measurement_sim/LBXHBS_me.RData")


##   LBXLYPCT

set.seed(seeds[18])
data_LBXLYPCT <- data_for_mv(mainTab, voi = "LBXLYPCT", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

data_ord_LBXLYPCT <- ord_data_for_mv(data = data_LBXLYPCT, ord_var = ord_var)

LBXLYPCT_me <- me_vib_sim(data = data_LBXLYPCT, ord_data = data_ord_LBXLYPCT, voi = "LBXLYPCT", voi_type = "Num", adj_var = adj_var_full,
                          adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(LBXLYPCT_me, file = "Results/measurement_sim/LBXLYPCT_me.RData")


##   LBXMPSI

set.seed(seeds[19])
data_LBXMPSI <- data_for_mv(mainTab, voi = "LBXMPSI", voi_type = "Num", adj_type = adj_type_full, 
                            adj_var = adj_var_full)

data_ord_LBXMPSI <- ord_data_for_mv(data = data_LBXMPSI, ord_var = ord_var)

LBXMPSI_me <- me_vib_sim(data = data_LBXMPSI, ord_data = data_ord_LBXMPSI, voi = "LBXMPSI", voi_type = "Num", adj_var = adj_var_full,
                         adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(LBXMPSI_me, file = "Results/measurement_sim/LBXMPSI_me.RData")


##   LBXSCA

set.seed(seeds[20])
data_LBXSCA <- data_for_mv(mainTab, voi = "LBXSCA", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

data_ord_LBXSCA <- ord_data_for_mv(data = data_LBXSCA, ord_var = ord_var)


LBXSCA_me <- me_vib_sim(data = data_LBXSCA, ord_data = data_ord_LBXSCA, voi = "LBXSCA", voi_type = "Num", adj_var = adj_var_full,
                        adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(LBXSCA_me, file = "Results/measurement_sim/LBXSCA_me.RData")


##   LBXSNASI

set.seed(seeds[21])
data_LBXSNASI <- data_for_mv(mainTab, voi = "LBXSNASI", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

data_ord_LBXSNASI <- ord_data_for_mv(data = data_LBXSNASI, ord_var = ord_var)


LBXSNASI_me <- me_vib_sim(data = data_LBXSNASI, ord_data = data_ord_LBXSNASI, voi = "LBXSNASI", voi_type = "Num", adj_var = adj_var_full,
                          adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(LBXSNASI_me, file = "Results/measurement_sim/LBXSNASI_me.RData")



##   LBXSOSSI

set.seed(seeds[22])
data_LBXSOSSI <- data_for_mv(mainTab, voi = "LBXSOSSI", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

data_ord_LBXSOSSI <- ord_data_for_mv(data = data_LBXSOSSI, ord_var = ord_var)


LBXSOSSI_me <- me_vib_sim(data = data_LBXSOSSI, ord_data = data_ord_LBXSOSSI, voi = "LBXSOSSI", voi_type = "Num", adj_var = adj_var_full,
                          adj_type = adj_type_full, me_range = me_mix, B = 1000)

save(LBXSOSSI_me, file = "Results/measurement_sim/LBXSOSSI_me.RData")


##   LBXSPH

set.seed(seeds[23])
data_LBXSPH <- data_for_mv(mainTab, voi = "LBXSPH", voi_type = "Num", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

data_ord_LBXSPH <- ord_data_for_mv(data = data_LBXSPH, ord_var = ord_var)


LBXSPH_me <- me_vib_sim(data = data_LBXSPH, ord_data = data_ord_LBXSPH, voi = "LBXSPH", voi_type = "Num", adj_var = adj_var_full,
                        adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(LBXSPH_me, file = "Results/measurement_sim/LBXSPH_me.RData")


###   male

set.seed(seeds[24])
data_male <- data_for_mv(mainTab, voi = "male", voi_type = "Bin", adj_type = adj_type_full, 
                         adj_var = adj_var_full)

data_ord_male <- ord_data_for_mv(data = data_male, ord_var = ord_var)


male_me <- me_vib_sim(data = data_male, ord_data = data_ord_male, voi = "male", voi_type = "Bin", adj_var = adj_var_full,
                      adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(male_me, file = "Results/measurement_sim/male_me.RData")


###   pest_control_home_last_month

set.seed(seeds[25])
data_pest_control_home_last_month <- data_for_mv(mainTab, voi = "pest_control_home_last_month", voi_type = "Bin", adj_type = adj_type_full, 
                                                 adj_var = adj_var_full)

data_ord_pest_control_home_last_month <- ord_data_for_mv(data = data_pest_control_home_last_month, ord_var = ord_var)


pest_control_home_last_month_me <- me_vib_sim(data = data_pest_control_home_last_month, ord_data = data_ord_pest_control_home_last_month, voi = "pest_control_home_last_month", voi_type = "Bin", adj_var = adj_var_full,
                                              adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(pest_control_home_last_month_me, file = "Results/measurement_sim/pest_control_home_last_month_me.RData")


###   pneu

set.seed(seeds[26])
data_pneu <- data_for_mv(mainTab, voi = "pneu", voi_type = "Bin", adj_type = adj_type_full,
                                 adj_var = adj_var_full)

data_ord_pneu <- ord_data_for_mv(data = data_pneu, ord_var = ord_var)

pneu_me <- me_vib_sim(data = data_pneu, ord_data = data_ord_pneu, voi = "pneu", voi_type = "Bin", adj_var = adj_var_full,
                                   adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(pneu_me, file = "Results/measurement_sim/pneu_me.RData")


###   private_water_source

set.seed(seeds[27])
data_private_water_source <- data_for_mv(mainTab, voi = "private_water_source", voi_type = "Bin", adj_type = adj_type_full, 
                                         adj_var = adj_var_full)

data_ord_private_water_source <- ord_data_for_mv(data = data_private_water_source, ord_var = ord_var)


private_water_source_me <- me_vib_sim(data = data_private_water_source, ord_data = data_ord_private_water_source, voi = "private_water_source", voi_type = "Bin", adj_var = adj_var_full,
                                      adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(private_water_source_me, file = "Results/measurement_sim/private_water_source_me.RData")


###   use_water_treatment

set.seed(seeds[28])
data_use_water_treatment <- data_for_mv(mainTab, voi = "use_water_treatment", voi_type = "Bin", adj_type = adj_type_full, 
                                        adj_var = adj_var_full)

data_ord_use_water_treatment <- ord_data_for_mv(data = data_use_water_treatment, ord_var = ord_var)


use_water_treatment_me <- me_vib_sim(data = data_use_water_treatment,ord_data = data_ord_use_water_treatment, voi = "use_water_treatment", voi_type = "Bin", adj_var = adj_var_full,
                                     adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(use_water_treatment_me, file = "Results/measurement_sim/use_water_treatment_me.RData")



###   RIDAGEYR

set.seed(seeds[29])
data_RIDAGEYR <- data_for_mv(mainTab, voi = "RIDAGEYR", voi_type = "Num", adj_type = adj_type_full,
                                 adj_var = adj_var_full)

data_ord_RIDAGEYR <- ord_data_for_mv(data = data_RIDAGEYR, ord_var = ord_var)

RIDAGEYR_me <- me_vib_sim(data = data_RIDAGEYR, ord_data = data_ord_RIDAGEYR, voi = "RIDAGEYR", voi_type = "Num", adj_var = adj_var_full,
                                   adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(RIDAGEYR_me, file = "Results/measurement_sim/RIDAGEYR_me.RData")



###   SMD410

set.seed(seeds[30])
data_SMD410 <- data_for_mv(mainTab, voi = "SMD410", voi_type = "Bin", adj_type = adj_type_full, 
                         adj_var = adj_var_full)

data_ord_SMD410 <- ord_data_for_mv(data = data_SMD410, ord_var = ord_var)


SMD410_me <- me_vib_sim(data = data_SMD410, ord_data = data_ord_SMD410, voi = "SMD410", voi_type = "Bin", adj_var = adj_var_full,
                              adj_type = adj_type_full, me_range = me_mix, B = 1000)


save(SMD410_me, file = "Results/measurement_sim/SMD410_me.RData")
