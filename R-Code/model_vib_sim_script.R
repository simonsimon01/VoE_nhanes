# Script for the assessment of model vibraton on simulated data


library(survival)
library(mvtnorm)
library(GenOrd)


source('model_vib_sim_fun.R')
source('data_gen_functions.R')


load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))

mainTab$current_past_smoking[which(mainTab$current_past_smoking == 2)] <- 1
mainTab$current_past_smoking[which(mainTab$current_past_smoking == 3)] <- 2


adj_var_full <- c('physical_activity', 'RIDRETH1', 'RIDAGEYR', 'male', 
                  'any_cancer_self_report', 'current_past_smoking', 
                  'bmi', 'any_ht', 'any_diabetes','LBXTC', 'drink_five_per_day', 
                  'education', 'SES_LEVEL', 'any_family_cad', 'any_cad')

adj_type_full <- c("Ord", "Num", "Num", "Bin", "Bin", "Bin", "Ord", "Bin", "Bin", "Num",
                   "Bin", "Ord", "Ord", "Bin", "Bin")

var_vec <- c("male", "RIDAGEYR")


fac_ind_full <- c(T,T,F,T,T,T,T,T,T,F,T,T,T,T,T)


mainTab$physical_activity <- mainTab$physical_activity + 1
mainTab$male <- mainTab$male + 1
mainTab$any_cancer_self_report <- mainTab$any_cancer_self_report + 1
mainTab$any_ht <- mainTab$any_ht + 1
mainTab$any_diabetes <- mainTab$any_diabetes + 1
mainTab$drink_five_per_day <- mainTab$drink_five_per_day + 1
mainTab$any_cad <- mainTab$any_cad + 1
mainTab$any_family_cad <- mainTab$any_family_cad + 1


covariates <- ~ SES_LEVEL + education + RIDRETH1 + bmi + any_cad + any_family_cad + any_diabetes + 
  any_cancer_self_report +  current_past_smoking  + drink_five_per_day + physical_activity  + LBXTC + any_ht



set.seed(1234)
seeds <- sample(10000:10000000, size = 30)


########################
###   Main results   ###
########################


set.seed(seeds[1])
data_any_diabetes <- data_for_mv(mainTab, voi = "any_diabetes", voi_type = "Bin", adj_type = adj_type_full, 
                                 adj_var = adj_var_full)

mod_sim_any_diabetes <- mod_vib_sim(data_new = data_any_diabetes, variable = "any_diabetes", adj_var_full = adj_var_full,
                                    var_vec = var_vec, covariates = covariates)
save(mod_sim_any_diabetes, file = "Results/model_sim/mod_sim_any_diabetes.RData")


set.seed(seeds[2])
data_any_cad <- data_for_mv(mainTab, voi = "any_cad", voi_type = "Bin", adj_type = adj_type_full, 
                            adj_var = adj_var_full)

mod_sim_any_cad <- mod_vib_sim(data_new = data_any_cad, variable = "any_cad", adj_var_full = adj_var_full,
                               var_vec = var_vec, covariates = covariates)
save(mod_sim_any_cad, file = "Results/model_sim/mod_sim_any_cad.RData")


set.seed(seeds[3])
data_BMXTHICR <- data_for_mv(mainTab, voi = "BMXTHICR", voi_type = "Num", adj_type = adj_type_full, 
                             adj_var = adj_var_full)

mod_sim_BMXTHICR <- mod_vib_sim(data_new = data_BMXTHICR, variable = "BMXTHICR", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_BMXTHICR, file = "Results/model_sim/mod_sim_BMXTHICR.RData")


set.seed(seeds[4])
data_LBDHDL <- data_for_mv(mainTab, voi = "LBDHDL", voi_type = "Num", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_LBDHDL <- mod_vib_sim(data_new = data_LBDHDL, variable = "LBDHDL", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_LBDHDL, file = "Results/model_sim/mod_sim_LBDHDL.RData")


################################
###   Supplemental results   ###
################################

set.seed(seeds[5])
data_any_ht <- data_for_mv(mainTab, voi = "any_ht", voi_type = "Bin", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_any_ht <- mod_vib_sim(data_new = data_any_ht, variable = "any_ht", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_any_ht, file = "Results/model_sim/mod_sim_any_ht.RData")


set.seed(seeds[6])
data_any_cancer_self_report <- data_for_mv(mainTab, voi = "any_cancer_self_report", voi_type = "Bin", adj_type = adj_type_full, 
                                           adj_var = adj_var_full)

mod_sim_any_cancer_self_report <- mod_vib_sim(data_new = data_any_cancer_self_report, variable = "any_cancer_self_report", adj_var_full = adj_var_full,
                                              var_vec = var_vec, covariates = covariates)
save(mod_sim_any_cancer_self_report, file = "Results/model_sim/mod_sim_any_cancer_self_report.RData")


set.seed(seeds[7])
data_any_family_cad <- data_for_mv(mainTab, voi = "any_family_cad", voi_type = "Bin", adj_type = adj_type_full, 
                                   adj_var = adj_var_full)

mod_sim_any_family_cad <- mod_vib_sim(data_new = data_any_family_cad, variable = "any_family_cad", adj_var_full = adj_var_full,
                                              var_vec = var_vec, covariates = covariates)
save(mod_sim_any_family_cad, file = "Results/model_sim/mod_sim_any_family_cad.RData")


set.seed(seeds[8])
data_LBDNENO <- data_for_mv(mainTab, voi = "LBDNENO", voi_type = "Num", adj_type = adj_type_full, 
                            adj_var = adj_var_full)

mod_sim_LBDNENO <- mod_vib_sim(data_new = data_LBDNENO, variable = "LBDNENO", adj_var_full = adj_var_full,
                               var_vec = var_vec, covariates = covariates)
save(mod_sim_LBDNENO, file = "Results/model_sim/mod_sim_LBDNENO.RData")


set.seed(seeds[9])
data_BMXCALF <- data_for_mv(mainTab, voi = "BMXCALF", voi_type = "Num", adj_type = adj_type_full, 
                            adj_var = adj_var_full)

mod_sim_BMXCALF <- mod_vib_sim(data_new = data_BMXCALF, variable = "BMXCALF", adj_var_full = adj_var_full,
                               var_vec = var_vec, covariates = covariates)
save(mod_sim_BMXCALF, file = "Results/model_sim/mod_sim_BMXCALF.RData")


set.seed(seeds[10])
data_BMXHT <- data_for_mv(mainTab, voi = "BMXHT", voi_type = "Num", adj_type = adj_type_full, 
                          adj_var = adj_var_full)

mod_sim_BMXHT <- mod_vib_sim(data_new = data_BMXHT, variable = "BMXHT", adj_var_full = adj_var_full,
                             var_vec = var_vec, covariates = covariates)
save(mod_sim_BMXHT, file = "Results/model_sim/mod_sim_BMXHT.RData")


set.seed(seeds[11])
data_BMXWAIST <- data_for_mv(mainTab, voi = "BMXWAIST", voi_type = "Num", adj_type = adj_type_full, 
                             adj_var = adj_var_full)

mod_sim_BMXWAIST <- mod_vib_sim(data_new = data_BMXWAIST, variable = "BMXWAIST", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_BMXWAIST, file = "Results/model_sim/mod_sim_BMXWAIST.RData")



set.seed(seeds[12])
data_BMXWT <- data_for_mv(mainTab, voi = "BMXWT", voi_type = "Num", adj_type = adj_type_full, 
                          adj_var = adj_var_full)

mod_sim_BMXWT <- mod_vib_sim(data_new = data_BMXWT, variable = "BMXWT", adj_var_full = adj_var_full,
                             var_vec = var_vec, covariates = covariates)
save(mod_sim_BMXWT, file = "Results/model_sim/mod_sim_BMXWT.RData")


set.seed(seeds[13])
data_BPXPLS <- data_for_mv(mainTab, voi = "BPXPLS", voi_type = "Num", adj_type = adj_type_full,
                           adj_var = adj_var_full)

mod_sim_BPXPLS <- mod_vib_sim(data_new = data_BPXPLS, variable = "BPXPLS", adj_var_full = adj_var_full,
                               var_vec = var_vec, covariates = covariates)
save(mod_sim_BPXPLS, file = "Results/model_sim/mod_sim_BPXPLS.RData")


set.seed(seeds[14])
data_DSDCOUNT <- data_for_mv(mainTab, voi = "DSDCOUNT", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

mod_sim_DSDCOUNT <- mod_vib_sim(data_new = data_DSDCOUNT, variable = "DSDCOUNT", adj_var_full = adj_var_full,
                                 var_vec = var_vec, covariates = covariates)
save(mod_sim_DSDCOUNT, file = "Results/model_sim/mod_sim_DSDCOUNT.RData")


set.seed(seeds[15])
data_LBXHA <- data_for_mv(mainTab, voi = "LBXHA", voi_type = "Bin", adj_type = adj_type_full,
                          adj_var = adj_var_full)

mod_sim_LBXHA <- mod_vib_sim(data_new = data_LBXHA, variable = "LBXHA", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXHA, file = "Results/model_sim/mod_sim_LBXHA.RData")


set.seed(seeds[16])
data_LBXHBC <- data_for_mv(mainTab, voi = "LBXHBC", voi_type = "Num", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_LBXHBC <- mod_vib_sim(data_new = data_LBXHBC, variable = "LBXHBC", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXHBC, file = "Results/model_sim/mod_sim_LBXHBC.RData")


set.seed(seeds[17])
data_LBXHBS <- data_for_mv(mainTab, voi = "LBXHBS", voi_type = "Bin", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_LBXHBS <- mod_vib_sim(data_new = data_LBXHBS, variable = "LBXHBS", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXHBS, file = "Results/model_sim/mod_sim_LBXHBS.RData")


set.seed(seeds[18])
data_LBXLYPCT <- data_for_mv(mainTab, voi = "LBXLYPCT", voi_type = "Num", adj_type = adj_type_full, 
                             adj_var = adj_var_full)

mod_sim_LBXLYPCT <- mod_vib_sim(data_new = data_LBXLYPCT, variable = "LBXLYPCT", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXLYPCT, file = "Results/model_sim/mod_sim_LBXLYPCT.RData")


set.seed(seeds[19])
data_LBXMPSI <- data_for_mv(mainTab, voi = "LBXMPSI", voi_type = "Num", adj_type = adj_type_full, 
                            adj_var = adj_var_full)

mod_sim_LBXMPSI <- mod_vib_sim(data_new = data_LBXMPSI, variable = "LBXMPSI", adj_var_full = adj_var_full,
                               var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXMPSI, file = "Results/model_sim/mod_sim_LBXMPSI.RData")


set.seed(seeds[20])
data_LBXSCA <- data_for_mv(mainTab, voi = "LBXSCA", voi_type = "Num", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_LBXSCA <- mod_vib_sim(data_new = data_LBXSCA, variable = "LBXSCA", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXSCA, file = "Results/model_sim/mod_sim_LBXSCA.RData")


set.seed(seeds[21])
data_LBXSNASI <- data_for_mv(mainTab, voi = "LBXSNASI", voi_type = "Num", adj_type = adj_type_full, 
                             adj_var = adj_var_full)

mod_sim_LBXSNASI <- mod_vib_sim(data_new = data_LBXSNASI, variable = "LBXSNASI", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXSNASI, file = "Results/model_sim/mod_sim_LBXSNASI.RData")


set.seed(seeds[22])
data_LBXSOSSI <- data_for_mv(mainTab, voi = "LBXSOSSI", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

mod_sim_LBXSOSSI <- mod_vib_sim(data_new = data_LBXSOSSI, variable = "LBXSOSSI", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXSOSSI, file = "Results/model_sim/mod_sim_LBXSOSSI.RData")



set.seed(seeds[23])
data_LBXSPH <- data_for_mv(mainTab, voi = "LBXSPH", voi_type = "Num", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_LBXSPH <- mod_vib_sim(data_new = data_LBXSPH, variable = "LBXSPH", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_LBXSPH, file = "Results/model_sim/mod_sim_LBXSPH.RData")


set.seed(seeds[24])
data_male <- data_for_mv(mainTab, voi = "male", voi_type = "Bin", adj_type = adj_type_full,
                         adj_var = adj_var_full)

mod_sim_male <- mod_vib_sim(data_new = data_male, variable = "male", adj_var_full = adj_var_full,
                            var_vec = var_vec, covariates = covariates)
save(mod_sim_male, file = "Results/model_sim/mod_sim_male.RData")



set.seed(seeds[25])
data_pest_control_home_last_month <- data_for_mv(mainTab, voi = "pest_control_home_last_month", voi_type = "Bin", adj_type = adj_type_full,
                                                 adj_var = adj_var_full)

mod_sim_pest_control_home_last_month <- mod_vib_sim(data_new = data_pest_control_home_last_month, variable = "pest_control_home_last_month", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_pest_control_home_last_month, file = "Results/model_sim/mod_sim_pest_control_home_last_month.RData")



set.seed(seeds[26])
data_pneu <- data_for_mv(mainTab, voi = "pneu", voi_type = "Bin", adj_type = adj_type_full,
                         adj_var = adj_var_full)

mod_sim_pneu <- mod_vib_sim(data_new = data_pneu, variable = "pneu", adj_var_full = adj_var_full,
                            var_vec = var_vec, covariates = covariates)
save(mod_sim_pneu, file = "Results/model_sim/mod_sim_pneu.RData")


set.seed(seeds[27])
data_private_water_source <- data_for_mv(mainTab, voi = "private_water_source", voi_type = "Bin", adj_type = adj_type_full,
                                         adj_var = adj_var_full)

mod_sim_private_water_source <- mod_vib_sim(data_new = data_private_water_source, variable = "private_water_source", adj_var_full = adj_var_full,
                                            var_vec = var_vec, covariates = covariates)
save(mod_sim_private_water_source, file = "Results/model_sim/mod_sim_private_water_source.RData")


set.seed(seeds[28])
data_use_water_treatment <- data_for_mv(mainTab, voi = "use_water_treatment", voi_type = "Bin", adj_type = adj_type_full,
                                        adj_var = adj_var_full)

mod_sim_use_water_treatment <- mod_vib_sim(data_new = data_use_water_treatment, variable = "use_water_treatment", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_use_water_treatment, file = "Results/model_sim/mod_sim_use_water_treatment.RData")


set.seed(seeds[29])
data_RIDAGEYR <- data_for_mv(mainTab, voi = "RIDAGEYR", voi_type = "Num", adj_type = adj_type_full,
                             adj_var = adj_var_full)

mod_sim_RIDAGEYR <- mod_vib_sim(data_new = data_RIDAGEYR, variable = "RIDAGEYR", adj_var_full = adj_var_full,
                                var_vec = var_vec, covariates = covariates)
save(mod_sim_RIDAGEYR, file = "Results/model_sim/mod_sim_RIDAGEYR.RData")


set.seed(seeds[30])
data_SMD410 <- data_for_mv(mainTab, voi = "SMD410", voi_type = "Bin", adj_type = adj_type_full, 
                           adj_var = adj_var_full)

mod_sim_SMD410 <- mod_vib_sim(data_new = data_SMD410, variable = "SMD410", adj_var_full = adj_var_full,
                              var_vec = var_vec, covariates = covariates)
save(mod_sim_SMD410, file = "Results/model_sim/mod_sim_SMD410.RData")