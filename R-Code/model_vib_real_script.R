# Script for the assessment of model vibration on real data

source('model_vib_real_fun.R')

load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))

mainTab$current_past_smoking[which(mainTab$current_past_smoking == 2)] <- 1
mainTab$current_past_smoking[which(mainTab$current_past_smoking == 3)] <- 2


########################
###   Main results   ###
########################

mod_real_any_diabetes <- model_vib_real(mainTab, voi = "any_diabetes", continuous = F, log = F)#, covariates = covariates)
save(mod_real_any_diabetes, file = "Results/model_real/mod_real_any_diabetes.RData")

mod_real_any_cad <- model_vib_real(voi = "any_cad", continuous = F, log = F, covariates = covariates)
save(mod_real_any_cad, file = "Results/model_real/mod_real_any_cad.RData")

mod_real_BMXTHICR <- model_vib_real(voi = "BMXTHICR", continuous = T, log = F, covariates = covariates)
save(mod_real_BMXTHICR, file = "Results/model_real/mod_real_BMXTHICR.RData")

mod_real_LBDHDL <- model_vib_real(voi = "LBDHDL", continuous = T, log = F, covariates = covariates)
save(mod_real_LBDHDL, file = "Results/model_real/mod_real_LBDHDL.RData")

################################
###   Supplemental results   ###
################################

mod_real_any_ht <- model_vib_real(voi = "any_ht", continuous = F, log = F, covariates = covariates)
save(mod_real_any_ht, file = "Results/model_real/mod_real_any_ht.RData")

mod_real_any_cancer_self_report <- model_vib_real(voi = "any_cancer_self_report", continuous = F, log = F, covariates = covariates)
save(mod_real_any_cancer_self_report, file = "Results/model_real/mod_real_any_cancer_self_report.RData")

mod_real_any_family_cad <- model_vib_real(voi = "any_family_cad", continuous = F, log = F, covariates = covariates)
save(mod_real_any_family_cad, file = "Results/model_real/mod_real_any_family_cad.RData")

mod_real_LBDNENO <- model_vib_real(voi = "LBDNENO", continuous = T, log = F, covariates = covariates)
save(mod_real_LBDNENO, file = "Results/model_real/mod_real_LBDNENO.RData")

mod_real_BMXCALF <- model_vib_real(voi = "BMXCALF", continuous = T, log = F, covariates = covariates)
save(mod_real_BMXCALF, file = "Results/model_real/mod_real_BMXCALF.RData")

mod_real_BMXHT <- model_vib_real(voi = "BMXHT", continuous = T, log = F, covariates = covariates)
save(mod_real_BMXHT, file = "Results/model_real/mod_real_BMXHT.RData")

mod_real_BMXWAIST <- model_vib_real(voi = "BMXWAIST", continuous = T, log = F, covariates = covariates)
save(mod_real_BMXWAIST, file = "Results/model_real/mod_real_BMXWAIST.RData")

mod_real_BMXWT <- model_vib_real(voi = "BMXWT", continuous = T, log = F, covariates = covariates)
save(mod_real_BMXWT, file = "Results/model_real/mod_real_BMXWT.RData")

mod_real_BPXPLS <- model_vib_real(voi = "BPXPLS", continuous = T, log = F, covariates = covariates)
save(mod_real_BPXPLS, file = "Results/model_real/mod_real_BPXPLS.RData")

mod_real_DSDCOUNT <- model_vib_real(voi = "DSDCOUNT", continuous = T, log = F, covariates = covariates)
save(mod_real_DSDCOUNT, file = "Results/model_real/mod_real_DSDCOUNT.RData")

mod_real_LBXHA <- model_vib_real(voi = "LBXHA", continuous = F, log = F, covariates = covariates)
save(mod_real_LBXHA, file = "Results/model_real/mod_real_LBXHA.RData")

mod_real_LBXHBC <- model_vib_real(voi = "LBXHBC", continuous = F, log = F, covariates = covariates)
save(mod_real_LBXHBC, file = "Results/model_real/mod_real_LBXHBC.RData")

mod_real_LBXHBS <- model_vib_real(voi = "LBXHBS", continuous = F, log = F, covariates = covariates)
save(mod_real_LBXHBS, file = "Results/model_real/mod_real_LBXHBS.RData")

mod_real_LBXLYPCT <- model_vib_real(voi = "LBXLYPCT", continuous = T, log = F, covariates = covariates)
save(mod_real_LBXLYPCT, file = "Results/model_real/mod_real_LBXLYPCT.RData")

mod_real_LBXMPSI <- model_vib_real(voi = "LBXMPSI", continuous = T, log = F, covariates = covariates)
save(mod_real_LBXMPSI, file = "Results/model_real/mod_real_LBXMPSI.RData")

mod_real_LBXSCA <- model_vib_real(voi = "LBXSCA", continuous = T, log = F, covariates = covariates)
save(mod_real_LBXSCA, file = "Results/model_real/mod_real_LBXSCA.RData")

mod_real_LBXSNASI <- model_vib_real(voi = "LBXSNASI", continuous = T, log = F, covariates = covariates)
save(mod_real_LBXSNASI, file = "Results/model_real/mod_real_LBXSNASI.RData")

mod_real_LBXSOSSI <- model_vib_real(voi = "LBXSOSSI", continuous = T, log = F, covariates = covariates)
save(mod_real_LBXSOSSI, file = "Results/model_real/mod_real_LBXSOSSI.RData")

mod_real_LBXSPH <- model_vib_real(voi = "LBXSPH", continuous = T, log = F, covariates = covariates)
save(mod_real_LBXSPH, file = "Results/model_real/mod_real_LBXSPH.RData")

mod_real_male <- model_vib_real(voi = "male", continuous = F, log = F, covariates = covariates)
save(mod_real_male, file = "Results/model_real/mod_real_male.RData")

mod_real_pest_control_home_last_month <- model_vib_real(voi = "pest_control_home_last_month", continuous = F, log = F, covariates = covariates)
save(mod_real_pest_control_home_last_month, file = "Results/model_real/mod_real_pest_control_home_last_month.RData")

mod_real_pneu <- model_vib_real(voi = "pneu", continuous = F, log = F, covariates = covariates)
save(mod_real_pneu, file = "Results/model_real/mod_real_pneu.RData")

mod_real_private_water_source <- model_vib_real(voi = "private_water_source", continuous = F, log = F, covariates = covariates)
save(mod_real_private_water_source, file = "Results/model_real/mod_real_private_water_source.RData")

mod_real_use_water_treatment <- model_vib_real(voi = "use_water_treatment", continuous = F, log = F, covariates = covariates)
save(mod_real_use_water_treatment, file = "Results/model_real/mod_real_use_water_treatment.RData")

mod_real_RIDAGEYR <- model_vib_real(voi = "RIDAGEYR", continuous = T, log = F, covariates = covariates)
save(mod_real_RIDAGEYR, file = "Results/model_real/mod_real_RIDAGEYR.RData")

mod_real_SMD410 <- model_vib_real(voi = "SMD410", continuous = F, log = F, covariates = covariates)
save(mod_real_SMD410, file = "Results/model_real/mod_real_SMD410.RData")