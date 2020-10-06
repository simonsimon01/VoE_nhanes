# Script for the assessment of sampling vibration on real data

library(survival)
library(MASS)

source("sample_vib_real_fun.R")

load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))

mainTab$current_past_smoking[which(mainTab$current_past_smoking == 2)] <- 1
mainTab$current_past_smoking[which(mainTab$current_past_smoking == 3)] <- 2

# from runif(1, 0, 10^8)
set.seed(40556898)
seeds <- sample(1000:10000000, size = 30)


########################
###   Main results   ###
########################

### any_diabetes

set.seed(seeds[1])

any_diabetes_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "any_diabetes", continuous = F, log = F,
                                            B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(any_diabetes_sam_05_full, file = "Results/sample_real/any_diabetes_sam_05.RData")


### any_cad

set.seed(seeds[2])

any_cad_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "any_cad", continuous = F, log = F,
                                       B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(any_cad_sam_05_full, file = "Results/sample_real/any_cad_sam_05.RData")



### BMXTHICR

set.seed(seeds[3])

BMXTHICR_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "BMXTHICR", continuous = T, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(BMXTHICR_sam_05_full, file = "Results/sample_real/BMXTHICR_sam_05.RData")


### LBDHDL

set.seed(seeds[4])

LBDHDL_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBDHDL", continuous = F, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBDHDL_sam_05_full, file = "Results/sample_real/LBDHDL_sam_05.RData")


################################
###   Supplemental results   ###
################################


### any_ht

set.seed(seeds[5])

any_ht_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "any_ht", continuous = F, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(any_ht_sam_05_full, file = "Results/sample_real/any_ht_sam_05.RData")


### any_cancer_self_report

set.seed(seeds[6])

any_cancer_self_report_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "any_cancer_self_report", continuous = F, log = F,
                                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(any_cancer_self_report_sam_05_full, file = "Results/sample_real/any_cancer_self_report_sam_05.RData")



### any_family_cad

set.seed(seeds[7])

any_family_cad_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "any_family_cad", continuous = F, log = F,
                                              B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(any_family_cad_sam_05_full, file = "Results/sample_real/any_family_cad_sam_05.RData")


### LBDNENO

set.seed(seeds[8])

LBDNENO_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBDNENO", continuous = T, log = F,
                                       B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBDNENO_sam_05_full, file = "Results/sample_real/LBDNENO_sam_05.RData")


### BMXCALF

set.seed(seeds[9])

BMXCALF_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "BMXCALF", continuous = T, log = F,
                                       B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(BMXCALF_sam_05_full, file = "Results/sample_real/BMXCALF_sam_05.RData")



### BMXHT

set.seed(seeds[10])

BMXHT_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "BMXHT", continuous = T, log = F,
                                     B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(BMXHT_sam_05_full, file = "Results/sample_real/BMXHT_sam_05.RData")


### BMXWAIST

set.seed(seeds[11])

BMXWAIST_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "BMXWAIST", continuous = T, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(BMXWAIST_sam_05_full, file = "Results/sample_real/BMXWAIST_sam_05.RData")



### BMXWT

set.seed(seeds[12])

BMXWT_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "BMXWT", continuous = T, log = F,
                                     B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(BMXWT_sam_05_full, file = "Results/sample_real/BMXWT_sam_05.RData")



### BPXPLS

set.seed(seeds[13])

BPXPLS_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "BPXPLS", continuous = T, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(BPXPLS_sam_05_full, file = "Results/sample_real/BPXPLS_sam_05.RData")


### DSDCOUNT

set.seed(seeds[14])

DSDCOUNT_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "DSDCOUNT", continuous = T, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(DSDCOUNT_sam_05_full, file = "Results/sample_real/DSDCOUNT_sam_05.RData")



### LBXHA

set.seed(seeds[15])

LBXHA_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXHA", continuous = F, log = F,
                                     B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXHA_sam_05_full, file = "Results/sample_real/LBXHA_sam_05.RData")


### LBXHBC

set.seed(seeds[16])

LBXHBC_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXHBC", continuous = F, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXHBC_sam_05_full, file = "Results/sample_real/LBXHBC_sam_05.RData")



### LBXHBS

set.seed(seeds[17])

LBXHBS_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXHBS", continuous = F, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXHBS_sam_05_full, file = "Results/sample_real/LBXHBS_sam_05.RData")


### LBXLYPCT

set.seed(seeds[18])

LBXLYPCT_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXLYPCT", continuous = T, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXLYPCT_sam_05_full, file = "Results/sample_real/LBXLYPCT_sam_05.RData")


### LBXMPSI

set.seed(seeds[19])

LBXMPSI_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXMPSI", continuous = T, log = F,
                                       B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXMPSI_sam_05_full, file = "Results/sample_real/LBXMPSI_sam_05.RData")



### LBXSCA

set.seed(seeds[20])

LBXSCA_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXSCA", continuous = T, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXSCA_sam_05_full, file = "Results/sample_real/LBXSCA_sam_05.RData")


### LBXSNASI

set.seed(seeds[21])

LBXSNASI_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXSNASI", continuous = T, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXSNASI_sam_05_full, file = "Results/sample_real/LBXSNASI_sam_05.RData")


### LBXSOSSI

set.seed(seeds[22])

LBXSOSSI_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXSOSSI", continuous = T, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXSOSSI_sam_05_full, file = "Results/sample_real/LBXSOSSI_sam_05.RData")



### LBXSPH

set.seed(seeds[23])

LBXSPH_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "LBXSPH", continuous = T, log = F,
                                      B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(LBXSPH_sam_05_full, file = "Results/sample_real/LBXSPH_sam_05.RData")


### male

set.seed(seeds[24])

male_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "male", continuous = F, log = F,
                                    B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(male_sam_05_full, file = "Results/sample_real/male_sam_05.RData")


### pest_control_home_last_month

set.seed(seeds[25])

pest_control_home_last_month_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "pest_control_home_last_month", continuous = F, log = F,
                                                            B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(pest_control_home_last_month_sam_05_full, file = "Results/sample_real/pest_control_home_last_month_sam_05.RData")



### pneu

set.seed(seeds[26])

pneu_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "pneu", continuous = F, log = F,
                                    B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(pneu_sam_05_full, file = "Results/sample_real/pneu_sam_05.RData")


### private_water_source

set.seed(seeds[27])

private_water_source_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "private_water_source", continuous = F, log = F,
                                                    B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(private_water_source_sam_05_full, file = "Results/sample_real/private_water_source_sam_05.RData")


### use_water_treatment

set.seed(seeds[28])

use_water_treatment_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "use_water_treatment", continuous = F, log = F,
                                                   B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(use_water_treatment_sam_05_full, file = "Results/sample_real/use_water_treatment_sam_05.RData")


### RIDAGEYR

set.seed(seeds[29])

RIDAGEYR_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "RIDAGEYR", continuous = T, log = F,
                                          B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(RIDAGEYR_sam_05_full, file = "Results/sample_real/RIDAGEYR_sam_05.RData")





### SMD410

set.seed(seeds[30])

SMD410_sam_05_full <- sample_vib_real(mainTab = mainTab, voi = "SMD410", continuous = F, log = F,
                                        B = 1000, replace = FALSE, adj_var = "full", exactPvalue = TRUE)

save(SMD410_sam_05_full, file = "Results/sample_real/SMD410_sam_05.RData")