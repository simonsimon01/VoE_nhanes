# Script for the assessment of measurement vibration on real data 
# for measurement error in adjustment variables


source("measurement_vib_ordinal.R")
source("measurement_vib_real_fun.R")

load(url('https://github.com/chiragjp/voe/blob/gh-pages/nhanes9904_VoE.Rdata?raw=true'))

mainTab$current_past_smoking[which(mainTab$current_past_smoking == 2)] <- 1
mainTab$current_past_smoking[which(mainTab$current_past_smoking == 3)] <- 2

# Adjustment variables

adj_var <- c('physical_activity', 'RIDRETH1', 'RIDAGEYR', 'male', 
             'any_cancer_self_report', 'current_past_smoking', 
             'bmi', 'any_ht', 'any_diabetes','LBXTC', 'drink_five_per_day', 
             'education', 'SES_LEVEL', 'any_family_cad', 'any_cad')

# Type of adjustment variables

adj_type <- c("Ord", "Num", "Num", "Bin", "Bin", "Bin", "Ord", "Bin", "Bin", "Num", 
              "Bin", "Ord", "Ord", "Bin", "Bin")

### Specify values for measurement error:

# Adjustment variables; high measurement error --> low correlation, sensitivity, specificity

me_adj_high <- list(physical_activity = 0.02,
                    RIDRETH1 = 1,
                    RIDAGEYR = 1,
                    male = c(1,1),
                    any_cancer_self_report = c(0.33, 0.14),
                    current_past_smoking = c(0.77, 0.79),
                    bmi = 0.48,
                    any_ht = c(0.14, 0.71),
                    any_diabetes = c(0.49, 0.91),
                    LBXTC = 0.63,
                    drink_five_per_day = c(0.38, 0.64),
                    education = 0.99,
                    SES_LEVEL = 0.09,
                    any_family_cad = c(0.67, 0.59),
                    any_cad = c(0.88, 0.98))

# Adjustment variables; low measurement error --> high correlation, sensitivity, specificity

me_adj_low <- list(physical_activity = 0.74,
                   RIDRETH1 = 1,
                   RIDAGEYR = 1,
                   male = c(1,1),
                   any_cancer_self_report = c(0.98, 0.96),
                   current_past_smoking = c(1, 1),
                   bmi = 0.88,
                   any_ht = c(0.71, 0.99),
                   any_diabetes = c(0.8, 1),
                   LBXTC = 0.77,
                   drink_five_per_day = c(0.96, 1),
                   education = 0.99,
                   SES_LEVEL = 0.81,
                   any_family_cad = c(0.89, 0.97),
                   any_cad = c(0.88, 0.98))


# Variable of interest; high measurement error

voi_me_high <- list(RIDAGEYR = list(err = 1, type = "Num"),
                    any_cad = list(err = c(0.88, 0.98), type = "Bin"),
                    any_cancer_self_report = list(err = c(0.33, 0.14), type = "Bin"),
                    pneu = list(err = c(NA, NA), type = "Bin"),
                    any_diabetes = list(err = c(0.49, 0.91), type = "Bin"),
                    any_ht = list(err = c(0.14, 0.71), type = "Bin"),
                    LBXHA = list(err = NA, type = "Bin"),
                    BMXTHICR = list(err = 0.98, type = "Num"),
                    BMXCALF = list(err = 0.99, type = "Num"),
                    LBXHBC = list(err = 0.74, type = "Num"),
                    LBXSOSSI = list(err = NA, type = "Num"),
                    male = list(err = c(1,1), type = "Bin"),
                    LBXLYPCT = list(err = 0.9, type = "Num"),
                    any_family_cad = list(err = c(0.67, 0.59), type = "Bin"),
                    LBXHBS = list(err = 0.74, type = "Num"),
                    LBXSNASI = list(err = 0.68, type = "Num"),
                    BPXPLS = list(err = NA, type = "Num"),
                    LBXSCA = list(err = 0.3, type = "Num"),
                    LBDHDL = list(err = 0.7, type = "Num"),
                    private_water_source = list(err = c(NA, NA), type = "Bin"),
                    pest_control_home_last_month = list(err = c(NA, NA), type = "Bin"),
                    LBXMPSI = list(err = 0.66, type = "Num"),
                    LBXSPH = list(err = 0.36, type = "Num"),
                    BMXHT = list(err = 0.59, type = "Num"),
                    LBDNENO = list(err = 0.95, type = "Num"),
                    BMXWAIST = list(err = 0.61, type = "Num"),
                    BMXWT = list(err = 0.81, type = "Num"),
                    DSDCOUNT = list(err = NA, type = "Num"),
                    SMD410 = list(err = c(0.38, 0.79), type = "Bin"),
                    use_water_treatment = list(err = c(NA, NA), type = "Bin"))

# Variable of interest; low measurement error

voi_me_low <- list(RIDAGEYR = list(err = 1, type = "Num"),
                   any_cad = list(err = c(0.88, 0.98), type = "Bin"),
                   any_cancer_self_report = list(err = c(0.98, 0.96), type = "Bin"),
                   pneu = list(err = c(NA, NA), type = "Bin"), #
                   any_diabetes = list(err = c(0.8, 1), type = "Bin"),
                   any_ht = list(err = c(0.71, 0.99), type = "Bin"),
                   LBXHA = list(err = NA, type = "Bin"),
                   BMXTHICR = list(err = 0.98, type = "Num"),
                   BMXCALF = list(err = 0.99, type = "Num"),
                   LBXHBC = list(err = 0.84, type = "Num"),
                   LBXSOSSI = list(err = NA, type = "Num"), #
                   male = list(err = c(1,1), type = "Bin"),
                   LBXLYPCT = list(err = 0.98, type = "Num"),
                   any_family_cad = list(err = c(0.89, 0.97), type = "Bin"),
                   LBXHBS = list(err = 0.84, type = "Num"),
                   LBXSNASI = list(err = 0.96, type = "Num"),
                   BPXPLS = list(err = NA, type = "Num"), #
                   LBXSCA = list(err = 0.69, type = "Num"),
                   LBDHDL = list(err = 0.79, type = "Num"),
                   private_water_source = list(err = c(NA, NA), type = "Bin"), #
                   pest_control_home_last_month = list(err = c(NA, NA), type = "Bin"), #
                   LBXMPSI = list(err = 0.99, type = "Num"),
                   LBXSPH = list(err = 0.54, type = "Num"),
                   BMXHT = list(err = 0.98, type = "Num"),
                   LBDNENO = list(err = 0.99, type = "Num"),
                   BMXWAIST = list(err = 0.99, type = "Num"),
                   BMXWT = list(err = 0.93, type = "Num"),
                   DSDCOUNT = list(err = NA, type = "Num"), #
                   SMD410 = list(err = c(0.66, 0.97), type = "Bin"),
                   use_water_treatment = list(err = c(NA, NA), type = "Bin"))  #


### range of measurement errors

voi_type <- lapply(voi_me_high, function(X) X$type)

# binary

voi_bin <- which(voi_type == "Bin")

sens_voi_high <- lapply(voi_me_high[voi_bin], function(X) X$err[1])
spec_voi_high <- lapply(voi_me_high[voi_bin], function(X) X$err[2])

sens_voi_low <- lapply(voi_me_low[voi_bin], function(X) X$err[1])
spec_voi_low <- lapply(voi_me_low[voi_bin], function(X) X$err[2])

sens_adj_high <- lapply(me_adj_high[which(adj_type == "Bin")], function(X) X[1])
spec_adj_high <- lapply(me_adj_high[which(adj_type == "Bin")], function(X) X[2])

sens_adj_low <- lapply(me_adj_low[which(adj_type == "Bin")], function(X) X[1])
spec_adj_low <- lapply(me_adj_low[which(adj_type == "Bin")], function(X) X[2])

sens_range <- summary(c(unlist(sens_voi_low), unlist(sens_voi_high), unlist(sens_adj_high), 
                        unlist(sens_adj_low)))
spec_range <- summary(c(unlist(spec_adj_low), unlist(spec_adj_high), unlist(spec_voi_high), 
                        unlist(spec_voi_low)))


### numeric

voi_num <- which(voi_type == "Num")

corr_voi_high <- lapply(voi_me_high[voi_num], function(X) X$err)
corr_voi_low <- lapply(voi_me_low[voi_num], function(X) X$err)

corr_adj_high <- me_adj_high[which(adj_type == "Num")]
corr_adj_low <- me_adj_low[which(adj_type == "Num")]

corr_range <- summary(c(unlist(corr_voi_high), unlist(corr_voi_low), unlist(corr_adj_high),
                        unlist(corr_adj_low)))


# calculate average values of sensitivities, specificities and correlations

sens_high_mean <- mean(unlist(sens_voi_high), na.rm = T)
sens_low_mean <- mean(unlist(sens_voi_low), na.rm = T)

spec_high_mean <- mean(unlist(spec_voi_high), na.rm = T)
spec_low_mean <- mean(unlist(spec_voi_low), na.rm = T)

corr_high_mean <- mean(unlist(corr_voi_high), na.rm = T)
corr_low_mean <- mean(unlist(corr_voi_low), na.rm = T)





me_mix <- list(correlation = c(round(corr_high_mean,2), round(corr_low_mean,2)),
               sensitivity = c(round(sens_high_mean, 2), round(sens_low_mean, 2)),
               specificity = c(round(spec_high_mean, 2), round(spec_low_mean, 2)))

# correlation: 0.73, 0.9
# sensitivity: 0.56, 0.85
# specificity: 0.73, 0.98

### Search for parameters of the normal distribution (ordinal variables) through a grid search
#
# set.seed(1234)
# bmi_ord <- me_ordinal_gs(data = mainTab, var_name = "bmi")
# physical_activity_ord <- me_ordinal_gs(data = mainTab, var_name = "physical_activity")
# education_ord <- me_ordinal_gs(data = mainTab, var_name = "education")
# SES_LEVEL_ord <- me_ordinal_gs(data = mainTab, var_name = "SES_LEVEL")
# 
# ord_var_test = list(bmi_ord = bmi_ord,
#                physical_activity_ord = physical_activity_ord,
#                education_ord = education_ord,
#                SES_LEVEL_ord = SES_LEVEL_ord)
# 
# save(ord_var, file = "Results/me_sim_ord_var.RData")

load("Results/me_sim_ord_var.RData")

set.seed(1234)
seeds <- sample(10000:10000000, size = 31)


set.seed(seeds[31])
data_ord <- me_ordinal_new(data = mainTab, ord_var = ord_var, n = nrow(mainTab))


########################
###   Main results   ###
########################

set.seed(seeds[1])

any_diabetes_me <- me_fun(data = mainTab, ord_var = data_ord, 
                          voi = "any_diabetes", voi_type = "Bin", 
                          me_type = "adj", me_range = me_mix,
                          adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(any_diabetes_me, file = "Results/measurement_real_adj/any_diabetes_me.RData")


set.seed(seeds[2])

any_cad_me <- me_fun(data = mainTab, ord_var = data_ord, 
                     voi = "any_cad", voi_type = "Bin", 
                     me_type = "adj", me_range = me_mix, 
                     adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(any_cad_me, file = "Results/measurement_real_adj/any_cad_me.RData")


set.seed(seeds[3])


BMXTHICR_me <- me_fun(data = mainTab, ord_var = data_ord, 
                      voi = "BMXTHICR", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, 
                      adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(BMXTHICR_me, file = "Results/measurement_real_adj/BMXTHICR_me.RData")


set.seed(seeds[4])


LBDHDL_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "LBDHDL", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBDHDL_me, file = "Results/measurement_real_adj/LBDHDL_me.RData")


################################
###   Supplemental results   ###
################################


set.seed(seeds[5])

any_ht_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "any_ht", voi_type = "Bin", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(any_ht_me, file = "Results/measurement_real_adj/any_ht_me.RData")



set.seed(seeds[6])

any_cancer_self_report_me <- me_fun(data = mainTab, ord_var = data_ord, 
                                    voi = "any_cancer_self_report", voi_type = "Bin", 
                                    me_type = "adj", me_range = me_mix, 
                                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(any_cancer_self_report_me, file = "Results/measurement_real_adj/any_cancer_self_report_me.RData")


set.seed(seeds[7])


any_family_cad_me <- me_fun(data = mainTab, ord_var = data_ord, 
                            voi = "any_family_cad", voi_type = "Bin", 
                            me_type = "adj", me_range = me_mix, 
                            adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(any_family_cad_me, file = "Results/measurement_real_adj/any_family_cad_me.RData")



set.seed(seeds[8])


LBDNENO_me <- me_fun(data = mainTab, ord_var = data_ord, 
                     voi = "LBDNENO", voi_type = "Num", 
                     me_type = "adj", me_range = me_mix, 
                     adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)


save(LBDNENO_me, file = "Results/measurement_real_adj/LBDNENO_me.RData")


set.seed(seeds[9])


BMXCALF_me <- me_fun(data = mainTab, ord_var = data_ord, 
                     voi = "BMXCALF", voi_type = "Num", 
                     me_type = "adj", me_range = me_mix, 
                     adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(BMXCALF_me, file = "Results/measurement_real_adj/BMXCALF_me.RData")




set.seed(seeds[10])


BMXHT_me <- me_fun(data = mainTab, ord_var = data_ord, 
                   voi = "BMXHT", voi_type = "Num", 
                   me_type = "adj", me_range = me_mix, 
                   adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(BMXHT_me, file = "Results/measurement_real_adj/BMXHT_me.RData")



set.seed(seeds[11])


BMXWAIST_me <- me_fun(data = mainTab, ord_var = data_ord, 
                      voi = "BMXWAIST", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, 
                      adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(BMXWAIST_me, file = "Results/measurement_real_adj/BMXWAIST_me.RData")



set.seed(seeds[12])


BMXWT_me <- me_fun(data = mainTab, ord_var = data_ord, 
                   voi = "BMXWT", voi_type = "Num", 
                   me_type = "adj", me_range = me_mix, 
                   adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(BMXWT_me, file = "Results/measurement_real_adj/BMXWT_me.RData")




set.seed(seeds[13])


BPXPLS_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "BPXPLS", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(BPXPLS_me, file = "Results/measurement_real_adj/BPXPLS_me.RData")


set.seed(seeds[14])


DSDCOUNT_me <- me_fun(data = mainTab, ord_var = data_ord, 
                      voi = "DSDCOUNT", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, 
                      adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(DSDCOUNT_me, file = "Results/measurement_real_adj/DSDCOUNT_me.RData")



set.seed(seeds[15])


LBXHA_me <- me_fun(data = mainTab, ord_var = data_ord, 
                   voi = "LBXHA", voi_type = "Bin", 
                   me_type = "adj", me_range = me_mix, 
                   adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXHA_me, file = "Results/measurement_real_adj/LBXHA_me.RData")


set.seed(seeds[16])


LBXHBC_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "LBXHBC", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXHBC_me, file = "Results/measurement_real_adj/LBXHBC_me.RData")



set.seed(seeds[17])


LBXHBS_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "LBXHBS", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXHBS_me, file = "Results/measurement_real_adj/LBXHBS_me.RData")



set.seed(seeds[18])


LBXLYPCT_me <- me_fun(data = mainTab, ord_var = data_ord, 
                      voi = "LBXLYPCT", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, 
                      adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXLYPCT_me, file = "Results/measurement_real_adj/LBXLYPCT_me.RData")


set.seed(seeds[19])


LBXMPSI_me <- me_fun(data = mainTab, ord_var = data_ord, 
                     voi = "LBXMPSI", voi_type = "Num", 
                     me_type = "adj", me_range = me_mix, 
                     adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXMPSI_me, file = "Results/measurement_real_adj/LBXMPSI_me.RData")


set.seed(seeds[20])


LBXSCA_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "LBXSCA", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXSCA_me, file = "Results/measurement_real_adj/LBXSCA_me.RData")



set.seed(seeds[21])


LBXSNASI_me <- me_fun(data = mainTab, ord_var = data_ord, 
                      voi = "LBXSNASI", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, 
                      adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXSNASI_me, file = "Results/measurement_real_adj/LBXSNASI_me.RData")


set.seed(seeds[22])


LBXSOSSI_me <- me_fun(data = mainTab, ord_var = data_ord, 
                      voi = "LBXSOSSI", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, 
                      adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXSOSSI_me, file = "Results/measurement_real_adj/LBXSOSSI_me.RData")



set.seed(seeds[23])


LBXSPH_me <- me_fun(data = mainTab, ord_var = data_ord, 
                    voi = "LBXSPH", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, 
                    adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(LBXSPH_me, file = "Results/measurement_real_adj/LBXSPH_me.RData")



set.seed(seeds[24])


male_me <- me_fun(data = mainTab, ord_var = data_ord, 
                  voi = "male", voi_type = "Bin", 
                  me_type = "adj", me_range = me_mix, 
                  adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(male_me, file = "Results/measurement_real_adj/male_me.RData")



set.seed(seeds[25])


pest_control_home_last_month_me <- me_fun(data = mainTab, ord_var = data_ord, 
                                          voi = "pest_control_home_last_month", voi_type = "Bin", 
                                          me_type = "adj", me_range = me_mix, 
                                          adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(pest_control_home_last_month_me, 
     file = "Results/measurement_real_adj/pest_control_home_last_month_me.RData")



set.seed(seeds[26])

pneu_me <- me_fun(data = mainTab, ord_var = data_ord, 
                  voi = "pneu", voi_type = "Bin", 
                  me_type = "adj", me_range = me_mix, 
                  adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(pneu_me, file = "Results/measurement_real_adj/pneu_me.RData")


set.seed(seeds[27])


private_water_source_me <- me_fun(data = mainTab, ord_var = data_ord, 
                                  voi = "private_water_source", voi_type = "Bin", 
                                  me_type = "adj", me_range = me_mix, 
                                  adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(private_water_source_me, file = "Results/measurement_real_adj/private_water_source_me.RData")



set.seed(seeds[28])


use_water_treatment_me <- me_fun(data = mainTab, ord_var = data_ord, 
                                 voi = "use_water_treatment", voi_type = "Bin", 
                                 me_type = "adj", me_range = me_mix, 
                                 adj_var = adj_var, adj_type = adj_type, B = 1000, print_progress = T)

save(use_water_treatment_me, file = "Results/measurement_real_adj/use_water_treatment_me.RData")



set.seed(seeds[29])


RIDAGEYR_me <- me_fun(data = mainTab, ord_var = data_ord, voi = "RIDAGEYR", voi_type = "Num", 
                      me_type = "adj", me_range = me_mix, adj_var = adj_var, adj_type = adj_type, 
                      B = 1000, print_progress = T)

save(RIDAGEYR_me, file = "Results/measurement_real_adj/RIDAGEYR_me.RData")



set.seed(seeds[30])


SMD410_me <- me_fun(data = mainTab, ord_var = data_ord, voi = "SMD410", voi_type = "Num", 
                    me_type = "adj", me_range = me_mix, adj_var = adj_var, adj_type = adj_type, 
                    B = 1000, print_progress = T)

save(SMD410_me, file = "Results/measurement_real_adj/SMD410_me.RData")