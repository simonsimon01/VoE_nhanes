# RHR for different sample sizes + barplots (Figures 5 - 8 + Supplementary Figures 27 - 52)

library(ggplot2)
library(ggmap)
library(dplyr)
library(cowplot)
theme_set(theme_grey())
library(grid)
library(ggplotify)
library(gridExtra)

#source("plot_functions.R")
source("IJE_Submission/R-Code/plot_functions.R")

############################
###   Find maximum ROR   ###
############################

# load("Results/measurement_sim/any_diabetes_me.RData")
# load("Results/sample_sim/any_diabetes.RData")
# load("Results/model_sim/mod_sim_any_diabetes.RData")
# 
# load("Results/measurement_sim/any_cad_me.RData")
# load("Results/sample_sim/any_cad.RData")
# load("Results/model_sim/mod_sim_any_cad.RData")
# 
# load("Results/measurement_sim/BMXTHICR_me.RData")
# load("Results/sample_sim/BMXTHICR.RData")
# load("Results/model_sim/mod_sim_BMXTHICR.RData")
# 
# load("Results/measurement_sim/LBDHDL_me.RData")
# load("Results/sample_sim/LBDHDL.RData")
# load("Results/model_sim/mod_sim_LBDHDL.RData")
# 
# RHR_500_any_diabetes_me <- mean(unlist(lapply(any_diabetes_me$vib_500, function(x) x$RHR)))
# RHR_500_any_diabetes_mod <- mean(unlist(lapply(mod_sim_any_diabetes$vib_500, function(x) x$RHR)))
# RHR_500_any_diabetes_sam <- mean(unlist(lapply(any_diabetes_05$vib_500_15, function(x) x$RHR)))
# 
# RHR_500_any_cad_me <- mean(unlist(lapply(any_cad_me$vib_500, function(x) x$RHR)))
# RHR_500_any_cad_mod <- mean(unlist(lapply(mod_sim_any_cad$vib_500, function(x) x$RHR)))
# RHR_500_any_cad_sam <- mean(unlist(lapply(any_cad_05$vib_500_15, function(x) x$RHR)))
# 
# RHR_500_BMXTHICR_me <- mean(unlist(lapply(BMXTHICR_me$vib_500, function(x) x$RHR)))
# RHR_500_BMXTHICR_mod <- mean(unlist(lapply(mod_sim_BMXTHICR$vib_500, function(x) x$RHR)))
# RHR_500_BMXTHICR_sam <- mean(unlist(lapply(BMXTHICR_05$vib_500_15, function(x) x$RHR)))
# 
# RHR_500_LBDHDL_me <- mean(unlist(lapply(LBDHDL_me$vib_500, function(x) x$RHR)))
# RHR_500_LBDHDL_mod <- mean(unlist(lapply(mod_sim_LBDHDL$vib_500, function(x) x$RHR)))
# RHR_500_LBDHDL_sam <- mean(unlist(lapply(LBDHDL_05$vib_500_15, function(x) x$RHR)))
# 
# 
# RHR_max_main <- max(c(RHR_500_any_diabetes_me, RHR_500_any_diabetes_mod,
#                       RHR_500_any_diabetes_sam, RHR_500_any_cad_me,
#                       RHR_500_any_cad_mod, RHR_500_any_cad_sam,
#                       RHR_500_BMXTHICR_me, RHR_500_BMXTHICR_mod,
#                       RHR_500_BMXTHICR_sam, RHR_500_LBDHDL_me,
#                       RHR_500_LBDHDL_mod, RHR_500_LBDHDL_sam))
# 
# save(RHR_max_main, file = "Results/RHR_max_main.RData")

########################
###   any_diabetes   ###
########################

load("Results/measurement_sim/any_diabetes_me.RData")
load("Results/sample_sim/any_diabetes.RData")
load("Results/model_sim/mod_sim_any_diabetes.RData")
load("Results/RHR_max_main.RData")

vibobj_me <- any_diabetes_me
vibobj_mod <- mod_sim_any_diabetes
vibobj_sam <- any_diabetes_05
RHR_max <- RHR_max_main


any_diabetes_sim_RHR <- sim_RHR(vibobj_me = any_diabetes_me, vibobj_mod = mod_sim_any_diabetes, 
                                vibobj_sam = any_diabetes_05, RHR_max = RHR_max)

a_bw <- as.grob(any_diabetes_sim_RHR$p1)
b1_bw <- as.grob(any_diabetes_sim_RHR$legend)
b2_bw <- as.grob(any_diabetes_sim_RHR$plot_500)
b3_bw <- as.grob(any_diabetes_sim_RHR$plot_1000)
b4_bw <- as.grob(any_diabetes_sim_RHR$plot_5000)
b5_bw <- as.grob(any_diabetes_sim_RHR$plot_10000)
b6_bw <- as.grob(any_diabetes_sim_RHR$plot_50000)
b7_bw <- as.grob(any_diabetes_sim_RHR$plot_100000)
b8_bw <- as.grob(any_diabetes_sim_RHR$plot_200000)


# Figure 5:

#pdf("Figures/voe_sim_col/any_diabetes_RHR_bar_bw.pdf", width = 10, height = 8)
tiff("Figures/voe_sim_col/any_diabetes_RHR_bar_bw.tiff", height = 800, width = 1000)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()




###################
###   any_cad   ###
###################


load("Results/measurement_sim/any_cad_me.RData")
load("Results/sample_sim/any_cad.RData")
load("Results/model_sim/mod_sim_any_cad.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- any_cad_me
vibobj_mod <- mod_sim_any_cad
vibobj_sam <- any_cad_05
RHR_max <- RHR_max_main


any_cad_sim_RHR <- sim_RHR(vibobj_me = any_cad_me, vibobj_mod = mod_sim_any_cad, 
                           vibobj_sam = any_cad_05, RHR_max = RHR_max)

a_bw <- as.grob(any_cad_sim_RHR$p1)
b1_bw <- as.grob(any_cad_sim_RHR$legend)
b2_bw <- as.grob(any_cad_sim_RHR$plot_500)
b3_bw <- as.grob(any_cad_sim_RHR$plot_1000)
b4_bw <- as.grob(any_cad_sim_RHR$plot_5000)
b5_bw <- as.grob(any_cad_sim_RHR$plot_10000)
b6_bw <- as.grob(any_cad_sim_RHR$plot_50000)
b7_bw <- as.grob(any_cad_sim_RHR$plot_100000)
b8_bw <- as.grob(any_cad_sim_RHR$plot_200000)

# Figure 6:

#pdf("Figures/voe_sim_col/any_cad_RHR_bar_bw.pdf", width = 10, height = 8)
tiff("Figures/voe_sim_col/any_cad_RHR_bar_bw.tiff", height = 800, width = 1000)


plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   BMXTHICR   ###
####################


load("Results/measurement_sim/BMXTHICR_me.RData")
load("Results/sample_sim/BMXTHICR.RData")
load("Results/model_sim/mod_sim_BMXTHICR.RData")
#load("Results/RHR_max_main.RData")


vibobj_me <- BMXTHICR_me
vibobj_mod <- mod_sim_BMXTHICR
vibobj_sam <- BMXTHICR_05
#RHR_max <- RHR_max_main

RHR_500_BMXTHICR_me <- mean(unlist(lapply(BMXTHICR_me$vib_500, function(x) x$RHR)))
RHR_500_BMXTHICR_mod <- mean(unlist(lapply(mod_sim_BMXTHICR$vib_500, function(x) x$RHR)))
RHR_500_BMXTHICR_sam <- mean(unlist(lapply(BMXTHICR_05$vib_500_15, function(x) x$RHR)))


RHR_max_BMXTHICR <- max(c(RHR_500_BMXTHICR_me, RHR_500_BMXTHICR_mod, RHR_500_BMXTHICR_sam))

BMXTHICR_sim_RHR <- sim_RHR(vibobj_me = BMXTHICR_me, vibobj_mod = mod_sim_BMXTHICR, 
                            vibobj_sam = BMXTHICR_05, RHR_max = RHR_max_BMXTHICR)

a_bw <- as.grob(BMXTHICR_sim_RHR$p1)
b1_bw <- as.grob(BMXTHICR_sim_RHR$legend)
b2_bw <- as.grob(BMXTHICR_sim_RHR$plot_500)
b3_bw <- as.grob(BMXTHICR_sim_RHR$plot_1000)
b4_bw <- as.grob(BMXTHICR_sim_RHR$plot_5000)
b5_bw <- as.grob(BMXTHICR_sim_RHR$plot_10000)
b6_bw <- as.grob(BMXTHICR_sim_RHR$plot_50000)
b7_bw <- as.grob(BMXTHICR_sim_RHR$plot_100000)
b8_bw <- as.grob(BMXTHICR_sim_RHR$plot_200000)

# Figure 7:

#pdf("Figures/voe_sim_col/BMXTHICR_RHR_bar_bw.pdf", width = 10, height = 8)
tiff("Figures/voe_sim_col/BMXTHICR_RHR_bar_bw.tiff", height = 800, width = 1000)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



##################
###   LBDHDL   ###
##################


load("Results/measurement_sim/LBDHDL_me.RData")
load("Results/sample_sim/LBDHDL.RData")
load("Results/model_sim/mod_sim_LBDHDL.RData")
#load("Results/RHR_max_main.RData")


vibobj_me <- LBDHDL_me
vibobj_mod <- mod_sim_LBDHDL
vibobj_sam <- LBDHDL_05
#RHR_max <- RHR_max_main


RHR_500_LBDHDL_me <- mean(unlist(lapply(LBDHDL_me$vib_500, function(x) x$RHR)))
RHR_500_LBDHDL_mod <- mean(unlist(lapply(mod_sim_LBDHDL$vib_500, function(x) x$RHR)))
RHR_500_LBDHDL_sam <- mean(unlist(lapply(LBDHDL_05$vib_500_15, function(x) x$RHR)))


RHR_max_LBDHDL <- max(c(RHR_500_LBDHDL_me, RHR_500_LBDHDL_mod, RHR_500_LBDHDL_sam))

LBDHDL_sim_RHR <- sim_RHR(vibobj_me = LBDHDL_me, vibobj_mod = mod_sim_LBDHDL, 
                          vibobj_sam = LBDHDL_05, RHR_max = RHR_max_LBDHDL)

a_bw <- as.grob(LBDHDL_sim_RHR$p1)
b1_bw <- as.grob(LBDHDL_sim_RHR$legend)
b2_bw <- as.grob(LBDHDL_sim_RHR$plot_500)
b3_bw <- as.grob(LBDHDL_sim_RHR$plot_1000)
b4_bw <- as.grob(LBDHDL_sim_RHR$plot_5000)
b5_bw <- as.grob(LBDHDL_sim_RHR$plot_10000)
b6_bw <- as.grob(LBDHDL_sim_RHR$plot_50000)
b7_bw <- as.grob(LBDHDL_sim_RHR$plot_100000)
b8_bw <- as.grob(LBDHDL_sim_RHR$plot_200000)

# Figure 8:

#pdf("Figures/voe_sim_col/LBDHDL_RHR_bar_bw.pdf", width = 10, height = 8)
tiff("Figures/voe_sim_col/LBDHDL_RHR_bar_bw.tiff", height = 800, width = 1000)


plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))

dev.off()


##################
###   any_ht   ###
##################


load("Results/measurement_sim/any_ht_me.RData")
load("Results/sample_sim/any_ht.RData")
load("Results/model_sim/mod_sim_any_ht.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- any_ht_me
vibobj_mod <- mod_sim_any_ht
vibobj_sam <- any_ht_05


RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


any_ht_sim_RHR <- sim_RHR(vibobj_me = any_ht_me, vibobj_mod = mod_sim_any_ht, 
                          vibobj_sam = any_ht_05, RHR_max = RHR_max)

a_bw <- as.grob(any_ht_sim_RHR$p1)
b1_bw <- as.grob(any_ht_sim_RHR$legend)
b2_bw <- as.grob(any_ht_sim_RHR$plot_500)
b3_bw <- as.grob(any_ht_sim_RHR$plot_1000)
b4_bw <- as.grob(any_ht_sim_RHR$plot_5000)
b5_bw <- as.grob(any_ht_sim_RHR$plot_10000)
b6_bw <- as.grob(any_ht_sim_RHR$plot_50000)
b7_bw <- as.grob(any_ht_sim_RHR$plot_100000)
b8_bw <- as.grob(any_ht_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/any_ht_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()


##################################
###   any_cancer_self_report   ###
##################################


load("Results/measurement_sim/any_cancer_self_report_me.RData")
load("Results/sample_sim/any_cancer_self_report.RData")
load("Results/model_sim/mod_sim_any_cancer_self_report.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- any_cancer_self_report_me
vibobj_mod <- mod_sim_any_cancer_self_report
vibobj_sam <- any_cancer_self_report_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


any_cancer_self_report_sim_RHR <- sim_RHR(vibobj_me = any_cancer_self_report_me, vibobj_mod = mod_sim_any_cancer_self_report, 
                                          vibobj_sam = any_cancer_self_report_05, RHR_max = RHR_max)

a_bw <- as.grob(any_cancer_self_report_sim_RHR$p1)
b1_bw <- as.grob(any_cancer_self_report_sim_RHR$legend)
b2_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_500)
b3_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_1000)
b4_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_5000)
b5_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_10000)
b6_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_50000)
b7_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_100000)
b8_bw <- as.grob(any_cancer_self_report_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/any_cancer_self_report_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()


##########################
###   any_family_cad   ###
##########################


load("Results/measurement_sim/any_family_cad_me.RData")
load("Results/sample_sim/any_family_cad.RData")
load("Results/model_sim/mod_sim_any_family_cad.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- any_family_cad_me
vibobj_mod <- mod_sim_any_family_cad
vibobj_sam <- any_family_cad_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


any_family_cad_sim_RHR <- sim_RHR(vibobj_me = any_family_cad_me, vibobj_mod = mod_sim_any_family_cad,
                                  vibobj_sam = any_family_cad_05, RHR_max = RHR_max)

a_bw <- as.grob(any_family_cad_sim_RHR$p1)
b1_bw <- as.grob(any_family_cad_sim_RHR$legend)
b2_bw <- as.grob(any_family_cad_sim_RHR$plot_500)
b3_bw <- as.grob(any_family_cad_sim_RHR$plot_1000)
b4_bw <- as.grob(any_family_cad_sim_RHR$plot_5000)
b5_bw <- as.grob(any_family_cad_sim_RHR$plot_10000)
b6_bw <- as.grob(any_family_cad_sim_RHR$plot_50000)
b7_bw <- as.grob(any_family_cad_sim_RHR$plot_100000)
b8_bw <- as.grob(any_family_cad_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/any_family_cad_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()


###################
###   LBDNENO   ###
###################


load("Results/measurement_sim/LBDNENO_me.RData")
load("Results/sample_sim/LBDNENO.RData")
load("Results/model_sim/mod_sim_LBDNENO.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBDNENO_me
vibobj_mod <- mod_sim_LBDNENO
vibobj_sam <- LBDNENO_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBDNENO_sim_RHR <- sim_RHR(vibobj_me = LBDNENO_me, vibobj_mod = mod_sim_LBDNENO, 
                           vibobj_sam = LBDNENO_05, RHR_max = RHR_max)

a_bw <- as.grob(LBDNENO_sim_RHR$p1)
b1_bw <- as.grob(LBDNENO_sim_RHR$legend)
b2_bw <- as.grob(LBDNENO_sim_RHR$plot_500)
b3_bw <- as.grob(LBDNENO_sim_RHR$plot_1000)
b4_bw <- as.grob(LBDNENO_sim_RHR$plot_5000)
b5_bw <- as.grob(LBDNENO_sim_RHR$plot_10000)
b6_bw <- as.grob(LBDNENO_sim_RHR$plot_50000)
b7_bw <- as.grob(LBDNENO_sim_RHR$plot_100000)
b8_bw <- as.grob(LBDNENO_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBDNENO_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



###################
###   BMXCALF   ###
###################


load("Results/measurement_sim/BMXCALF_me.RData")
load("Results/sample_sim/BMXCALF.RData")
load("Results/model_sim/mod_sim_BMXCALF.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- BMXCALF_me
vibobj_mod <- mod_sim_BMXCALF
vibobj_sam <- BMXCALF_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


BMXCALF_sim_RHR <- sim_RHR(vibobj_me = BMXCALF_me, vibobj_mod = mod_sim_BMXCALF, 
                           vibobj_sam = BMXCALF_05, RHR_max = RHR_max)

a_bw <- as.grob(BMXCALF_sim_RHR$p1)
b1_bw <- as.grob(BMXCALF_sim_RHR$legend)
b2_bw <- as.grob(BMXCALF_sim_RHR$plot_500)
b3_bw <- as.grob(BMXCALF_sim_RHR$plot_1000)
b4_bw <- as.grob(BMXCALF_sim_RHR$plot_5000)
b5_bw <- as.grob(BMXCALF_sim_RHR$plot_10000)
b6_bw <- as.grob(BMXCALF_sim_RHR$plot_50000)
b7_bw <- as.grob(BMXCALF_sim_RHR$plot_100000)
b8_bw <- as.grob(BMXCALF_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/BMXCALF_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



#################
###   BMXHT   ###
#################


load("Results/measurement_sim/BMXHT_me.RData")
load("Results/sample_sim/BMXHT.RData")
load("Results/model_sim/mod_sim_BMXHT.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- BMXHT_me
vibobj_mod <- mod_sim_BMXHT
vibobj_sam <- BMXHT_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


BMXHT_sim_RHR <- sim_RHR(vibobj_me = BMXHT_me, vibobj_mod = mod_sim_BMXHT, 
                         vibobj_sam = BMXHT_05, RHR_max = RHR_max)

a_bw <- as.grob(BMXHT_sim_RHR$p1)
b1_bw <- as.grob(BMXHT_sim_RHR$legend)
b2_bw <- as.grob(BMXHT_sim_RHR$plot_500)
b3_bw <- as.grob(BMXHT_sim_RHR$plot_1000)
b4_bw <- as.grob(BMXHT_sim_RHR$plot_5000)
b5_bw <- as.grob(BMXHT_sim_RHR$plot_10000)
b6_bw <- as.grob(BMXHT_sim_RHR$plot_50000)
b7_bw <- as.grob(BMXHT_sim_RHR$plot_100000)
b8_bw <- as.grob(BMXHT_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/BMXHT_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   BMXWAIST   ###
####################


load("Results/measurement_sim/BMXWAIST_me.RData")
load("Results/sample_sim/BMXWAIST.RData")
load("Results/model_sim/mod_sim_BMXWAIST.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- BMXWAIST_me
vibobj_mod <- mod_sim_BMXWAIST
vibobj_sam <- BMXWAIST_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


BMXWAIST_sim_RHR <- sim_RHR(vibobj_me = BMXWAIST_me, vibobj_mod = mod_sim_BMXWAIST, 
                            vibobj_sam = BMXWAIST_05, RHR_max = RHR_max)

a_bw <- as.grob(BMXWAIST_sim_RHR$p1)
b1_bw <- as.grob(BMXWAIST_sim_RHR$legend)
b2_bw <- as.grob(BMXWAIST_sim_RHR$plot_500)
b3_bw <- as.grob(BMXWAIST_sim_RHR$plot_1000)
b4_bw <- as.grob(BMXWAIST_sim_RHR$plot_5000)
b5_bw <- as.grob(BMXWAIST_sim_RHR$plot_10000)
b6_bw <- as.grob(BMXWAIST_sim_RHR$plot_50000)
b7_bw <- as.grob(BMXWAIST_sim_RHR$plot_100000)
b8_bw <- as.grob(BMXWAIST_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/BMXWAIST_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



#################
###   BMXWT   ###
#################


load("Results/measurement_sim/BMXWT_me.RData")
load("Results/sample_sim/BMXWT.RData")
load("Results/model_sim/mod_sim_BMXWT.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- BMXWT_me
vibobj_mod <- mod_sim_BMXWT
vibobj_sam <- BMXWT_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


BMXWT_sim_RHR <- sim_RHR(vibobj_me = BMXWT_me, vibobj_mod = mod_sim_BMXWT, 
                         vibobj_sam = BMXWT_05, RHR_max = RHR_max)

a_bw <- as.grob(BMXWT_sim_RHR$p1)
b1_bw <- as.grob(BMXWT_sim_RHR$legend)
b2_bw <- as.grob(BMXWT_sim_RHR$plot_500)
b3_bw <- as.grob(BMXWT_sim_RHR$plot_1000)
b4_bw <- as.grob(BMXWT_sim_RHR$plot_5000)
b5_bw <- as.grob(BMXWT_sim_RHR$plot_10000)
b6_bw <- as.grob(BMXWT_sim_RHR$plot_50000)
b7_bw <- as.grob(BMXWT_sim_RHR$plot_100000)
b8_bw <- as.grob(BMXWT_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/BMXWT_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



##################
###   BPXPLS   ###
##################


load("Results/measurement_sim/BPXPLS_me.RData")
load("Results/sample_sim/BPXPLS.RData")
load("Results/model_sim/mod_sim_BPXPLS.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- BPXPLS_me
vibobj_mod <- mod_sim_BPXPLS
vibobj_sam <- BPXPLS_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


BPXPLS_sim_RHR <- sim_RHR(vibobj_me = BPXPLS_me, vibobj_mod = mod_sim_BPXPLS, 
                          vibobj_sam = BPXPLS_05, RHR_max = RHR_max)

a_bw <- as.grob(BPXPLS_sim_RHR$p1)
b1_bw <- as.grob(BPXPLS_sim_RHR$legend)
b2_bw <- as.grob(BPXPLS_sim_RHR$plot_500)
b3_bw <- as.grob(BPXPLS_sim_RHR$plot_1000)
b4_bw <- as.grob(BPXPLS_sim_RHR$plot_5000)
b5_bw <- as.grob(BPXPLS_sim_RHR$plot_10000)
b6_bw <- as.grob(BPXPLS_sim_RHR$plot_50000)
b7_bw <- as.grob(BPXPLS_sim_RHR$plot_100000)
b8_bw <- as.grob(BPXPLS_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/BPXPLS_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   DSDCOUNT   ###
####################


load("Results/measurement_sim/DSDCOUNT_me.RData")
load("Results/sample_sim/DSDCOUNT.RData")
load("Results/model_sim/mod_sim_DSDCOUNT.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- DSDCOUNT_me
vibobj_mod <- mod_sim_DSDCOUNT
vibobj_sam <- DSDCOUNT_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


DSDCOUNT_sim_RHR <- sim_RHR(vibobj_me = DSDCOUNT_me, vibobj_mod = mod_sim_DSDCOUNT, 
                            vibobj_sam = DSDCOUNT_05, RHR_max = RHR_max)

a_bw <- as.grob(DSDCOUNT_sim_RHR$p1)
b1_bw <- as.grob(DSDCOUNT_sim_RHR$legend)
b2_bw <- as.grob(DSDCOUNT_sim_RHR$plot_500)
b3_bw <- as.grob(DSDCOUNT_sim_RHR$plot_1000)
b4_bw <- as.grob(DSDCOUNT_sim_RHR$plot_5000)
b5_bw <- as.grob(DSDCOUNT_sim_RHR$plot_10000)
b6_bw <- as.grob(DSDCOUNT_sim_RHR$plot_50000)
b7_bw <- as.grob(DSDCOUNT_sim_RHR$plot_100000)
b8_bw <- as.grob(DSDCOUNT_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/DSDCOUNT_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()


#################
###   LBXHA   ###
#################


load("Results/measurement_sim/LBXHA_me.RData")
load("Results/sample_sim/LBXHA.RData")
load("Results/model_sim/mod_sim_LBXHA.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXHA_me
vibobj_mod <- mod_sim_LBXHA
vibobj_sam <- LBXHA_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXHA_sim_RHR <- sim_RHR(vibobj_me = LBXHA_me, vibobj_mod = mod_sim_LBXHA, 
                         vibobj_sam = LBXHA_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXHA_sim_RHR$p1)
b1_bw <- as.grob(LBXHA_sim_RHR$legend)
b2_bw <- as.grob(LBXHA_sim_RHR$plot_500)
b3_bw <- as.grob(LBXHA_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXHA_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXHA_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXHA_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXHA_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXHA_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXHA_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



##################
###   LBXHBC   ###
##################


load("Results/measurement_sim/LBXHBC_me.RData")
load("Results/sample_sim/LBXHBC.RData")
load("Results/model_sim/mod_sim_LBXHBC.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXHBC_me
vibobj_mod <- mod_sim_LBXHBC
vibobj_sam <- LBXHBC_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXHBC_sim_RHR <- sim_RHR(vibobj_me = LBXHBC_me, vibobj_mod = mod_sim_LBXHBC, 
                          vibobj_sam = LBXHBC_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXHBC_sim_RHR$p1)
b1_bw <- as.grob(LBXHBC_sim_RHR$legend)
b2_bw <- as.grob(LBXHBC_sim_RHR$plot_500)
b3_bw <- as.grob(LBXHBC_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXHBC_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXHBC_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXHBC_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXHBC_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXHBC_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXHBC_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()


##################
###   LBXHBS   ###
##################


load("Results/measurement_sim/LBXHBS_me.RData")
load("Results/sample_sim/LBXHBS.RData")
load("Results/model_sim/mod_sim_LBXHBS.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXHBS_me
vibobj_mod <- mod_sim_LBXHBS
vibobj_sam <- LBXHBS_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXHBS_sim_RHR <- sim_RHR(vibobj_me = LBXHBS_me, vibobj_mod = mod_sim_LBXHBS, 
                          vibobj_sam = LBXHBS_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXHBS_sim_RHR$p1)
b1_bw <- as.grob(LBXHBS_sim_RHR$legend)
b2_bw <- as.grob(LBXHBS_sim_RHR$plot_500)
b3_bw <- as.grob(LBXHBS_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXHBS_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXHBS_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXHBS_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXHBS_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXHBS_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXHBS_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   LBXLYPCT   ###
####################


load("Results/measurement_sim/LBXLYPCT_me.RData")
load("Results/sample_sim/LBXLYPCT.RData")
load("Results/model_sim/mod_sim_LBXLYPCT.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXLYPCT_me
vibobj_mod <- mod_sim_LBXLYPCT
vibobj_sam <- LBXLYPCT_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXLYPCT_sim_RHR <- sim_RHR(vibobj_me = LBXLYPCT_me, vibobj_mod = mod_sim_LBXLYPCT, 
                            vibobj_sam = LBXLYPCT_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXLYPCT_sim_RHR$p1)
b1_bw <- as.grob(LBXLYPCT_sim_RHR$legend)
b2_bw <- as.grob(LBXLYPCT_sim_RHR$plot_500)
b3_bw <- as.grob(LBXLYPCT_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXLYPCT_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXLYPCT_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXLYPCT_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXLYPCT_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXLYPCT_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXLYPCT_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



###################
###   LBXMPSI   ###
###################


load("Results/measurement_sim/LBXMPSI_me.RData")
load("Results/sample_sim/LBXMPSI.RData")
load("Results/model_sim/mod_sim_LBXMPSI.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXMPSI_me
vibobj_mod <- mod_sim_LBXMPSI
vibobj_sam <- LBXMPSI_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXMPSI_sim_RHR <- sim_RHR(vibobj_me = LBXMPSI_me, vibobj_mod = mod_sim_LBXMPSI, 
                           vibobj_sam = LBXMPSI_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXMPSI_sim_RHR$p1)
b1_bw <- as.grob(LBXMPSI_sim_RHR$legend)
b2_bw <- as.grob(LBXMPSI_sim_RHR$plot_500)
b3_bw <- as.grob(LBXMPSI_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXMPSI_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXMPSI_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXMPSI_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXMPSI_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXMPSI_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXMPSI_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



##################
###   LBXSCA   ###
##################


load("Results/measurement_sim/LBXSCA_me.RData")
load("Results/sample_sim/LBXSCA.RData")
load("Results/model_sim/mod_sim_LBXSCA.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXSCA_me
vibobj_mod <- mod_sim_LBXSCA
vibobj_sam <- LBXSCA_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXSCA_sim_RHR <- sim_RHR(vibobj_me = LBXSCA_me, vibobj_mod = mod_sim_LBXSCA, 
                          vibobj_sam = LBXSCA_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXSCA_sim_RHR$p1)
b1_bw <- as.grob(LBXSCA_sim_RHR$legend)
b2_bw <- as.grob(LBXSCA_sim_RHR$plot_500)
b3_bw <- as.grob(LBXSCA_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXSCA_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXSCA_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXSCA_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXSCA_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXSCA_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXSCA_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   LBXSNASI   ###
####################


load("Results/measurement_sim/LBXSNASI_me.RData")
load("Results/sample_sim/LBXSNASI.RData")
load("Results/model_sim/mod_sim_LBXSNASI.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXSNASI_me
vibobj_mod <- mod_sim_LBXSNASI
vibobj_sam <- LBXSNASI_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXSNASI_sim_RHR <- sim_RHR(vibobj_me = LBXSNASI_me, vibobj_mod = mod_sim_LBXSNASI, 
                            vibobj_sam = LBXSNASI_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXSNASI_sim_RHR$p1)
b1_bw <- as.grob(LBXSNASI_sim_RHR$legend)
b2_bw <- as.grob(LBXSNASI_sim_RHR$plot_500)
b3_bw <- as.grob(LBXSNASI_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXSNASI_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXSNASI_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXSNASI_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXSNASI_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXSNASI_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXSNASI_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   LBXSOSSI   ###
####################


load("Results/measurement_sim/LBXSOSSI_me.RData")
load("Results/sample_sim/LBXSOSSI.RData")
load("Results/model_sim/mod_sim_LBXSOSSI.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXSOSSI_me
vibobj_mod <- mod_sim_LBXSOSSI
vibobj_sam <- LBXSOSSI_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXSOSSI_sim_RHR <- sim_RHR(vibobj_me = LBXSOSSI_me, vibobj_mod = mod_sim_LBXSOSSI, 
                            vibobj_sam = LBXSOSSI_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXSOSSI_sim_RHR$p1)
b1_bw <- as.grob(LBXSOSSI_sim_RHR$legend)
b2_bw <- as.grob(LBXSOSSI_sim_RHR$plot_500)
b3_bw <- as.grob(LBXSOSSI_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXSOSSI_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXSOSSI_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXSOSSI_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXSOSSI_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXSOSSI_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXSOSSI_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



##################
###   LBXSPH   ###
##################


load("Results/measurement_sim/LBXSPH_me.RData")
load("Results/sample_sim/LBXSPH.RData")
load("Results/model_sim/mod_sim_LBXSPH.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- LBXSPH_me
vibobj_mod <- mod_sim_LBXSPH
vibobj_sam <- LBXSPH_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


LBXSPH_sim_RHR <- sim_RHR(vibobj_me = LBXSPH_me, vibobj_mod = mod_sim_LBXSPH, 
                          vibobj_sam = LBXSPH_05, RHR_max = RHR_max)

a_bw <- as.grob(LBXSPH_sim_RHR$p1)
b1_bw <- as.grob(LBXSPH_sim_RHR$legend)
b2_bw <- as.grob(LBXSPH_sim_RHR$plot_500)
b3_bw <- as.grob(LBXSPH_sim_RHR$plot_1000)
b4_bw <- as.grob(LBXSPH_sim_RHR$plot_5000)
b5_bw <- as.grob(LBXSPH_sim_RHR$plot_10000)
b6_bw <- as.grob(LBXSPH_sim_RHR$plot_50000)
b7_bw <- as.grob(LBXSPH_sim_RHR$plot_100000)
b8_bw <- as.grob(LBXSPH_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/LBXSPH_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



################
###   male   ###
################


load("Results/measurement_sim/male_me.RData")
load("Results/sample_sim/male.RData")
load("Results/model_sim/mod_sim_male.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- male_me
vibobj_mod <- mod_sim_male
vibobj_sam <- male_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


male_sim_RHR <- sim_RHR(vibobj_me = male_me, vibobj_mod = mod_sim_male, 
                        vibobj_sam = male_05, RHR_max = RHR_max)

a_bw <- as.grob(male_sim_RHR$p1)
b1_bw <- as.grob(male_sim_RHR$legend)
b2_bw <- as.grob(male_sim_RHR$plot_500)
b3_bw <- as.grob(male_sim_RHR$plot_1000)
b4_bw <- as.grob(male_sim_RHR$plot_5000)
b5_bw <- as.grob(male_sim_RHR$plot_10000)
b6_bw <- as.grob(male_sim_RHR$plot_50000)
b7_bw <- as.grob(male_sim_RHR$plot_100000)
b8_bw <- as.grob(male_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/male_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



########################################
###   pest_control_home_last_month   ###
########################################


load("Results/measurement_sim/pest_control_home_last_month_me.RData")
load("Results/sample_sim/pest_control_home_last_month.RData")
load("Results/model_sim/mod_sim_pest_control_home_last_month.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- pest_control_home_last_month_me
vibobj_mod <- mod_sim_pest_control_home_last_month
vibobj_sam <- pest_control_home_last_month_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


pest_control_home_last_month_sim_RHR <- sim_RHR(vibobj_me = pest_control_home_last_month_me, vibobj_mod = mod_sim_pest_control_home_last_month, 
                                                vibobj_sam = pest_control_home_last_month_05, RHR_max = RHR_max)

a_bw <- as.grob(pest_control_home_last_month_sim_RHR$p1)
b1_bw <- as.grob(pest_control_home_last_month_sim_RHR$legend)
b2_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_500)
b3_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_1000)
b4_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_5000)
b5_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_10000)
b6_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_50000)
b7_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_100000)
b8_bw <- as.grob(pest_control_home_last_month_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/pest_control_home_last_month_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



################
###   pneu   ###
################


load("Results/measurement_sim/pneu_me.RData")
load("Results/sample_sim/pneu.RData")
load("Results/model_sim/mod_sim_pneu.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- pneu_me
vibobj_mod <- mod_sim_pneu
vibobj_sam <- pneu_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


pneu_sim_RHR <- sim_RHR(vibobj_me = pneu_me, vibobj_mod = mod_sim_pneu, 
                        vibobj_sam = pneu_05, RHR_max = RHR_max)

a_bw <- as.grob(pneu_sim_RHR$p1)
b1_bw <- as.grob(pneu_sim_RHR$legend)
b2_bw <- as.grob(pneu_sim_RHR$plot_500)
b3_bw <- as.grob(pneu_sim_RHR$plot_1000)
b4_bw <- as.grob(pneu_sim_RHR$plot_5000)
b5_bw <- as.grob(pneu_sim_RHR$plot_10000)
b6_bw <- as.grob(pneu_sim_RHR$plot_50000)
b7_bw <- as.grob(pneu_sim_RHR$plot_100000)
b8_bw <- as.grob(pneu_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/pneu_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



################################
###   private_water_source   ###
################################


load("Results/measurement_sim/private_water_source_me.RData")
load("Results/sample_sim/private_water_source.RData")
load("Results/model_sim/mod_sim_private_water_source.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- private_water_source_me
vibobj_mod <- mod_sim_private_water_source
vibobj_sam <- private_water_source_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


private_water_source_sim_RHR <- sim_RHR(vibobj_me = private_water_source_me, vibobj_mod = mod_sim_private_water_source, 
                                        vibobj_sam = private_water_source_05, RHR_max = RHR_max)

a_bw <- as.grob(private_water_source_sim_RHR$p1)
b1_bw <- as.grob(private_water_source_sim_RHR$legend)
b2_bw <- as.grob(private_water_source_sim_RHR$plot_500)
b3_bw <- as.grob(private_water_source_sim_RHR$plot_1000)
b4_bw <- as.grob(private_water_source_sim_RHR$plot_5000)
b5_bw <- as.grob(private_water_source_sim_RHR$plot_10000)
b6_bw <- as.grob(private_water_source_sim_RHR$plot_50000)
b7_bw <- as.grob(private_water_source_sim_RHR$plot_100000)
b8_bw <- as.grob(private_water_source_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/private_water_source_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



###############################
###   use_water_treatment   ###
###############################


load("Results/measurement_sim/use_water_treatment_me.RData")
load("Results/sample_sim/use_water_treatment.RData")
load("Results/model_sim/mod_sim_use_water_treatment.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- use_water_treatment_me
vibobj_mod <- mod_sim_use_water_treatment
vibobj_sam <- use_water_treatment_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


use_water_treatment_sim_RHR <- sim_RHR(vibobj_me = use_water_treatment_me, vibobj_mod = mod_sim_use_water_treatment, 
                                       vibobj_sam = use_water_treatment_05, RHR_max = RHR_max)

a_bw <- as.grob(use_water_treatment_sim_RHR$p1)
b1_bw <- as.grob(use_water_treatment_sim_RHR$legend)
b2_bw <- as.grob(use_water_treatment_sim_RHR$plot_500)
b3_bw <- as.grob(use_water_treatment_sim_RHR$plot_1000)
b4_bw <- as.grob(use_water_treatment_sim_RHR$plot_5000)
b5_bw <- as.grob(use_water_treatment_sim_RHR$plot_10000)
b6_bw <- as.grob(use_water_treatment_sim_RHR$plot_50000)
b7_bw <- as.grob(use_water_treatment_sim_RHR$plot_100000)
b8_bw <- as.grob(use_water_treatment_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/use_water_treatment_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



####################
###   RIDAGEYR   ###
####################


load("Results/measurement_sim/RIDAGEYR_me.RData")
load("Results/sample_sim/RIDAGEYR.RData")
load("Results/model_sim/mod_sim_RIDAGEYR.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- RIDAGEYR_me
vibobj_mod <- mod_sim_RIDAGEYR
vibobj_sam <- RIDAGEYR_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


RIDAGEYR_sim_RHR <- sim_RHR(vibobj_me = RIDAGEYR_me, vibobj_mod = mod_sim_RIDAGEYR, 
                            vibobj_sam = RIDAGEYR_05, RHR_max = RHR_max)

a_bw <- as.grob(RIDAGEYR_sim_RHR$p1)
b1_bw <- as.grob(RIDAGEYR_sim_RHR$legend)
b2_bw <- as.grob(RIDAGEYR_sim_RHR$plot_500)
b3_bw <- as.grob(RIDAGEYR_sim_RHR$plot_1000)
b4_bw <- as.grob(RIDAGEYR_sim_RHR$plot_5000)
b5_bw <- as.grob(RIDAGEYR_sim_RHR$plot_10000)
b6_bw <- as.grob(RIDAGEYR_sim_RHR$plot_50000)
b7_bw <- as.grob(RIDAGEYR_sim_RHR$plot_100000)
b8_bw <- as.grob(RIDAGEYR_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/RIDAGEYR_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()



##################
###   SMD410   ###
##################


load("Results/measurement_sim/SMD410_me.RData")
load("Results/sample_sim/SMD410.RData")
load("Results/model_sim/mod_sim_SMD410.RData")
load("Results/RHR_max_main.RData")


vibobj_me <- SMD410_me
vibobj_mod <- mod_sim_SMD410
vibobj_sam <- SMD410_05
RHR_max <- max(c(mean(unlist(lapply(vibobj_me$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_mod$vib_500, FUN = function(x)  x$RHR))),
                 mean(unlist(lapply(vibobj_sam$vib_500_15, FUN = function(x)  x$RHR)))))


SMD410_sim_RHR <- sim_RHR(vibobj_me = SMD410_me, vibobj_mod = mod_sim_SMD410, 
                          vibobj_sam = SMD410_05, RHR_max = RHR_max)

a_bw <- as.grob(SMD410_sim_RHR$p1)
b1_bw <- as.grob(SMD410_sim_RHR$legend)
b2_bw <- as.grob(SMD410_sim_RHR$plot_500)
b3_bw <- as.grob(SMD410_sim_RHR$plot_1000)
b4_bw <- as.grob(SMD410_sim_RHR$plot_5000)
b5_bw <- as.grob(SMD410_sim_RHR$plot_10000)
b6_bw <- as.grob(SMD410_sim_RHR$plot_50000)
b7_bw <- as.grob(SMD410_sim_RHR$plot_100000)
b8_bw <- as.grob(SMD410_sim_RHR$plot_200000)

pdf("Figures/voe_sim_col/SMD410_RHR_bar_bw.pdf", width = 10, height = 8)

plot1 <- grid.arrange(grobs = list(a_bw, b1_bw, b2_bw, b3_bw, b4_bw, b5_bw, b6_bw, b7_bw, b8_bw),
                      heights=c(3,2),
                      layout_matrix = rbind(c(NA, NA, NA, 1, 1,1,1,1,1,1,1, 1,1,1, 1, 1, 1, 1,1,1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1, 1),
                                            c(NA, NA, 2,2, NA, NA, 3,3,3, NA, 4,4,4, NA, 5,5,5, NA, 6,6,6, NA, 7,7,7, NA, 8,8,8, NA, 9, 9, 9, NA)))


dev.off()