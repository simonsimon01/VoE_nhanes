# Volcano plots for real data results (Figures 1 - 4 + Supplementary Figures 1 - 26)

library(ggmap)
library(dplyr)
library(cowplot)
library(grid)
library(ggplotify)
library(gridExtra)

#setwd("IJE_Submission/R-Code/")

source("IJE_Submission/R-Code/plot_functions.R")

########################
###   any_diabetes   ###
########################


load("Results/model_real/mod_real_any_diabetes.RData")
load("Results/sample_real/any_diabetes_sam_05.RData")

load("Results/measurement_real_voi/any_diabetes_me.RData")
any_diabetes_me_voi <- any_diabetes_me
load("Results/measurement_real_adj/any_diabetes_me.RData")
any_diabetes_me_adj <- any_diabetes_me
load("Results/measurement_real/any_diabetes_me.RData")

yrange <- range(c(-log10(.05), 
                   -log10(mod_real_any_diabetes$vibFrame$pvalue),
                   -log10(any_diabetes_sam_05_full$results$pvalue)
                   -log10(any_diabetes_me$results$pvalue),
                  -log10(any_diabetes_me_adj$results$pvalue),
                  -log10(any_diabetes_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_any_diabetes$vibFrame$HR,
                  any_diabetes_sam_05_full$results$HR,
                  any_diabetes_me$results$HR,
                  any_diabetes_me_adj$results$HR,
                  any_diabetes_me_voi$results$HR), na.rm = T)


  
  # measurement error
 me_plot <-  vib2d_real_color(vibObj = any_diabetes_me, legend_position = c(0.3, 0.7), 
                        y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")
  
 
 # measurement error adjustment
 me_adj_plot <-  vib2d_real_color(vibObj = any_diabetes_me_adj, legend_position = c(0.3, 0.7), 
                            y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")
 
 
 # measurement error variable of interest
 me_voi_plot <-  vib2d_real_color(vibObj = any_diabetes_me_voi, legend_position = c(0.3, 0.7), 
                            y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")
 
  
  # model
  model_plot <- vib2d_real_color(vibObj = mod_real_any_diabetes, legend_position = c(0.8, 0.7), 
                      y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                      true_beta = F)
  
  # sample
  sample_plot <- vib2d_real_color(vibObj = any_diabetes_sam_05_full, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                       true_beta = F)
  


a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))
         
# Figure 1:
      
#pdf("Figures/voe_pattern_real_color/any_diabetes.pdf", height = 9, width = 13)
tiff("Figures/voe_pattern_real_color/any_diabetes.tiff", height = 700, width = 1000)
grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()




###################
###   any_cad   ###
###################


load("Results/model_real/mod_real_any_cad.RData")
load("Results/sample_real/any_cad_sam_05.RData")

load("Results/measurement_real_voi/any_cad_me.RData")
any_cad_me_voi <- any_cad_me
load("Results/measurement_real_adj/any_cad_me.RData")
any_cad_me_adj <- any_cad_me
load("Results/measurement_real/any_cad_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_any_cad$vibFrame$pvalue),
                  -log10(any_cad_sam_05_full$results$pvalue)
                  -log10(any_cad_me$results$pvalue),
                  -log10(any_cad_me_adj$results$pvalue),
                  -log10(any_cad_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_any_cad$vibFrame$HR,
                  any_cad_sam_05_full$results$HR,
                  any_cad_me$results$HR,
                  any_cad_me_adj$results$HR,
                  any_cad_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = any_cad_me, legend_position = c(0.8, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = any_cad_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = any_cad_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_any_cad,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = any_cad_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

# Figure 2:

#pdf("Figures/voe_pattern_real_color/any_cad.pdf", height = 9, width = 13)
tiff("Figures/voe_pattern_real_color/any_cad.tiff", height = 700, width = 1000)
grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



####################
###   BMXTHICR   ###
####################


load("Results/model_real/mod_real_BMXTHICR.RData")
load("Results/sample_real/BMXTHICR_sam_05.RData")

load("Results/measurement_real_voi/BMXTHICR_me.RData")
BMXTHICR_me_voi <- BMXTHICR_me
load("Results/measurement_real_adj/BMXTHICR_me.RData")
BMXTHICR_me_adj <- BMXTHICR_me
load("Results/measurement_real/BMXTHICR_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_BMXTHICR$vibFrame$pvalue),
                  -log10(BMXTHICR_sam_05_full$results$pvalue)
                  -log10(BMXTHICR_me$results$pvalue),
                  -log10(BMXTHICR_me_adj$results$pvalue),
                  -log10(BMXTHICR_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_BMXTHICR$vibFrame$HR,
                  BMXTHICR_sam_05_full$results$HR,
                  BMXTHICR_me$results$HR,
                  BMXTHICR_me_adj$results$HR,
                  BMXTHICR_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = BMXTHICR_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = BMXTHICR_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = BMXTHICR_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_BMXTHICR,legend_position = c(0.8, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = BMXTHICR_sam_05_full,legend_position = c(0.8, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

# Figure 3:

#pdf("Figures/voe_pattern_real_color/BMXTHICR.pdf", height = 9, width = 13)
tiff("Figures/voe_pattern_real_color/BMXTHICR.tiff", height = 700, width = 1000)
grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))

dev.off()




##################
###   LBDHDL   ###
##################


load("Results/model_real/mod_real_LBDHDL.RData")
load("Results/sample_real/LBDHDL_sam_05.RData")

load("Results/measurement_real_voi/LBDHDL_me.RData")
LBDHDL_me_voi <- LBDHDL_me
load("Results/measurement_real_adj/LBDHDL_me.RData")
LBDHDL_me_adj <- LBDHDL_me
load("Results/measurement_real/LBDHDL_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBDHDL$vibFrame$pvalue),
                  -log10(LBDHDL_sam_05_full$results$pvalue)
                  -log10(LBDHDL_me$results$pvalue),
                  -log10(LBDHDL_me_adj$results$pvalue),
                  -log10(LBDHDL_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBDHDL$vibFrame$HR,
                  LBDHDL_sam_05_full$results$HR,
                  LBDHDL_me$results$HR,
                  LBDHDL_me_adj$results$HR,
                  LBDHDL_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBDHDL_me, legend_position = c(0.8, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBDHDL_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBDHDL_me_voi, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBDHDL,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBDHDL_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

# Figure 4:

#pdf("Figures/voe_pattern_real_color/LBDHDL.pdf", height = 9, width = 13)
tiff("Figures/voe_pattern_real_color/LBDHDL.tiff", height = 700, width = 1000)
grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))
dev.off()




##################
###   any_ht   ###
##################


load("Results/model_real/mod_real_any_ht.RData")
load("Results/sample_real/any_ht_sam_05.RData")

load("Results/measurement_real_voi/any_ht_me.RData")
any_ht_me_voi <- any_ht_me
load("Results/measurement_real_adj/any_ht_me.RData")
any_ht_me_adj <- any_ht_me
load("Results/measurement_real/any_ht_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_any_ht$vibFrame$pvalue),
                  -log10(any_ht_sam_05_full$results$pvalue)
                  -log10(any_ht_me$results$pvalue),
                  -log10(any_ht_me_adj$results$pvalue),
                  -log10(any_ht_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_any_ht$vibFrame$HR,
                  any_ht_sam_05_full$results$HR,
                  any_ht_me$results$HR,
                  any_ht_me_adj$results$HR,
                  any_ht_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = any_ht_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = any_ht_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = any_ht_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_any_ht,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = any_ht_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/any_ht.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



##################################
###   any_cancer_self_report   ###
##################################


load("Results/model_real/mod_real_any_cancer_self_report.RData")
load("Results/sample_real/any_cancer_self_report_sam_05.RData")

load("Results/measurement_real_voi/any_cancer_self_report_me.RData")
any_cancer_self_report_me_voi <- any_cancer_self_report_me
load("Results/measurement_real_adj/any_cancer_self_report_me.RData")
any_cancer_self_report_me_adj <- any_cancer_self_report_me
load("Results/measurement_real/any_cancer_self_report_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_any_cancer_self_report$vibFrame$pvalue),
                  -log10(any_cancer_self_report_sam_05_full$results$pvalue)
                  -log10(any_cancer_self_report_me$results$pvalue),
                  -log10(any_cancer_self_report_me_adj$results$pvalue),
                  -log10(any_cancer_self_report_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_any_cancer_self_report$vibFrame$HR,
                  any_cancer_self_report_sam_05_full$results$HR,
                  any_cancer_self_report_me$results$HR,
                  any_cancer_self_report_me_adj$results$HR,
                  any_cancer_self_report_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = any_cancer_self_report_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = any_cancer_self_report_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = any_cancer_self_report_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_any_cancer_self_report,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = any_cancer_self_report_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/any_cancer_self_report.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


##########################
###   any_family_cad   ###
##########################


load("Results/model_real/mod_real_any_family_cad.RData")
load("Results/sample_real/any_family_cad_sam_05.RData")

load("Results/measurement_real_voi/any_family_cad_me.RData")
any_family_cad_me_voi <- any_family_cad_me
load("Results/measurement_real_adj/any_family_cad_me.RData")
any_family_cad_me_adj <- any_family_cad_me
load("Results/measurement_real/any_family_cad_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_any_family_cad$vibFrame$pvalue),
                  -log10(any_family_cad_sam_05_full$results$pvalue)
                  -log10(any_family_cad_me$results$pvalue),
                  -log10(any_family_cad_me_adj$results$pvalue),
                  -log10(any_family_cad_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_any_family_cad$vibFrame$HR,
                  any_family_cad_sam_05_full$results$HR,
                  any_family_cad_me$results$HR,
                  any_family_cad_me_adj$results$HR,
                  any_family_cad_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = any_family_cad_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = any_family_cad_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = any_family_cad_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_any_family_cad,legend_position = c(0.8, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = any_family_cad_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/any_family_cad.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


###################
###   LBDNENO   ###
###################


load("Results/model_real/mod_real_LBDNENO.RData")
load("Results/sample_real/LBDNENO_sam_05.RData")

load("Results/measurement_real_voi/LBDNENO_me.RData")
LBDNENO_me_voi <- LBDNENO_me
load("Results/measurement_real_adj/LBDNENO_me.RData")
LBDNENO_me_adj <- LBDNENO_me
load("Results/measurement_real/LBDNENO_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBDNENO$vibFrame$pvalue),
                  -log10(LBDNENO_sam_05_full$results$pvalue)
                  -log10(LBDNENO_me$results$pvalue),
                  -log10(LBDNENO_me_adj$results$pvalue),
                  -log10(LBDNENO_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBDNENO$vibFrame$HR,
                  LBDNENO_sam_05_full$results$HR,
                  LBDNENO_me$results$HR,
                  LBDNENO_me_adj$results$HR,
                  LBDNENO_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBDNENO_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBDNENO_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBDNENO_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBDNENO,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBDNENO_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBDNENO.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


###################
###   BMXCALF   ###
###################


load("Results/model_real/mod_real_BMXCALF.RData")
load("Results/sample_real/BMXCALF_sam_05.RData")

load("Results/measurement_real_voi/BMXCALF_me.RData")
BMXCALF_me_voi <- BMXCALF_me
load("Results/measurement_real_adj/BMXCALF_me.RData")
BMXCALF_me_adj <- BMXCALF_me
load("Results/measurement_real/BMXCALF_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_BMXCALF$vibFrame$pvalue),
                  -log10(BMXCALF_sam_05_full$results$pvalue)
                  -log10(BMXCALF_me$results$pvalue),
                  -log10(BMXCALF_me_adj$results$pvalue),
                  -log10(BMXCALF_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_BMXCALF$vibFrame$HR,
                  BMXCALF_sam_05_full$results$HR,
                  BMXCALF_me$results$HR,
                  BMXCALF_me_adj$results$HR,
                  BMXCALF_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = BMXCALF_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = BMXCALF_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = BMXCALF_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_BMXCALF,legend_position = c(0.8, 0.8), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = BMXCALF_sam_05_full,legend_position = c(0.6, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/BMXCALF.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


#################
###   BMXHT   ###
#################


load("Results/model_real/mod_real_BMXHT.RData")
load("Results/sample_real/BMXHT_sam_05.RData")

load("Results/measurement_real_voi/BMXHT_me.RData")
BMXHT_me_voi <- BMXHT_me
load("Results/measurement_real_adj/BMXHT_me.RData")
BMXHT_me_adj <- BMXHT_me
load("Results/measurement_real/BMXHT_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_BMXHT$vibFrame$pvalue),
                  -log10(BMXHT_sam_05_full$results$pvalue)
                  -log10(BMXHT_me$results$pvalue),
                  -log10(BMXHT_me_adj$results$pvalue),
                  -log10(BMXHT_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_BMXHT$vibFrame$HR,
                  BMXHT_sam_05_full$results$HR,
                  BMXHT_me$results$HR,
                  BMXHT_me_adj$results$HR,
                  BMXHT_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = BMXHT_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = BMXHT_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = BMXHT_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_BMXHT,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = BMXHT_sam_05_full,legend_position = c(0.7, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/BMXHT.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



####################
###   BMXWAIST   ###
####################


load("Results/model_real/mod_real_BMXWAIST.RData")
load("Results/sample_real/BMXWAIST_sam_05.RData")

load("Results/measurement_real_voi/BMXWAIST_me.RData")
BMXWAIST_me_voi <- BMXWAIST_me
load("Results/measurement_real_adj/BMXWAIST_me.RData")
BMXWAIST_me_adj <- BMXWAIST_me
load("Results/measurement_real/BMXWAIST_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_BMXWAIST$vibFrame$pvalue),
                  -log10(BMXWAIST_sam_05_full$results$pvalue)
                  -log10(BMXWAIST_me$results$pvalue),
                  -log10(BMXWAIST_me_adj$results$pvalue),
                  -log10(BMXWAIST_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_BMXWAIST$vibFrame$HR,
                  BMXWAIST_sam_05_full$results$HR,
                  BMXWAIST_me$results$HR,
                  BMXWAIST_me_adj$results$HR,
                  BMXWAIST_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = BMXWAIST_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = BMXWAIST_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = BMXWAIST_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_BMXWAIST,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = BMXWAIST_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/BMXWAIST.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



#################
###   BMXWT   ###
#################


load("Results/model_real/mod_real_BMXWT.RData")
load("Results/sample_real/BMXWT_sam_05.RData")

load("Results/measurement_real_voi/BMXWT_me.RData")
BMXWT_me_voi <- BMXWT_me
load("Results/measurement_real_adj/BMXWT_me.RData")
BMXWT_me_adj <- BMXWT_me
load("Results/measurement_real/BMXWT_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_BMXWT$vibFrame$pvalue),
                  -log10(BMXWT_sam_05_full$results$pvalue)
                  -log10(BMXWT_me$results$pvalue),
                  -log10(BMXWT_me_adj$results$pvalue),
                  -log10(BMXWT_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_BMXWT$vibFrame$HR,
                  BMXWT_sam_05_full$results$HR,
                  BMXWT_me$results$HR,
                  BMXWT_me_adj$results$HR,
                  BMXWT_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = BMXWT_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = BMXWT_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = BMXWT_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_BMXWT,legend_position = c(0.8, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = BMXWT_sam_05_full,legend_position = c(0.8, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/BMXWT.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



##################
###   BPXPLS   ###
##################


load("Results/model_real/mod_real_BPXPLS.RData")
load("Results/sample_real/BPXPLS_sam_05.RData")

load("Results/measurement_real_voi/BPXPLS_me.RData")
BPXPLS_me_voi <- BPXPLS_me
load("Results/measurement_real_adj/BPXPLS_me.RData")
BPXPLS_me_adj <- BPXPLS_me
load("Results/measurement_real/BPXPLS_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_BPXPLS$vibFrame$pvalue),
                  -log10(BPXPLS_sam_05_full$results$pvalue)
                  -log10(BPXPLS_me$results$pvalue),
                  -log10(BPXPLS_me_adj$results$pvalue),
                  -log10(BPXPLS_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_BPXPLS$vibFrame$HR,
                  BPXPLS_sam_05_full$results$HR,
                  BPXPLS_me$results$HR,
                  BPXPLS_me_adj$results$HR,
                  BPXPLS_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = BPXPLS_me, legend_position = c(0.8, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = BPXPLS_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = BPXPLS_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_BPXPLS,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = BPXPLS_sam_05_full,legend_position = c(0.4, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/BPXPLS.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



####################
###   DSDCOUNT   ###
####################


load("Results/model_real/mod_real_DSDCOUNT.RData")
load("Results/sample_real/DSDCOUNT_sam_05.RData")

load("Results/measurement_real_voi/DSDCOUNT_me.RData")
DSDCOUNT_me_voi <- DSDCOUNT_me
load("Results/measurement_real_adj/DSDCOUNT_me.RData")
DSDCOUNT_me_adj <- DSDCOUNT_me
load("Results/measurement_real/DSDCOUNT_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_DSDCOUNT$vibFrame$pvalue),
                  -log10(DSDCOUNT_sam_05_full$results$pvalue)
                  -log10(DSDCOUNT_me$results$pvalue),
                  -log10(DSDCOUNT_me_adj$results$pvalue),
                  -log10(DSDCOUNT_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_DSDCOUNT$vibFrame$HR,
                  DSDCOUNT_sam_05_full$results$HR,
                  DSDCOUNT_me$results$HR,
                  DSDCOUNT_me_adj$results$HR,
                  DSDCOUNT_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = DSDCOUNT_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = DSDCOUNT_me_voi, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = DSDCOUNT_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_DSDCOUNT,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = DSDCOUNT_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/DSDCOUNT.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



#################
###   LBXHA   ###
#################


load("Results/model_real/mod_real_LBXHA.RData")
load("Results/sample_real/LBXHA_sam_05.RData")

load("Results/measurement_real_voi/LBXHA_me.RData")
LBXHA_me_voi <- LBXHA_me
load("Results/measurement_real_adj/LBXHA_me.RData")
LBXHA_me_adj <- LBXHA_me
load("Results/measurement_real/LBXHA_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXHA$vibFrame$pvalue),
                  -log10(LBXHA_sam_05_full$results$pvalue)
                  -log10(LBXHA_me$results$pvalue),
                  -log10(LBXHA_me_adj$results$pvalue),
                  -log10(LBXHA_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXHA$vibFrame$HR,
                  LBXHA_sam_05_full$results$HR,
                  LBXHA_me$results$HR,
                  LBXHA_me_adj$results$HR,
                  LBXHA_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXHA_me, legend_position = c(0.5, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXHA_me_voi, legend_position = c(0.5, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXHA_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXHA,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXHA_sam_05_full,legend_position = c(0.7, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXHA.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


##################
###   LBXHBC   ###
##################


load("Results/model_real/mod_real_LBXHBC.RData")
load("Results/sample_real/LBXHBC_sam_05.RData")

load("Results/measurement_real_voi/LBXHBC_me.RData")
LBXHBC_me_voi <- LBXHBC_me
load("Results/measurement_real_adj/LBXHBC_me.RData")
LBXHBC_me_adj <- LBXHBC_me
load("Results/measurement_real/LBXHBC_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXHBC$vibFrame$pvalue),
                  -log10(LBXHBC_sam_05_full$results$pvalue)
                  -log10(LBXHBC_me$results$pvalue),
                  -log10(LBXHBC_me_adj$results$pvalue),
                  -log10(LBXHBC_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXHBC$vibFrame$HR,
                  LBXHBC_sam_05_full$results$HR,
                  LBXHBC_me$results$HR,
                  LBXHBC_me_adj$results$HR,
                  LBXHBC_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXHBC_me, legend_position = c(0.7, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXHBC_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXHBC_me_adj, legend_position = c(0.7, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXHBC,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXHBC_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXHBC.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


##################
###   LBXHBS   ###
##################


load("Results/model_real/mod_real_LBXHBS.RData")
load("Results/sample_real/LBXHBS_sam_05.RData")

load("Results/measurement_real_voi/LBXHBS_me.RData")
LBXHBS_me_voi <- LBXHBS_me
load("Results/measurement_real_adj/LBXHBS_me.RData")
LBXHBS_me_adj <- LBXHBS_me
load("Results/measurement_real/LBXHBS_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXHBS$vibFrame$pvalue),
                  -log10(LBXHBS_sam_05_full$results$pvalue)
                  -log10(LBXHBS_me$results$pvalue),
                  -log10(LBXHBS_me_adj$results$pvalue),
                  -log10(LBXHBS_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXHBS$vibFrame$HR,
                  LBXHBS_sam_05_full$results$HR,
                  LBXHBS_me$results$HR,
                  LBXHBS_me_adj$results$HR,
                  LBXHBS_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXHBS_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXHBS_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXHBS_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXHBS,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXHBS_sam_05_full,legend_position = c(0.4, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXHBS.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



####################
###   LBXLYPCT   ###
####################


load("Results/model_real/mod_real_LBXLYPCT.RData")
load("Results/sample_real/LBXLYPCT_sam_05.RData")

load("Results/measurement_real_voi/LBXLYPCT_me.RData")
LBXLYPCT_me_voi <- LBXLYPCT_me
load("Results/measurement_real_adj/LBXLYPCT_me.RData")
LBXLYPCT_me_adj <- LBXLYPCT_me
load("Results/measurement_real/LBXLYPCT_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXLYPCT$vibFrame$pvalue),
                  -log10(LBXLYPCT_sam_05_full$results$pvalue)
                  -log10(LBXLYPCT_me$results$pvalue),
                  -log10(LBXLYPCT_me_adj$results$pvalue),
                  -log10(LBXLYPCT_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXLYPCT$vibFrame$HR,
                  LBXLYPCT_sam_05_full$results$HR,
                  LBXLYPCT_me$results$HR,
                  LBXLYPCT_me_adj$results$HR,
                  LBXLYPCT_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXLYPCT_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXLYPCT_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXLYPCT_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXLYPCT,legend_position = c(0.8, 0.8), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXLYPCT_sam_05_full,legend_position = c(0.7, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXLYPCT.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



###################
###   LBXMPSI   ###
###################


load("Results/model_real/mod_real_LBXMPSI.RData")
load("Results/sample_real/LBXMPSI_sam_05.RData")

load("Results/measurement_real_voi/LBXMPSI_me.RData")
LBXMPSI_me_voi <- LBXMPSI_me
load("Results/measurement_real_adj/LBXMPSI_me.RData")
LBXMPSI_me_adj <- LBXMPSI_me
load("Results/measurement_real/LBXMPSI_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXMPSI$vibFrame$pvalue),
                  -log10(LBXMPSI_sam_05_full$results$pvalue)
                  -log10(LBXMPSI_me$results$pvalue),
                  -log10(LBXMPSI_me_adj$results$pvalue),
                  -log10(LBXMPSI_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXMPSI$vibFrame$HR,
                  LBXMPSI_sam_05_full$results$HR,
                  LBXMPSI_me$results$HR,
                  LBXMPSI_me_adj$results$HR,
                  LBXMPSI_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXMPSI_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXMPSI_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXMPSI_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXMPSI,legend_position = c(0.8, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXMPSI_sam_05_full,legend_position = c(0.7, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXMPSI.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



##################
###   LBXSCA   ###
##################


load("Results/model_real/mod_real_LBXSCA.RData")
load("Results/sample_real/LBXSCA_sam_05.RData")

load("Results/measurement_real_voi/LBXSCA_me.RData")
LBXSCA_me_voi <- LBXSCA_me
load("Results/measurement_real_adj/LBXSCA_me.RData")
LBXSCA_me_adj <- LBXSCA_me
load("Results/measurement_real/LBXSCA_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXSCA$vibFrame$pvalue),
                  -log10(LBXSCA_sam_05_full$results$pvalue)
                  -log10(LBXSCA_me$results$pvalue),
                  -log10(LBXSCA_me_adj$results$pvalue),
                  -log10(LBXSCA_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXSCA$vibFrame$HR,
                  LBXSCA_sam_05_full$results$HR,
                  LBXSCA_me$results$HR,
                  LBXSCA_me_adj$results$HR,
                  LBXSCA_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXSCA_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXSCA_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXSCA_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXSCA,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXSCA_sam_05_full,legend_position = c(0.5, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXSCA.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


####################
###   LBXSNASI   ###
####################


load("Results/model_real/mod_real_LBXSNASI.RData")
load("Results/sample_real/LBXSNASI_sam_05.RData")

load("Results/measurement_real_voi/LBXSNASI_me.RData")
LBXSNASI_me_voi <- LBXSNASI_me
load("Results/measurement_real_adj/LBXSNASI_me.RData")
LBXSNASI_me_adj <- LBXSNASI_me
load("Results/measurement_real/LBXSNASI_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXSNASI$vibFrame$pvalue),
                  -log10(LBXSNASI_sam_05_full$results$pvalue)
                  -log10(LBXSNASI_me$results$pvalue),
                  -log10(LBXSNASI_me_adj$results$pvalue),
                  -log10(LBXSNASI_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXSNASI$vibFrame$HR,
                  LBXSNASI_sam_05_full$results$HR,
                  LBXSNASI_me$results$HR,
                  LBXSNASI_me_adj$results$HR,
                  LBXSNASI_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXSNASI_me, legend_position = c(0.7, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXSNASI_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXSNASI_me_adj, legend_position = c(0.7, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXSNASI,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXSNASI_sam_05_full,legend_position = c(0.5, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXSNASI.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



####################
###   LBXSOSSI   ###
####################


load("Results/model_real/mod_real_LBXSOSSI.RData")
load("Results/sample_real/LBXSOSSI_sam_05.RData")

load("Results/measurement_real_voi/LBXSOSSI_me.RData")
LBXSOSSI_me_voi <- LBXSOSSI_me
load("Results/measurement_real_adj/LBXSOSSI_me.RData")
LBXSOSSI_me_adj <- LBXSOSSI_me
load("Results/measurement_real/LBXSOSSI_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXSOSSI$vibFrame$pvalue),
                  -log10(LBXSOSSI_sam_05_full$results$pvalue)
                  -log10(LBXSOSSI_me$results$pvalue),
                  -log10(LBXSOSSI_me_adj$results$pvalue),
                  -log10(LBXSOSSI_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXSOSSI$vibFrame$HR,
                  LBXSOSSI_sam_05_full$results$HR,
                  LBXSOSSI_me$results$HR,
                  LBXSOSSI_me_adj$results$HR,
                  LBXSOSSI_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXSOSSI_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXSOSSI_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXSOSSI_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXSOSSI,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXSOSSI_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXSOSSI.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


##################
###   LBXSPH   ###
##################


load("Results/model_real/mod_real_LBXSPH.RData")
load("Results/sample_real/LBXSPH_sam_05.RData")

load("Results/measurement_real_voi/LBXSPH_me.RData")
LBXSPH_me_voi <- LBXSPH_me
load("Results/measurement_real_adj/LBXSPH_me.RData")
LBXSPH_me_adj <- LBXSPH_me
load("Results/measurement_real/LBXSPH_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_LBXSPH$vibFrame$pvalue),
                  -log10(LBXSPH_sam_05_full$results$pvalue)
                  -log10(LBXSPH_me$results$pvalue),
                  -log10(LBXSPH_me_adj$results$pvalue),
                  -log10(LBXSPH_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_LBXSPH$vibFrame$HR,
                  LBXSPH_sam_05_full$results$HR,
                  LBXSPH_me$results$HR,
                  LBXSPH_me_adj$results$HR,
                  LBXSPH_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = LBXSPH_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = LBXSPH_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = LBXSPH_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_LBXSPH,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = LBXSPH_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/LBXSPH.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



################
###   male   ###
################


load("Results/model_real/mod_real_male.RData")
load("Results/sample_real/male_sam_05.RData")

load("Results/measurement_real_voi/male_me.RData")
male_me_voi <- male_me
load("Results/measurement_real_adj/male_me.RData")
male_me_adj <- male_me
load("Results/measurement_real/male_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_male$vibFrame$pvalue),
                  -log10(male_sam_05_full$results$pvalue)
                  -log10(male_me$results$pvalue),
                  -log10(male_me_adj$results$pvalue),
                  -log10(male_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_male$vibFrame$HR,
                  male_sam_05_full$results$HR,
                  male_me$results$HR,
                  male_me_adj$results$HR,
                  male_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = male_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = male_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = male_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_male,legend_position = c(0.8, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = male_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/male.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


########################################
###   pest_control_home_last_month   ###
########################################


load("Results/model_real/mod_real_pest_control_home_last_month.RData")
load("Results/sample_real/pest_control_home_last_month_sam_05.RData")

load("Results/measurement_real_voi/pest_control_home_last_month_me.RData")
pest_control_home_last_month_me_voi <- pest_control_home_last_month_me
load("Results/measurement_real_adj/pest_control_home_last_month_me.RData")
pest_control_home_last_month_me_adj <- pest_control_home_last_month_me
load("Results/measurement_real/pest_control_home_last_month_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_pest_control_home_last_month$vibFrame$pvalue),
                  -log10(pest_control_home_last_month_sam_05_full$results$pvalue)
                  -log10(pest_control_home_last_month_me$results$pvalue),
                  -log10(pest_control_home_last_month_me_adj$results$pvalue),
                  -log10(pest_control_home_last_month_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_pest_control_home_last_month$vibFrame$HR,
                  pest_control_home_last_month_sam_05_full$results$HR,
                  pest_control_home_last_month_me$results$HR,
                  pest_control_home_last_month_me_adj$results$HR,
                  pest_control_home_last_month_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = pest_control_home_last_month_me, legend_position = c(0.3, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = pest_control_home_last_month_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = pest_control_home_last_month_me_adj, legend_position = c(0.7, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_pest_control_home_last_month,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = pest_control_home_last_month_sam_05_full,legend_position = c(0.4, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/pest_control_home_last_month.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


################
###   pneu   ###
################


load("Results/model_real/mod_real_pneu.RData")
load("Results/sample_real/pneu_sam_05.RData")

load("Results/measurement_real_voi/pneu_me.RData")
pneu_me_voi <- pneu_me
load("Results/measurement_real_adj/pneu_me.RData")
pneu_me_adj <- pneu_me
load("Results/measurement_real/pneu_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_pneu$vibFrame$pvalue),
                  -log10(pneu_sam_05_full$results$pvalue)
                  -log10(pneu_me$results$pvalue),
                  -log10(pneu_me_adj$results$pvalue),
                  -log10(pneu_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_pneu$vibFrame$HR,
                  pneu_sam_05_full$results$HR,
                  pneu_me$results$HR,
                  pneu_me_adj$results$HR,
                  pneu_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = pneu_me, legend_position = c(0.5, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = pneu_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = pneu_me_adj, legend_position = c(0.7, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_pneu,legend_position = c(0.8, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = pneu_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/pneu.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


################################
###   private_water_source   ###
################################


load("Results/model_real/mod_real_private_water_source.RData")
load("Results/sample_real/private_water_source_sam_05.RData")

load("Results/measurement_real_voi/private_water_source_me.RData")
private_water_source_me_voi <- private_water_source_me
load("Results/measurement_real_adj/private_water_source_me.RData")
private_water_source_me_adj <- private_water_source_me
load("Results/measurement_real/private_water_source_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_private_water_source$vibFrame$pvalue),
                  -log10(private_water_source_sam_05_full$results$pvalue)
                  -log10(private_water_source_me$results$pvalue),
                  -log10(private_water_source_me_adj$results$pvalue),
                  -log10(private_water_source_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_private_water_source$vibFrame$HR,
                  private_water_source_sam_05_full$results$HR,
                  private_water_source_me$results$HR,
                  private_water_source_me_adj$results$HR,
                  private_water_source_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = private_water_source_me, legend_position = c(0.7, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = private_water_source_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = private_water_source_me_adj, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_private_water_source,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = private_water_source_sam_05_full,legend_position = c(0.8, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/private_water_source.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()



###############################
###   use_water_treatment   ###
###############################


load("Results/model_real/mod_real_use_water_treatment.RData")
load("Results/sample_real/use_water_treatment_sam_05.RData")

load("Results/measurement_real_voi/use_water_treatment_me.RData")
use_water_treatment_me_voi <- use_water_treatment_me
load("Results/measurement_real_adj/use_water_treatment_me.RData")
use_water_treatment_me_adj <- use_water_treatment_me
load("Results/measurement_real/use_water_treatment_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_use_water_treatment$vibFrame$pvalue),
                  -log10(use_water_treatment_sam_05_full$results$pvalue)
                  -log10(use_water_treatment_me$results$pvalue),
                  -log10(use_water_treatment_me_adj$results$pvalue),
                  -log10(use_water_treatment_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_use_water_treatment$vibFrame$HR,
                  use_water_treatment_sam_05_full$results$HR,
                  use_water_treatment_me$results$HR,
                  use_water_treatment_me_adj$results$HR,
                  use_water_treatment_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = use_water_treatment_me, legend_position = c(0.8, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = use_water_treatment_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = use_water_treatment_me_adj, legend_position = c(0.7, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_use_water_treatment,legend_position = c(0.7, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = use_water_treatment_sam_05_full,legend_position = c(0.6, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/use_water_treatment.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


####################
###   RIDAGEYR   ###
####################

load("Results/model_real/mod_real_RIDAGEYR.RData")
load("Results/sample_real/RIDAGEYR_sam_05.RData")

load("Results/measurement_real_voi/RIDAGEYR_me.RData")
RIDAGEYR_me_voi <- RIDAGEYR_me
load("Results/measurement_real_adj/RIDAGEYR_me.RData")
RIDAGEYR_me_adj <- RIDAGEYR_me
load("Results/measurement_real/RIDAGEYR_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_RIDAGEYR$vibFrame$pvalue),
                  -log10(RIDAGEYR_sam_05_full$results$pvalue)
                  -log10(RIDAGEYR_me$results$pvalue),
                  -log10(RIDAGEYR_me_adj$results$pvalue),
                  -log10(RIDAGEYR_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_RIDAGEYR$vibFrame$HR,
                  RIDAGEYR_sam_05_full$results$HR,
                  RIDAGEYR_me$results$HR,
                  RIDAGEYR_me_adj$results$HR,
                  RIDAGEYR_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = RIDAGEYR_me, legend_position = c(0.8, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = RIDAGEYR_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = RIDAGEYR_me_adj, legend_position = c(0.3, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_RIDAGEYR,legend_position = c(0.8, 0.3), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = RIDAGEYR_sam_05_full,legend_position = c(0.3, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/RIDAGEYR.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()


##################
###   SMD410   ###
##################


load("Results/model_real/mod_real_SMD410.RData")
load("Results/sample_real/SMD410_sam_05.RData")

load("Results/measurement_real_voi/SMD410_me.RData")
SMD410_me_voi <- SMD410_me
load("Results/measurement_real_adj/SMD410_me.RData")
SMD410_me_adj <- SMD410_me
load("Results/measurement_real/SMD410_me.RData")

yrange <- range(c(-log10(.05), 
                  -log10(mod_real_SMD410$vibFrame$pvalue),
                  -log10(SMD410_sam_05_full$results$pvalue)
                  -log10(SMD410_me$results$pvalue),
                  -log10(SMD410_me_adj$results$pvalue),
                  -log10(SMD410_me_voi$results$pvalue)), na.rm = T)

xrange <- range(c(mod_real_SMD410$vibFrame$HR,
                  SMD410_sam_05_full$results$HR,
                  SMD410_me$results$HR,
                  SMD410_me_adj$results$HR,
                  SMD410_me_voi$results$HR), na.rm = T)



# measurement error
me_plot <-  vib2d_real_color(vibObj = SMD410_me, legend_position = c(0.7, 0.7), 
                       y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest and in \nadjustment variables")



# measurement error variable of interest
me_voi_plot <-  vib2d_real_color(vibObj = SMD410_me_voi, legend_position = c(0.8, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in variable of interest\n")


# measurement error adjustment
me_adj_plot <-  vib2d_real_color(vibObj = SMD410_me_adj, legend_position = c(0.7, 0.7), 
                           y_reference = T, yRange = yrange, xRange = xrange, title = "Error in adjustment variables\n")


# model
model_plot <- vib2d_real_color(vibObj = mod_real_SMD410,legend_position = c(0.3, 0.7), 
                         y_reference = T, yRange = yrange, xRange = xrange, title = "Model vibration", 
                         true_beta = F)

# sample
sample_plot <- vib2d_real_color(vibObj = SMD410_sam_05_full,legend_position = c(0.7, 0.7), 
                          y_reference = T, yRange = yrange, xRange = xrange, title = "Sampling vibration", 
                          true_beta = F)



a1 <- as.grob(me_plot)
a2 <- as.grob(me_voi_plot)
a3 <- as.grob(me_adj_plot)
a4 <- as.grob(model_plot)
a5 <- as.grob(sample_plot)
a6 <- textGrob("Measurement vibration", rot = 90, gp = gpar(fontsize = 16))

pdf("Figures/voe_pattern_real_color/SMD410.pdf", height = 9, width = 13)


grid.arrange(grobs = list(a1, a2, a3, a4, a5, a6),
             heights=c(1,1),
             layout_matrix = rbind(c(6,1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3),
                                   c(NA, NA, NA, 4,4,4,4,4, NA, 5,5,5,5,5, NA, NA)))


dev.off()