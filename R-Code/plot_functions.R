# Functions required to create the figures
#
# vib2d_real: Volcano plots for the NHANES data
# sim_RHR: RHR for different sample sizes + barplots
#   tos: subfunction of sim_RHR; type of significance for the three types of vibration


#########################################################
###   vib2d_real: Volcano plots for the nhanes data   ###
#########################################################

### Input:
#
# vibObj              Object containing results of measurement, model or sampling vibration
# contour             Logical, whether plot should be a contour plot, default is FALSE
# nbins               See argument bins in geom_hex()
# alpha               See argument alpha in geom_point()
# title               Main title, default is NULL
# xRange              Range of x axis
# yRange              Range of y axis
# y_reference         Logical, whether a reference line should be drawn at p = 0.001, default is FALSE
# true_beta           Logical, default is TRUE, should be FALSE for model and sampling vibration
# legend_position     Legend position, default is 'none'
#
### Output: 
#
# p                   Volcano plot


vib2d_real <- function(vibObj, contour = FALSE, nbins = 50, alpha = NULL, title = NULL,
                  xRange = NULL, yRange = NULL, y_reference = F,
                  true_beta = T, legend_position = "none"){
  
  if("results" %in% names(vibObj)){
    vibFrame <- vibObj$results
  } else if("vibFrame" %in% names(vibObj)){
    vibFrame <- vibObj$vibFrame
  }
  
  if(contour){
    if(is.null(alpha)){
      alpha = 1
    }
    contourData <- getContoursForPctile_OR(vibFrame)
    if(!is.null(nbins)){
      warning("argument nbins ignored")
    }
  } else {
    if(is.null(nbins)){
      nbins = 20
    }
    if(!is.null(alpha)){
      warning("argument alpha ignored")
    }
  }
  
  if(is.null(xRange)){
    xRange <- range(vibFrame$HR, na.rm=T)
  }
  if(is.null(yRange)){
    yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm=T)
  }
  
  probs <- c(0.01, 0.5, 0.99)
  hProbs <- c(0.01, 0.5, 0.99)
  
  qs_p <- quantile(vibFrame$pvalue, probs, na.rm = T)
  pQuant <- data.frame(probs=probs, pvalue=qs_p)          
  qs_HR <- quantile(vibFrame$estimate, probs, na.rm = T)
  hQuant <- data.frame(probs=probs, HR=exp(qs_HR))          
  
  RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)            
  RPvalue <- round(-log10(pQuant[1,'pvalue']) + log10(pQuant[3,'pvalue']), 2) 
  
  p <- ggplot(vibFrame, aes(HR, -log10(pvalue)))            
  
  if(contour){
    p <- p + geom_point(alpha=alpha)
    p <- p + geom_contour(data=contourData$densityData, aes(x=x,y=y,z=z), breaks=contourData$levels, size=.3)
  } else {
    p <- p + geom_hex(bins=nbins) + scale_fill_gradientn(name='Density', colours=c('gray40','gray90'))
    #p <- p + geom_hex(bins=nbins) + scale_fill_gradientn(name='Density', colours=c('blue','yellow'))
   
  }
  

  pQuant$x <- max(xRange)
  p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant, show.legend = F)    # Linien bei wichtigen quantilen pvalue
  p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant) # Texte an Linien
  
 
  hQuant$y <- max(c(yRange, -log10(0.05)))
  p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant, show.legend = F) # Linien bei wichtigen Quantilen OR
  p <- p + geom_text(aes(x=HR, y=y, label=round(probs*100, 2), hjust=-.1, vjust=.4), data=hQuant) # Texte an Linien
  
  # p <- p + geom_hline(yintercept=-log10(0.05), colour = "#1a9850") + geom_vline(xintercept = 1, color = "#1a9850")
  # p <- p + annotate('text', label = 'p = 0.05', x=xRange[1], y=-log10(0.05), hjust=.2, vjust=-.4,
  #                   size = 3, color = "#1a9850")
  
  #p <- p + geom_hline(yintercept=-log10(0.05)) + geom_vline(xintercept = 1, color = "#1a9850")
  p <- p + geom_hline(yintercept=-log10(0.05)) + geom_vline(xintercept = 1)
  p <- p + annotate('text', label = 'p = 0.05', x=xRange[1], y=-log10(0.05), hjust=.2, vjust=-.4,
                    size = 3)
  
  
  if(y_reference){
    
    # p <- p + geom_hline(yintercept=-log10(0.001), color = "#1a9850") #+ geom_hline(yintercept=-log10(0.01))
    # p <- p + annotate('text', label = 'p = 0.001', x=xRange[1], y=-log10(0.001), hjust=.2, vjust=-.4,
    #                   size = 3, color = "#1a9850") # Texte an Linien
    
    p <- p + geom_hline(yintercept=-log10(0.001))
    p <- p + annotate('text', label = 'p = 0.001', x=xRange[1], y=-log10(0.001), hjust=.2, vjust=-.4,
                      size = 3)
  }
  
  p <- p + scale_x_continuous(limits=xRange) + scale_y_continuous(limits=yRange) 
  
  
  if(!is.null(title)){
    
    if(!is.null(vibObj$n)){
      p <- p + ggtitle(label = paste(title), 
                       subtitle = sprintf('RHR = %.02f\nRP = %.02f',  RHR, RPvalue))
    } else{
      p <- p + ggtitle(label = paste(title), 
                       subtitle = sprintf('RHR = %.02f\nRP = %.02f',  RHR, RPvalue))
    }
    p <- p + xlab('Hazard Ratio') + ylab('-log10(p)') + theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                                                                           legend.position = legend_position,
                                                                           legend.text = element_text(size = 10),
                                                                           legend.key.size =  unit(0.4, "cm"))
    
  } else {
    
    title = ""
    
    if(!is.null(vibObj$n)){
      p <- p + ggtitle(sprintf('%s \nRHR = %.02f\nRP = %.02f', title, RHR, RPvalue))
    } else {
      p <- p + ggtitle(sprintf('%s \nRHR = %.02f\nRP = %.02f', title, RHR, RPvalue))
    }
    
    p <- p + xlab('Hazard Ratio') + ylab('-log10(p)') + theme_bw() + theme(plot.title = element_text(hjust = 0, size = 12),
                                                                           legend.position = legend_position,
                                                                           legend.text = element_text(size = 10),
                                                                           legend.key.size =  unit(0.4, "cm"))
    
  }
  
  if(true_beta){
    p <- p + geom_point(aes(x = exp(vibObj$true_beta[1]), y = -log10(vibObj$true_beta[6])), colour="black", shape = 4, size = 3)
    #p <- p + geom_point(aes(x = exp(vibObj$true_beta[1]), y = -log10(vibObj$true_beta[6])), colour="green")
  }
  
  
  return(p)
  
}


########################################################################
###   vib2d_real_color: Volcano plots for the nhanes data in color   ###
########################################################################

### Input:
#
# vibObj              Object containing results of measurement, model or sampling vibration
# contour             Logical, whether plot should be a contour plot, default is FALSE
# nbins               See argument bins in geom_hex()
# alpha               See argument alpha in geom_point()
# title               Main title, default is NULL
# xRange              Range of x axis
# yRange              Range of y axis
# y_reference         Logical, whether a reference line should be drawn at p = 0.001, default is FALSE
# true_beta           Logical, default is TRUE, should be FALSE for model and sampling vibration
# legend_position     Legend position, default is 'none'
#
### Output: 
#
# p                   Volcano plot


vib2d_real_color <- function(vibObj, contour = FALSE, nbins = 50, alpha = NULL, title = NULL,
                       xRange = NULL, yRange = NULL, y_reference = F,
                       true_beta = T, legend_position = "none"){
  
  if("results" %in% names(vibObj)){
    vibFrame <- vibObj$results
  } else if("vibFrame" %in% names(vibObj)){
    vibFrame <- vibObj$vibFrame
  }
  
  if(contour){
    if(is.null(alpha)){
      alpha = 1
    }
    contourData <- getContoursForPctile_OR(vibFrame)
    if(!is.null(nbins)){
      warning("argument nbins ignored")
    }
  } else {
    if(is.null(nbins)){
      nbins = 20
    }
    if(!is.null(alpha)){
      warning("argument alpha ignored")
    }
  }
  
  if(is.null(xRange)){
    xRange <- range(vibFrame$HR, na.rm=T)
  }
  if(is.null(yRange)){
    yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm=T)
  }
  
  probs <- c(0.01, 0.5, 0.99)
  hProbs <- c(0.01, 0.5, 0.99)
  
  qs_p <- quantile(vibFrame$pvalue, probs, na.rm = T)
  pQuant <- data.frame(probs=probs, pvalue=qs_p)          
  qs_HR <- quantile(vibFrame$estimate, probs, na.rm = T)
  hQuant <- data.frame(probs=probs, HR=exp(qs_HR))          
  
  RHR <- round(hQuant[3,'HR']/hQuant[1,'HR'], 2)            
  RPvalue <- round(-log10(pQuant[1,'pvalue']) + log10(pQuant[3,'pvalue']), 2) 
  
  p <- ggplot(vibFrame, aes(HR, -log10(pvalue)))            
  
  if(contour){
    p <- p + geom_point(alpha=alpha)
    p <- p + geom_contour(data=contourData$densityData, aes(x=x,y=y,z=z), breaks=contourData$levels, size=.3)
  } else {
    #p <- p + geom_hex(bins=nbins) + scale_fill_gradientn(name='Density', colours=c('gray40','gray90'))
    p <- p + geom_hex(bins=nbins) + scale_fill_gradientn(name='Density', colours=c('blue','yellow'))
    
  }
  
  
  pQuant$x <- max(xRange)
  p <- p + geom_hline(aes(yintercept=-log10(pvalue), alpha=.4), linetype='dashed', data=pQuant, show.legend = F)    # Linien bei wichtigen quantilen pvalue
  p <- p + geom_text(aes(x=x, y=-log10(pvalue), label=round(probs*100, 2), vjust=-.2), data=pQuant) # Texte an Linien
  
  
  hQuant$y <- max(c(yRange, -log10(0.05)))
  p <- p + geom_vline(aes(xintercept=HR, alpha=.4), linetype='dashed', data=hQuant, show.legend = F) # Linien bei wichtigen Quantilen OR
  p <- p + geom_text(aes(x=HR, y=y, label=round(probs*100, 2), hjust=-.1, vjust=.4), data=hQuant) # Texte an Linien
  
  p <- p + geom_hline(yintercept=-log10(0.05), colour = "#1a9850") + geom_vline(xintercept = 1, color = "#1a9850")
  p <- p + annotate('text', label = 'p = 0.05', x=xRange[1], y=-log10(0.05), hjust=.2, vjust=-.4,
                    size = 3, color = "#1a9850")
  
  # p <- p + geom_hline(yintercept=-log10(0.05)) + geom_vline(xintercept = 1, color = "#1a9850")
  # #p <- p + geom_hline(yintercept=-log10(0.05)) + geom_vline(xintercept = 1)
  # p <- p + annotate('text', label = 'p = 0.05', x=xRange[1], y=-log10(0.05), hjust=.2, vjust=-.4,
  #                   size = 3)
  
  
  if(y_reference){
    
    p <- p + geom_hline(yintercept=-log10(0.001), color = "#1a9850") #+ geom_hline(yintercept=-log10(0.01))
    p <- p + annotate('text', label = 'p = 0.001', x=xRange[1], y=-log10(0.001), hjust=.2, vjust=-.4,
                      size = 3, color = "#1a9850") # Texte an Linien
    
    # p <- p + geom_hline(yintercept=-log10(0.001)) 
    # p <- p + annotate('text', label = 'p = 0.001', x=xRange[1], y=-log10(0.001), hjust=.2, vjust=-.4,
    #                   size = 3)
  }
  
  p <- p + scale_x_continuous(limits=xRange) + scale_y_continuous(limits=yRange) 
  
  
  if(!is.null(title)){
    
    if(!is.null(vibObj$n)){
      p <- p + ggtitle(label = paste(title), 
                       subtitle = sprintf('RHR = %.02f\nRP = %.02f',  RHR, RPvalue))
    } else{
      p <- p + ggtitle(label = paste(title), 
                       subtitle = sprintf('RHR = %.02f\nRP = %.02f',  RHR, RPvalue))
    }
    p <- p + xlab('Hazard Ratio') + ylab('-log10(p)') + theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                                                                           legend.position = legend_position,
                                                                           legend.text = element_text(size = 10),
                                                                           legend.key.size =  unit(0.4, "cm"))
    
  } else {
    
    title = ""
    
    if(!is.null(vibObj$n)){
      p <- p + ggtitle(sprintf('%s \nRHR = %.02f\nRP = %.02f', title, RHR, RPvalue))
    } else {
      p <- p + ggtitle(sprintf('%s \nRHR = %.02f\nRP = %.02f', title, RHR, RPvalue))
    }
    
    p <- p + xlab('Hazard Ratio') + ylab('-log10(p)') + theme_bw() + theme(plot.title = element_text(hjust = 0, size = 12),
                                                                           legend.position = legend_position,
                                                                           legend.text = element_text(size = 10),
                                                                           legend.key.size =  unit(0.4, "cm"))
    
  }
  
  if(true_beta){
    p <- p + geom_point(aes(x = exp(vibObj$true_beta[1]), y = -log10(vibObj$true_beta[6])), colour="black", shape = 4, size = 3)
    #p <- p + geom_point(aes(x = exp(vibObj$true_beta[1]), y = -log10(vibObj$true_beta[6])), colour="green")
  }
  
  
  return(p)
  
}




##############################################################
###   sim_RHR: RHR for different sample sizes + barplots   ###
##############################################################

### Input:
#
# vibobj_me     Object containing results of measurement vibration
# vibobj_mod    Object containing results of model vibration
# vibobj_sam    Object containing results of sampling vibration 
# RHR_max       Maximum RHR
#
### Output: List containing all plots and the legend for barplots

sim_RHR <- function(vibobj_me, vibobj_mod, vibobj_sam, RHR_max){
  
  
  me_RHR <- c(vibobj_me$vib_200000$RHR, vibobj_me$vib_100000$RHR, vibobj_me$vib_50000$RHR, vibobj_me$vib_10000$RHR,
              mean(unlist(lapply(vibobj_me$vib_5000, function(x) x$RHR))),
              mean(unlist(lapply(vibobj_me$vib_1000, function(x) x$RHR))),
              mean(unlist(lapply(vibobj_me$vib_500, function(x) x$RHR))))
  
  
  mod_RHR <- c(vibobj_mod$vib_200000$RHR, vibobj_mod$vib_100000$RHR, vibobj_mod$vib_50000$RHR, vibobj_mod$vib_10000$RHR,
               mean(unlist(lapply(vibobj_mod$vib_5000, function(x) x$RHR))),
               mean(unlist(lapply(vibobj_mod$vib_1000, function(x) x$RHR))),
               mean(unlist(lapply(vibobj_mod$vib_500, function(x) x$RHR))))
  
  
  sam_RHR <- c(vibobj_sam$vib_200000_15$RHR, vibobj_sam$vib_100000_15$RHR, vibobj_sam$vib_50000_15$RHR, 
               vibobj_sam$vib_10000_15$RHR,
               mean(unlist(lapply(vibobj_sam$vib_5000_15, function(x) x$RHR))),
               mean(unlist(lapply(vibobj_sam$vib_1000_15, function(x) x$RHR))),
               mean(unlist(lapply(vibobj_sam$vib_500_15, function(x) x$RHR))))
  
  unc_type = c(rep("Measurement", 7), rep("Model", 7), rep("Sampling", 7))
  x_numev <- c("200000", "100000", "50000", "10000", "5000", "1000", "500")
  legend_title <- paste("Type of vibration")
  
  df <- data.frame(RHR = c(me_RHR, mod_RHR, sam_RHR), n = rep(x_numev, 3), unc_type)
  
  
  
  
  p1 <- ggplot(ymin = 0, ymax = RHR_max) +
    geom_point(data = df, aes(y = RHR, x = as.factor(n), shape = unc_type), size = 3) +
    geom_line(data = df, aes(y = RHR, x = as.factor(n), group = unc_type, linetype = unc_type)) +
    scale_x_discrete(limits = c(paste(x_numev[7]), paste(x_numev[6]), paste(x_numev[5]),
                                paste(x_numev[4]), paste(x_numev[3]), paste(x_numev[2]),
                                paste(x_numev[1]))) +
    scale_y_continuous(limits = c(1, RHR_max)) +
    scale_linetype_manual(name = legend_title, values = c("solid", "twodash", "dotted")) +
    ylab("Relative Hazard Ratio (strength of vibration)\n") +
    xlab("Sample size") +
    scale_shape_manual(name = legend_title, values=c(0,1,2)) +
    theme(legend.position = c(0.7, 0.7),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.x  = element_text(size=12),
          axis.text.y  = element_text(size=12),
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          plot.title = element_text(size = 16))
  
  
  
  ###__________________________###
  ###   Type of significance   ###
  ###__________________________###
  
  sig_results <- tos(vibobj_me, vibobj_mod, vibobj_sam)
  
  ### Barplots (black and white)
  
  plot_500_bw <- ggplot(sig_results$df_500, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar(stat="identity", position = "fill") + #scale_fill_grey() +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    scale_fill_manual(values = c("red", "grey", "green")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  
  plot_1000_bw <- ggplot(sig_results$df_1000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar(stat="identity", position = "fill") + #scale_fill_grey() +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    scale_fill_manual(values = c("red", "grey", "green")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  
  plot_5000_bw <- ggplot(sig_results$df_5000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar( stat="identity", position = "fill") + #scale_fill_grey() +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    scale_fill_manual(values = c("red", "grey", "green")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  
  plot_10000_bw <- ggplot(sig_results$df_10000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar( stat="identity", position = "fill") + #scale_fill_grey() +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    scale_fill_manual(values = c("red", "grey", "green")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  plot_50000_bw <- ggplot(sig_results$df_50000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar( stat="identity", position = "fill") + #scale_fill_grey() +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    scale_fill_manual(values = c("red", "grey", "green")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  plot_100000_bw <- ggplot(sig_results$df_100000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar( stat="identity", position = "fill") + #scale_fill_grey() +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    scale_fill_manual(values = c("red", "grey", "green")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  plot_200000_bw <- ggplot(sig_results$df_200000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar( stat="identity", position = "fill") + #scale_fill_grey() +
    scale_fill_manual(values = c("red", "grey", "green")) +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696")) +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 90),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(),
                       axis.text = element_text(size = 8))
  
  # Legend
  
  
  plot_legend_bw <- ggplot(sig_results$df_200000, aes(fill=cond, y=value, x=vib_type)) +
    geom_bar( stat="identity", position = "fill") +
    #scale_fill_manual(values = c("#525252", "#d9d9d9", "#969696"),
    scale_fill_manual(values = c("red", "grey", "green"),
                      name = "Type of results",
                      breaks=c("neg_sig", "nonsig", "pos_sig"),
                      labels=c("Negative significant", "Non-significant", "Positive significant")) +
    theme_bw() + theme(axis.text.x = element_text(size = 12, angle = 45),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line())
  
  
  
  ###___________________###
  ###   Combine plots   ###
  ###___________________###
  
  legend_bw <- get_legend(plot_legend_bw)
  
  
  # Figure 1:
  

  return(list(p1 = p1, legend = legend_bw, plot_500 = plot_500_bw, plot_1000 = plot_1000_bw,
              plot_5000 = plot_5000_bw, plot_10000 = plot_10000_bw, plot_50000 = plot_50000_bw,
              plot_100000 = plot_100000_bw, plot_200000 = plot_200000_bw))
}




######################################################################
###   tos: Type of significance for the three types of vibration   ###
######################################################################

# subfunction of sim_RHR

### Input:
#
# vibobj_me     Object containing results of measurement vibration
# vibobj_mod    Object containing results of model vibration
# vibobj_sam    Object containing results of sampling vibration 
#
### Output:
#
# sig_results   List containing data frames with information about type of significance for
#               all sample sizes

tos <- function(vibobj_me, vibobj_mod, vibobj_sam){
  
  
  ### measurement vibration
  
  pos_200000_me <- which(vibobj_me$vib_200000$results$estimate > 0)
  neg_200000_me <- which(vibobj_me$vib_200000$results$estimate <= 0)
  
  pos_sig_200000_me <- sum(vibobj_me$vib_200000$results$pvalue[pos_200000_me] < 0.05, na.rm = T)
  neg_sig_200000_me <- sum(vibobj_me$vib_200000$results$pvalue[neg_200000_me] < 0.05, na.rm = T)
  nonsig_200000_me <- sum(vibobj_me$vib_200000$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_100000_me <- which(vibobj_me$vib_100000$results$estimate > 0)
  neg_100000_me <- which(vibobj_me$vib_100000$results$estimate <= 0)
  
  pos_sig_100000_me <- sum(vibobj_me$vib_100000$results$pvalue[pos_100000_me] < 0.05, na.rm = T)
  neg_sig_100000_me <- sum(vibobj_me$vib_100000$results$pvalue[neg_100000_me] < 0.05, na.rm = T)
  nonsig_100000_me <- sum(vibobj_me$vib_100000$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_50000_me <- which(vibobj_me$vib_50000$results$estimate > 0)
  neg_50000_me <- which(vibobj_me$vib_50000$results$estimate <= 0)
  
  pos_sig_50000_me <- sum(vibobj_me$vib_50000$results$pvalue[pos_50000_me] < 0.05, na.rm = T)
  neg_sig_50000_me <- sum(vibobj_me$vib_50000$results$pvalue[neg_50000_me] < 0.05, na.rm = T)
  nonsig_50000_me <- sum(vibobj_me$vib_50000$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_10000_me <- which(vibobj_me$vib_10000$results$estimate > 0)
  neg_10000_me <- which(vibobj_me$vib_10000$results$estimate <= 0)
  
  pos_sig_10000_me <- sum(vibobj_me$vib_10000$results$pvalue[pos_10000_me] < 0.05, na.rm = T)
  neg_sig_10000_me <- sum(vibobj_me$vib_10000$results$pvalue[neg_10000_me] < 0.05, na.rm = T)
  nonsig_10000_me <- sum(vibobj_me$vib_10000$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_5000_me <- lapply(vibobj_me$vib_5000, function(x) which(x$results$estimate > 0))
  neg_5000_me <- lapply(vibobj_me$vib_5000, function(x) which(x$results$estimate <= 0))
  
  pos_sig_5000_me <- neg_sig_5000_me <- nonsig_5000_me <- list()
  
  for(i in 1:10){
    pos_sig_5000_me[[i]] <- sum(vibobj_me$vib_5000[[i]]$results$pvalue[pos_5000_me[[i]]] < 0.05, na.rm = T)
    neg_sig_5000_me[[i]] <- sum(vibobj_me$vib_5000[[i]]$results$pvalue[neg_5000_me[[i]]] < 0.05, na.rm = T)
    nonsig_5000_me[[i]] <- sum(vibobj_me$vib_5000[[i]]$results$pvalue >= 0.05, na.rm = T)
  }
  
  
  pos_1000_me <- lapply(vibobj_me$vib_1000, function(x) which(x$results$estimate > 0))
  neg_1000_me <- lapply(vibobj_me$vib_1000, function(x) which(x$results$estimate <= 0))
  
  pos_sig_1000_me <- neg_sig_1000_me <- nonsig_1000_me <- list()
  
  for(i in 1:10){
    pos_sig_1000_me[[i]] <- sum(vibobj_me$vib_1000[[i]]$results$pvalue[pos_1000_me[[i]]] < 0.05, na.rm = T)
    neg_sig_1000_me[[i]] <- sum(vibobj_me$vib_1000[[i]]$results$pvalue[neg_1000_me[[i]]] < 0.05, na.rm = T)
    nonsig_1000_me[[i]] <- sum(vibobj_me$vib_1000[[i]]$results$pvalue >= 0.05, na.rm = T)
  }
  
  
  pos_500_me <- lapply(vibobj_me$vib_500, function(x) which(x$results$estimate > 0))
  neg_500_me <- lapply(vibobj_me$vib_500, function(x) which(x$results$estimate <= 0))
  
  pos_sig_500_me <- neg_sig_500_me <- nonsig_500_me <- list()
  
  for(i in 1:10){
    pos_sig_500_me[[i]] <- sum(vibobj_me$vib_500[[i]]$results$pvalue[pos_500_me[[i]]] < 0.05, na.rm = T)
    neg_sig_500_me[[i]] <- sum(vibobj_me$vib_500[[i]]$results$pvalue[neg_500_me[[i]]] < 0.05, na.rm = T)
    nonsig_500_me[[i]] <- sum(vibobj_me$vib_500[[i]]$results$pvalue >= 0.05, na.rm = T)
  } 
  
  ###   Model vibration
  
  pos_200000_mod <- which(vibobj_mod$vib_200000$vibFrame$estimate > 0)
  neg_200000_mod <- which(vibobj_mod$vib_200000$vibFrame$estimate <= 0)
  
  pos_sig_200000_mod <- sum(vibobj_mod$vib_200000$vibFrame$pvalue[pos_200000_mod] < 0.05, na.rm = T)
  neg_sig_200000_mod <- sum(vibobj_mod$vib_200000$vibFrame$pvalue[neg_200000_mod] < 0.05, na.rm = T)
  nonsig_200000_mod <- sum(vibobj_mod$vib_200000$vibFrame$pvalue >= 0.05, na.rm = T)
  
  
  pos_100000_mod <- which(vibobj_mod$vib_100000$vibFrame$estimate > 0)
  neg_100000_mod <- which(vibobj_mod$vib_100000$vibFrame$estimate <= 0)
  
  pos_sig_100000_mod <- sum(vibobj_mod$vib_100000$vibFrame$pvalue[pos_100000_mod] < 0.05, na.rm = T)
  neg_sig_100000_mod <- sum(vibobj_mod$vib_100000$vibFrame$pvalue[neg_100000_mod] < 0.05, na.rm = T)
  nonsig_100000_mod <- sum(vibobj_mod$vib_100000$vibFrame$pvalue >= 0.05, na.rm = T)
  
  
  pos_50000_mod <- which(vibobj_mod$vib_50000$vibFrame$estimate > 0)
  neg_50000_mod <- which(vibobj_mod$vib_50000$vibFrame$estimate <= 0)
  
  pos_sig_50000_mod <- sum(vibobj_mod$vib_50000$vibFrame$pvalue[pos_50000_mod] < 0.05, na.rm = T)
  neg_sig_50000_mod <- sum(vibobj_mod$vib_50000$vibFrame$pvalue[neg_50000_mod] < 0.05, na.rm = T)
  nonsig_50000_mod <- sum(vibobj_mod$vib_50000$vibFrame$pvalue >= 0.05, na.rm = T)
  
  
  pos_10000_mod <- which(vibobj_mod$vib_10000$vibFrame$estimate > 0)
  neg_10000_mod <- which(vibobj_mod$vib_10000$vibFrame$estimate <= 0)
  
  pos_sig_10000_mod <- sum(vibobj_mod$vib_10000$vibFrame$pvalue[pos_10000_mod] < 0.05, na.rm = T)
  neg_sig_10000_mod <- sum(vibobj_mod$vib_10000$vibFrame$pvalue[neg_10000_mod] < 0.05, na.rm = T)
  nonsig_10000_mod <- sum(vibobj_mod$vib_10000$vibFrame$pvalue >= 0.05, na.rm = T)
  
  
  pos_5000_mod <- lapply(vibobj_mod$vib_5000, function(x) which(x$vibFrame$estimate > 0))
  neg_5000_mod <- lapply(vibobj_mod$vib_5000, function(x) which(x$vibFrame$estimate <= 0))
  
  pos_sig_5000_mod <- neg_sig_5000_mod <- nonsig_5000_mod <- list()
  
  for(i in 1:10){
    pos_sig_5000_mod[[i]] <- sum(vibobj_mod$vib_5000[[i]]$vibFrame$pvalue[pos_5000_mod[[i]]] < 0.05, na.rm = T)
    neg_sig_5000_mod[[i]] <- sum(vibobj_mod$vib_5000[[i]]$vibFrame$pvalue[neg_5000_mod[[i]]] < 0.05, na.rm = T)
    nonsig_5000_mod[[i]] <- sum(vibobj_mod$vib_5000[[i]]$vibFrame$pvalue >= 0.05, na.rm = T)
  }
  
  
  pos_1000_mod <- lapply(vibobj_mod$vib_1000, function(x) which(x$vibFrame$estimate > 0))
  neg_1000_mod <- lapply(vibobj_mod$vib_1000, function(x) which(x$vibFrame$estimate <= 0))
  
  pos_sig_1000_mod <- neg_sig_1000_mod <- nonsig_1000_mod <- list()
  
  for(i in 1:10){
    pos_sig_1000_mod[[i]] <- sum(vibobj_mod$vib_1000[[i]]$vibFrame$pvalue[pos_1000_mod[[i]]] < 0.05, na.rm = T)
    neg_sig_1000_mod[[i]] <- sum(vibobj_mod$vib_1000[[i]]$vibFrame$pvalue[neg_1000_mod[[i]]] < 0.05, na.rm = T)
    nonsig_1000_mod[[i]] <- sum(vibobj_mod$vib_1000[[i]]$vibFrame$pvalue >= 0.05, na.rm = T)
  }
  
  
  pos_500_mod <- lapply(vibobj_mod$vib_500, function(x) which(x$vibFrame$estimate > 0))
  neg_500_mod <- lapply(vibobj_mod$vib_500, function(x) which(x$vibFrame$estimate <= 0))
  
  pos_sig_500_mod <- neg_sig_500_mod <- nonsig_500_mod <- list()
  
  for(i in 1:10){
    pos_sig_500_mod[[i]] <- sum(vibobj_mod$vib_500[[i]]$vibFrame$pvalue[pos_500_mod[[i]]] < 0.05, na.rm = T)
    neg_sig_500_mod[[i]] <- sum(vibobj_mod$vib_500[[i]]$vibFrame$pvalue[neg_500_mod[[i]]] < 0.05, na.rm = T)
    nonsig_500_mod[[i]] <- sum(vibobj_mod$vib_500[[i]]$vibFrame$pvalue >= 0.05, na.rm = T)
  } 
  
  
  ###   Sampling vibration
  
  pos_200000_sam <- which(vibobj_sam$vib_200000_15$results$estimate > 0)
  neg_200000_sam <- which(vibobj_sam$vib_200000_15$results$estimate <= 0)
  
  pos_sig_200000_sam <- sum(vibobj_sam$vib_200000_15$results$pvalue[pos_200000_sam] < 0.05, na.rm = T)
  neg_sig_200000_sam <- sum(vibobj_sam$vib_200000_15$results$pvalue[neg_200000_sam] < 0.05, na.rm = T)
  nonsig_200000_sam <- sum(vibobj_sam$vib_200000_15$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_100000_sam <- which(vibobj_sam$vib_100000_15$results$estimate > 0)
  neg_100000_sam <- which(vibobj_sam$vib_100000_15$results$estimate <= 0)
  
  pos_sig_100000_sam <- sum(vibobj_sam$vib_100000_15$results$pvalue[pos_100000_sam] < 0.05, na.rm = T)
  neg_sig_100000_sam <- sum(vibobj_sam$vib_100000_15$results$pvalue[neg_100000_sam] < 0.05, na.rm = T)
  nonsig_100000_sam <- sum(vibobj_sam$vib_100000_15$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_50000_sam <- which(vibobj_sam$vib_50000_15$results$estimate > 0)
  neg_50000_sam <- which(vibobj_sam$vib_50000_15$results$estimate <= 0)
  
  pos_sig_50000_sam <- sum(vibobj_sam$vib_50000_15$results$pvalue[pos_50000_sam] < 0.05, na.rm = T)
  neg_sig_50000_sam <- sum(vibobj_sam$vib_50000_15$results$pvalue[neg_50000_sam] < 0.05, na.rm = T)
  nonsig_50000_sam <- sum(vibobj_sam$vib_50000_15$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_10000_sam <- which(vibobj_sam$vib_10000_15$results$estimate > 0)
  neg_10000_sam <- which(vibobj_sam$vib_10000_15$results$estimate <= 0)
  
  pos_sig_10000_sam <- sum(vibobj_sam$vib_10000_15$results$pvalue[pos_10000_sam] < 0.05, na.rm = T)
  neg_sig_10000_sam <- sum(vibobj_sam$vib_10000_15$results$pvalue[neg_10000_sam] < 0.05, na.rm = T)
  nonsig_10000_sam <- sum(vibobj_sam$vib_10000_15$results$pvalue >= 0.05, na.rm = T)
  
  
  pos_5000_sam <- lapply(vibobj_sam$vib_5000_15, function(x) which(x$results$estimate > 0))
  neg_5000_sam <- lapply(vibobj_sam$vib_5000_15, function(x) which(x$results$estimate <= 0))
  
  pos_sig_5000_sam <- neg_sig_5000_sam <- nonsig_5000_sam <- list()
  
  for(i in 1:10){
    pos_sig_5000_sam[[i]] <- sum(vibobj_sam$vib_5000_15[[i]]$results$pvalue[pos_5000_sam[[i]]] < 0.05, na.rm = T)
    neg_sig_5000_sam[[i]] <- sum(vibobj_sam$vib_5000_15[[i]]$results$pvalue[neg_5000_sam[[i]]] < 0.05, na.rm = T)
    nonsig_5000_sam[[i]] <- sum(vibobj_sam$vib_5000_15[[i]]$results$pvalue >= 0.05, na.rm = T)
  }
  
  
  pos_1000_sam <- lapply(vibobj_sam$vib_1000_15, function(x) which(x$results$estimate > 0))
  neg_1000_sam <- lapply(vibobj_sam$vib_1000_15, function(x) which(x$results$estimate <= 0))
  
  pos_sig_1000_sam <- neg_sig_1000_sam <- nonsig_1000_sam <- list()
  
  for(i in 1:10){
    pos_sig_1000_sam[[i]] <- sum(vibobj_sam$vib_1000_15[[i]]$results$pvalue[pos_1000_sam[[i]]] < 0.05, na.rm = T)
    neg_sig_1000_sam[[i]] <- sum(vibobj_sam$vib_1000_15[[i]]$results$pvalue[neg_1000_sam[[i]]] < 0.05, na.rm = T)
    nonsig_1000_sam[[i]] <- sum(vibobj_sam$vib_1000_15[[i]]$results$pvalue >= 0.05, na.rm = T)
  }
  
  
  pos_500_sam <- lapply(vibobj_sam$vib_500_15, function(x) which(x$results$estimate > 0))
  neg_500_sam <- lapply(vibobj_sam$vib_500_15, function(x) which(x$results$estimate <= 0))
  
  pos_sig_500_sam <- neg_sig_500_sam <- nonsig_500_sam <- list()
  
  for(i in 1:10){
    pos_sig_500_sam[[i]] <- sum(vibobj_sam$vib_500_15[[i]]$results$pvalue[pos_500_sam[[i]]] < 0.05, na.rm = T)
    neg_sig_500_sam[[i]] <- sum(vibobj_sam$vib_500_15[[i]]$results$pvalue[neg_500_sam[[i]]] < 0.05, na.rm = T)
    nonsig_500_sam[[i]] <- sum(vibobj_sam$vib_500_15[[i]]$results$pvalue >= 0.05, na.rm = T)
  } 
  
  
  ### Create data frames
  
  df_500 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                       cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                       value = c(mean(unlist(pos_sig_500_me)),
                                 mean(unlist(nonsig_500_me)), mean(unlist(neg_sig_500_me)),
                                 mean(unlist(pos_sig_500_mod)), mean(unlist(nonsig_500_mod)),
                                 mean(unlist(neg_sig_500_mod)),
                                 mean(unlist(pos_sig_500_sam)), mean(unlist(nonsig_500_sam)),
                                 mean(unlist(neg_sig_500_sam))))
  
  
  df_1000 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                        cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                        value = c(mean(unlist(pos_sig_1000_me)),
                                  mean(unlist(nonsig_1000_me)), mean(unlist(neg_sig_1000_me)),
                                  mean(unlist(pos_sig_1000_mod)), mean(unlist(nonsig_1000_mod)),
                                  mean(unlist(neg_sig_1000_mod)),
                                  mean(unlist(pos_sig_1000_sam)), mean(unlist(nonsig_1000_sam)),
                                  mean(unlist(neg_sig_1000_sam))))
  
  
  df_5000 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                        cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                        value = c(mean(unlist(pos_sig_5000_me)),
                                  mean(unlist(nonsig_5000_me)), mean(unlist(neg_sig_5000_me)),
                                  mean(unlist(pos_sig_5000_mod)), mean(unlist(nonsig_5000_mod)),
                                  mean(unlist(neg_sig_5000_mod)),
                                  mean(unlist(pos_sig_5000_sam)), mean(unlist(nonsig_5000_sam)),
                                  mean(unlist(neg_sig_5000_sam))))
  
  
  df_10000 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                         cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                         value = c(pos_sig_10000_me, nonsig_10000_me, neg_sig_10000_me,
                                   pos_sig_10000_mod, nonsig_10000_mod, neg_sig_10000_mod,
                                   pos_sig_10000_sam, nonsig_10000_sam, neg_sig_10000_sam))
  
  df_50000 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                         cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                         value = c(pos_sig_50000_me, nonsig_50000_me, neg_sig_50000_me,
                                   pos_sig_50000_mod, nonsig_50000_mod, neg_sig_50000_mod,
                                   pos_sig_50000_sam, nonsig_50000_sam, neg_sig_50000_sam))
  
  df_100000 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                          cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                          value = c(pos_sig_100000_me, nonsig_100000_me, neg_sig_100000_me,
                                    pos_sig_100000_mod, nonsig_100000_mod, neg_sig_100000_mod,
                                    pos_sig_100000_sam, nonsig_100000_sam, neg_sig_100000_sam))
  
  df_200000 <- data.frame(vib_type = c(rep("Measurement", 3), rep("Model", 3), rep("Sampling", 3)),
                          cond = rep(c("pos_sig", "nonsig", "neg_sig"), 3),
                          value = c(pos_sig_200000_me, nonsig_200000_me, neg_sig_200000_me,
                                    pos_sig_200000_mod, nonsig_200000_mod, neg_sig_200000_mod,
                                    pos_sig_200000_sam, nonsig_200000_sam, neg_sig_200000_sam))
  
  
  df_500$vib_type <- as.factor(df_500$vib_type)
  df_500$vib_type <- factor(df_500$vib_type, levels = c("Measurement", "Model", "Sampling"),
                            labels = c("Measurement", "Model", "Sampling"))
  
  df_1000$vib_type <- as.factor(df_1000$vib_type)
  df_1000$vib_type <- factor(df_1000$vib_type, levels = c("Measurement", "Model", "Sampling"),
                             labels = c("Measurement", "Model", "Sampling"))
  
  df_5000$vib_type <- as.factor(df_5000$vib_type)
  df_5000$vib_type <- factor(df_5000$vib_type, levels = c("Measurement", "Model", "Sampling"),
                             labels = c("Measurement", "Model", "Sampling"))
  
  
  df_10000$vib_type <- as.factor(df_10000$vib_type)
  df_10000$vib_type <- factor(df_10000$vib_type, levels = c("Measurement", "Model", "Sampling"),
                              labels = c("Measurement", "Model", "Sampling"))
  
  
  df_50000$vib_type <- as.factor(df_50000$vib_type)
  df_50000$vib_type <- factor(df_50000$vib_type, levels = c("Measurement", "Model", "Sampling"),
                              labels = c("Measurement", "Model", "Sampling"))
  
  
  df_100000$vib_type <- as.factor(df_100000$vib_type)
  df_100000$vib_type <- factor(df_100000$vib_type, levels = c("Measurement", "Model", "Sampling"),
                               labels = c("Measurement", "Model", "Sampling"))
  
  df_200000$vib_type <- as.factor(df_200000$vib_type)
  df_200000$vib_type <- factor(df_200000$vib_type, levels = c("Measurement", "Model", "Sampling"),
                               labels = c("Measurement", "Model", "Sampling"))
  
  sig_results <- list(df_500 = df_500, df_1000 = df_1000, df_5000 = df_5000,
                      df_10000 = df_10000, df_50000 = df_50000, df_100000 = df_100000,
                      df_200000 = df_200000)
  
  return(sig_results)
  
}