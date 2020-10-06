# Functions for the assessment of measurement vibration on real data

# me_fun: Main function in order to assess measurement vibration on real data
#   weight_function: subfunction of me_fun; calculating weights for cox model
#   recomputesamplePvalue: subfunction of me_fun; calculate the exact p-value



#######################################################################################
###   me_fun: Main function in order to assess measurement vibration on real data   ###
#######################################################################################

### Input:
#
# data              Data frame containing the NHANES data
# ord_var           Ordinal variable obtained with me_ordinal_new
# voi               Character specifying variable of interest
# voi_type          Type of variable of interest, either 'Bin' or 'Num'
# me_type           Character specifying the variables with measurement error with possible options all, voi, and adj
# me_range          Ranges of correlation, sensitivity and specificity
# adj_var           Character vector specifying the adjustment variables
# adj_type          Character vector specifying the type of adjustment variables with options 'Bin', 'Num', 'Ord'
# B                 Number of iterations
# print_progress    Logical, whether the iteration number shall be printed, default is FALSE

### Output:
#
# results         Data frame with results for all iterations
# RHR             Relative hazard ratio
# RP              Relative p-value
# name            Name of the variable of interest
# n               Number of observations in the data
# numev           Number of events in the data
# dir_neg         Number of iterations with results in negative direction
# dir_pos         Number of iterations with results in positive direction
# warnings        Vector indicating runs that caused a warning
# true_beta       Coefficient and p-value for the model without measurement error


me_fun <- function(data, ord_var, voi, voi_type, me_type = "all", me_range, adj_var, adj_type, 
                   B = 1000, print_progress = F){
  
  data_raw <- data
  
  data_raw <- weight_function(data_raw, paste(voi))
  
    if(voi %in% adj_var){
      voi_ind <- which(adj_var == paste(voi))
      adj_var <- adj_var[-voi_ind]
      adj_type <- adj_type[-voi_ind]
    }
  
  data <- data_raw[,  c('PERMTH_EXM', 'MORTSTAT', paste(voi), adj_var, 'weight', 'area')]
  colnames(data)[which(colnames(data) == paste(voi))] <- "voi"

  
  if(voi_type == "Num"){
    data[,'voi'] <- scale(data[,'voi'])
  }

  
  data$physical_activity <- as.factor(data$physical_activity)
  data$RIDRETH1 <- as.factor(data$RIDRETH1)
  data$education <- as.factor(data$education)
  data$SES_LEVEL <- as.factor(data$SES_LEVEL)
  data$bmi <- as.factor(data$bmi)
  
  basemodel <- as.formula(paste("Surv(PERMTH_EXM, MORTSTAT) ~ voi + ", paste(adj_var, collapse = "+"), "+ cluster(area)"))
  true_beta <- summary(do.call("coxph", list(basemodel, weights = data$weight, data = data)))
  
  voi_index <- grep("voi", rownames(true_beta$coefficients))
  true_beta <- true_beta$coefficients[voi_index,]
  
  
  n <- dim(data)[1]
  
  df <- data.frame(estimate = rep (NA, B), HR = rep(NA, B), pvalue = rep(NA, B), z = rep(NA, B))
  fail <- numeric(B)
  
  if(me_type == "all"){
    adj_type[which(adj_var == "RIDRETH1")] <- "None"
    adj_type[which(adj_var == "RIDAGEYR")] <- "None"
    adj_type[which(adj_var == "male")] <- "None"
    # adj_type[which(adj_var == "weight")] <- "None"
    # adj_type[which(adj_var == "area")] <- "None"
  } else if(me_type == "voi"){
    adj_type <- rep("None", length(adj_type))
  } else if(me_type == "adj"){
    adj_type[which(adj_var == "RIDRETH1")] <- "None"
    adj_type[which(adj_var == "RIDAGEYR")] <- "None"
    adj_type[which(adj_var == "male")] <- "None"
    # adj_type[which(adj_var == "weight")] <- "None"
    # adj_type[which(adj_var == "area")] <- "None"
    voi_type <- "None"
  }
  
  var_type <- c(voi_type, adj_type)
  if(!length(var_type) == (dim(data)[2] - 4)){
    stop("An error occured")
  }
  
  for(i in 1:B){
    
    if(print_progress){
      print(i)
    }
    
    data_var <- data[, 3:(ncol(data)-2)]
    
    me_num <- runif(1, me_range$correlation[1], me_range$correlation[2])
    
    ### numeric:
    num_ind <- which(var_type == "Num")
    
    #data_var[,num_ind] <- scale(data_var[,num_ind])
    
    for(k in num_ind){
      
      k_var <- var(data_var[,k], na.rm = T)
      #proz_wahr <-  me_num
      proz_wahr <- me_num
      tot_var <- k_var/proz_wahr
      sd_to_use <-  sqrt(tot_var - k_var)
      
      data_var[,k] <- data_var[,k] + rnorm(n, 0, sd_to_use)
      
    }
    
    ### binary:
    
    bin_ind <- which(var_type == "Bin")
    
    me_sens <- runif(1, me_range$sensitivity[1], me_range$sensitivity[2])
    me_spec <- runif(1, me_range$specificity[1], me_range$specificity[2])
    
    for(k in bin_ind){
      
      tempvec <- rep(NA, n)
      
      bin_var_levels <- levels(as.factor((data_var[,k])))
      
      var_lev2 <- which(data_var[,k] == bin_var_levels[2])
      rvec1 <- rbinom(length(var_lev2), 1, me_sens)
      tempvec[var_lev2] <- as.numeric(bin_var_levels[1]) + rvec1
      
      var_lev1 <- which(data_var[,k] == bin_var_levels[1])
      rvec2 <- rbinom(length(var_lev1), 1, me_spec)
      tempvec[var_lev1] <- as.numeric(bin_var_levels[2]) - rvec2
      
      data_var[,k] <- as.factor(tempvec)
      
    }
    
    ### ordinal:
    
    if(me_type != "voi"){
      ordinal_names <- lapply(ord_var, FUN = function(x) x$var_name)
      bmi_ind <- which(ordinal_names == "bmi")
      phys_act_ind <- which(ordinal_names == "physical_activity")
      education_ind <- which(ordinal_names == "education")
      SES_LEVEL_ind <- which(ordinal_names == "SES_LEVEL")
      
      proz_wahr <- me_num
      
      var_var_bmi <- var(ord_var[[bmi_ind]]$new_var, na.rm = T)
      tot_var_bmi <- var_var_bmi/proz_wahr
      var_sd_bmi <-  sqrt(tot_var_bmi - var_var_bmi)
      
      data_var[,'bmi'] <-  me_ord_sim(var_name = "bmi",
                                      n = ord_var[[bmi_ind]]$n,
                                      new_var = ord_var[[bmi_ind]]$new_var,
                                      var_sd = var_sd_bmi,
                                      quant = ord_var[[bmi_ind]]$quant,
                                      cuts = ord_var[[bmi_ind]]$cuts)
      
      
      var_var_phys_act <- var(ord_var[[phys_act_ind]]$new_var, na.rm = T)
      tot_var_phys_act <- var_var_phys_act/proz_wahr
      var_sd_phys_act <-  sqrt(tot_var_phys_act - var_var_phys_act)
      
      data_var[,'physical_activity'] <-  me_ord_sim(var_name = "physical_activity",
                                                    n = ord_var[[phys_act_ind]]$n,
                                                    new_var = ord_var[[phys_act_ind]]$new_var,
                                                    var_sd = var_sd_phys_act,
                                                    quant = ord_var[[phys_act_ind]]$quant,
                                                    cuts = ord_var[[phys_act_ind]]$cuts)
      
      
      var_var_SES_LEVEL <- var(ord_var[[SES_LEVEL_ind]]$new_var, na.rm = T)
      tot_var_SES_LEVEL <- var_var_SES_LEVEL/proz_wahr
      var_sd_SES_LEVEL <-  sqrt(tot_var_SES_LEVEL - var_var_SES_LEVEL)
      
      data_var[,'SES_LEVEL'] <-  me_ord_sim(var_name = "SES_LEVEL",
                                            n = ord_var[[SES_LEVEL_ind]]$n,
                                            new_var = ord_var[[SES_LEVEL_ind]]$new_var,
                                            var_sd = var_sd_SES_LEVEL,
                                            quant = ord_var[[SES_LEVEL_ind]]$quant,
                                            cuts = ord_var[[SES_LEVEL_ind]]$cuts)
      
      
      var_var_edu <- var(ord_var[[education_ind]]$new_var, na.rm = T)
      tot_var_edu <- var_var_edu/proz_wahr
      var_sd_edu <-  sqrt(tot_var_edu - var_var_edu)
      
      data_var[,'education'] <-  me_ord_sim(var_name = "education",
                                            n = ord_var[[education_ind]]$n,
                                            new_var = ord_var[[education_ind]]$new_var,
                                            var_sd = var_sd_edu,
                                            quant = ord_var[[education_ind]]$quant,
                                            cuts = ord_var[[education_ind]]$cuts)
      
      
      data_var$education <- as.factor(data_var$education)
      data_var$SES_LEVEL <- as.factor(data_var$SES_LEVEL)
      data_var$bmi <- as.factor(data_var$bmi)
      data_var$physical_activity <- as.factor(data_var$physical_activity)
    }
    
    
    # calculate model:
    
    outcome <- Surv(time = data$PERMTH_EXM, event = data$MORTSTAT)
    
    formula <- paste("outcome ~ voi + ", paste(adj_var, collapse = " + "), " + cluster(area)", sep="")
    
    data_me <- data.frame(outcome, data_var, weight = data$weight, area = data$area)
    
    tryCatch(mod <- summary(do.call("coxph", list(as.formula(formula), weights = data$weight, 
                                                  data = data_me,
                                                  control = list(eps = 1e-05, iter.max = 50,
                                                                 timefix = T,
                                                                 toler.chol = .Machine$double.eps^0.75,
                                                                 toler.inf = sqrt(1e-01))
    ))),
    
    warning = function(w) {
      print(w)
      mod <<- NULL
      fail[i] <<- 1
    })
    
    # save model results in a data frame
    
    if(!is.null(mod)){
      df[i,1] <- mod$coefficients[1,1] # estimate
      df[i,2] <- mod$coefficients[1,2] # HR
      if(!mod$used.robust){
        df[i,3] <- mod$coefficients[1,5] # p-value
        df[i,4] <- mod$coefficients[1,4] # z
      } else {
        df[i,3] <- mod$coefficients[1,6] # p-value
        df[i,4] <- mod$coefficients[1,5] # z
      }
    }
    
  }
  
  
  # Calculate summary measures:
  
  dir_neg <- sum(df$estimate < 0, na.rm = T)
  dir_pos <- sum(df$estimate > 0, na.rm = T)
  
  qhr <- quantile(df[,2], probs = c(0.01, 0.99), na.rm = T)
  RHR <- qhr[2]/qhr[1]
  
  df <- recomputesamplePvalue(df)
  
  
  logtenp <- -log10(df[,3])
  
  qp <- quantile(logtenp, probs = c(0.01, 0.99), na.rm = T)
  RP <- qp[2] - qp[1]
  
  erg <- list(results = df, RHR = RHR, RP = RP, name = voi,
              n = n, numev = sum(outcome[,2]),
              dir_neg = dir_neg, dir_pos = dir_pos, warnings = fail, true_beta = true_beta)
  
  return(erg)
  
}  


###__________________________________________________________________________###
###   weight_function: Calculating appropriate weights for the Nhanes data   ###
###__________________________________________________________________________###


# Subfunction of me_fun

### Input:
#
# mainTab     Data frame containing original nhanes data
# var         Character, name of the variable of interest

### Output:
#
# mainTab     mainTab with additional variable 'weight'

weight_function <- function(mainTab, var){
  
  mainTab$seriesStr <- factor(mainTab$SDDSRVYR, labels = c('1999-2000', '2001-2002', '2003-2004'))
  
  var_index <- which(colnames(mainTab) == paste(var))
  
  a <- which(is.na(mainTab[,var_index]))
  mainTab_mod <- mainTab[-a,]
  
  mainTab[, 'weight'] <- NA
  seriesStr <- table(droplevels(mainTab_mod$seriesStr))
  
  if(length(seriesStr) == 3) {
    
    mainTab[mainTab$SDDSRVYR==3, 'weight'] <- mainTab[mainTab$SDDSRVYR==3, 'WTMEC2YR']*(1/3)
    mainTab[mainTab$SDDSRVYR<3, 'weight'] <- mainTab[mainTab$SDDSRVYR<3, 'WTMEC4YR']*(2/3)
    
  } else if(length(seriesStr) == 2) {
    if(sum(names(seriesStr) %in% c('1999-2000', '2001-2002'))==2) {
      mainTab[, 'weight'] <- mainTab[, 'WTMEC4YR']
    }
    else if(sum(names(seriesStr) %in% c('1999-2000', '2003-2004'))==2) {
      mainTab[, 'weight'] <- mainTab[, 'WTMEC2YR']*(1/2)
    }
    else if(sum(names(seriesStr) %in% c('2001-2002', '2003-2004'))==2) {
      mainTab[, 'weight'] <- mainTab[, 'WTMEC2YR']*(1/2)
    }
  } else {
    mainTab[, 'weight'] <- mainTab[, 'WTMEC2YR']
  }
  
  return(mainTab)
  
}


###________________________________________________________###
###   recomputesamplePvalue: Calculate the exact p-value   ###
###________________________________________________________###

# Subfunction of me_fun

recomputesamplePvalue <- function(df) {
  zeroPval <- !is.na(df[,3]) & (df[,3] == 0)
  df[zeroPval, 3] <- pnorm(abs(df[zeroPval, 4]), lower.tail=F)*2
  return(df)
}