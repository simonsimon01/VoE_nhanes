# Functions for the assessment of measurement vibration on simulated data
#
# me_vib_sim: Measurement vibration for simulated data with different numbers of observations
#   me_fun: subfunction of me_vib_sim; calculate measurement vibration for one data set
#     recomputesamplePvalue: subfunction of me_fun; calculate the exact p-value


#######################################################################################################
###   me_vib_sim: Measurement vibration for simulated data with different numbers of observations   ###
#######################################################################################################

### Input:
#
# data        Simulated data with different numbers of observations obtained from data_for_mv
# ord_data    Ordinal data with different numbers of observations obtained from ord_data_for_mv
# voi         Character specifying variable of interest
# voi_type    Type of variable of interest, either 'Bin' or 'Num'
# adj_var     Character vector specifying the adjustment variables
# adj_type    Character vector specifying the type of adjustment variables with options 'Bin', 'Num', 'Ord'
# me_range    Ranges of correlation, sensitivity and specificity
# me_type     Specifying the variables with measurement error, in this study always 'all'
# B           Number of iterations, default is 1000

### Output:
#
# List with results of me_fun for seven different sizes



me_vib_sim <- function(data, ord_data, voi, voi_type, adj_var, adj_type, me_range, 
                       me_type = "all", B = 1000){
  

  
  vib_500 <- vib_1000 <- vib_5000 <- list()
  
  for(i in 1:10){
    
    print(i)

    print("n = 500")
    vib_500[[i]] <- me_fun(data_new = data$gen_data_500[[i]], adj_var = adj_var, adj_type = adj_type, 
                           voi = paste(voi), voi_type = voi_type, me_range = me_range,
                           B = B, ord_var = ord_data$ord_var_500[[i]], me_type = me_type, print_progress = F)
    

    print("n = 1000")
    vib_1000[[i]] <- me_fun(data_new = data$gen_data_1000[[i]], adj_var = adj_var, adj_type = adj_type, 
                            voi = paste(voi), voi_type = voi_type, me_range = me_range,
                            B = B, ord_var = ord_data$ord_var_1000[[i]], me_type = me_type, print_progress = F)
    
    
    print("n = 5000")
    vib_5000[[i]] <- me_fun(data_new = data$gen_data_5000[[i]], adj_var = adj_var, adj_type = adj_type, 
                            voi = paste(voi), voi_type = voi_type, me_range = me_range,
                            B = B, ord_var = ord_data$ord_var_5000[[i]], me_type = me_type, print_progress = F)
    
  }
  
  
  ###   10000
  
  print("n = 10000")
  

  vib_10000 <- me_fun(data_new = data$gen_data_10000, adj_var = adj_var, adj_type = adj_type, 
                      voi = paste(voi), voi_type = voi_type, me_range = me_range,
                      B = B, ord_var = ord_data$ord_var_10000, me_type = me_type, print_progress = F)
  
  ###   50000
  
  print("n = 50000")
  
  vib_50000 <- me_fun(data_new = data$gen_data_50000, adj_var = adj_var, adj_type = adj_type, 
                      voi = paste(voi), voi_type = voi_type, me_range = me_range,
                      B = B, ord_var = ord_data$ord_var_50000, me_type = me_type, print_progress = F)
  
  ###   100000
  
  print("n = 100000")
  
  vib_100000 <- me_fun(data_new = data$gen_data_100000, adj_var = adj_var, adj_type = adj_type, 
                        voi = paste(voi), voi_type = voi_type, me_range = me_range,
                        B = B, ord_var = ord_data$ord_var_100000, me_type = me_type, print_progress = F)
  
  ###   200000
  
  print("n = 200000")
  
  vib_200000 <- me_fun(data_new = data$gen_data_200000, adj_var = adj_var, adj_type = adj_type, 
                       voi = paste(voi), voi_type = voi_type, me_range = me_range,
                       B = B, ord_var = ord_data$ord_var_200000, me_type = me_type, print_progress = F)
  
  
  
  output <- list(vib_500 = vib_500, vib_1000 = vib_1000, vib_5000 = vib_5000, 
                 vib_10000 = vib_10000, vib_50000 = vib_50000, vib_100000 = vib_100000,
                 vib_200000 = vib_200000)
  
  return(output)
  
}


###_______________________________________________________________###
###   me_fun: Calculate measurement vibration for simulated data  ###
###_______________________________________________________________###

# subfunction of me_fun

### Input:
#
# data_new            Data set containing simulated data
# voi                 Character specifying variable of interest
# voi_type            Type of variable of interest, either 'Bin' or 'Num'
# adj_var             Character vector specifying the adjustment variables
# adj_type            Character vector specifying the type of adjustment variables with options 'Bin', 'Num', 'Ord'
# me_range            Ranges of correlation, sensitivity and specificity
# B                   Number of iterations
# ord_var             Ordinal data obtained with ord_data_for_mv
# me_type             Character specifying the variables with measurement error, in this study always 'all'
# print_progress      Logical, whether the iteration number shall be printed, default is FALSE

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


me_fun <- function(data_new, voi, voi_type, adj_var, adj_type, me_range, B, ord_var, me_type = me_type,
                   print_progress = F){
  
  data <- data_new
  
  true_beta <- coxph(Y ~ ., data = data)
  voi_index <- grep(paste(voi), rownames(summary(true_beta)$coefficients))
  true_beta <- summary(true_beta)$coefficients[voi_index,]
  
  if(voi %in% adj_var){
    voi_ind <- which(adj_var == paste(voi))
    adj_type <- adj_type[-voi_ind]
    adj_var <- adj_var[-voi_ind]
  }
  
  data <- data[,  c('Y', paste(voi), adj_var)]
  colnames(data)[which(colnames(data) == paste(voi))] <- "voi"
  
  n <- dim(data)[1]
  
  df <- data.frame(estimate = rep (NA, B), HR = rep(NA, B), pvalue = rep(NA, B), z = rep(NA, B))
  fail <- numeric(B)
  
  if(me_type == "all"){
    adj_type[which(adj_var == "RIDRETH1")] <- "None"
    adj_type[which(adj_var == "RIDAGEYR")] <- "None"
    adj_type[which(adj_var == "male")] <- "None"
  } else if(me_type == "voi"){
    adj_type <- rep("None", length(adj_type))
  } else if(me_type == "adj"){
    adj_type[which(adj_var == "RIDRETH1")] <- "None"
    adj_type[which(adj_var == "RIDAGEYR")] <- "None"
    adj_type[which(adj_var == "male")] <- "None"
    voi_type <- "None"
  }
  
  var_type <- c(voi_type, adj_type)
  if(!length(var_type) == (dim(data)[2] - 1)){
    stop("An error occured")
  }
  
  for(i in 1:B){
    
    if(print_progress){
      print(i)
    }
    
    data_var <- data[, 2:ncol(data)]
    
    me_num <- runif(1, me_range$correlation[1], me_range$correlation[2])
    
    ### numeric:
    num_ind <- which(var_type == "Num")
    
    #data_var[,num_ind] <- scale(data_var[,num_ind])
    
    for(k in num_ind){
      
      k_var <- var(data_var[,k], na.rm = T)
      proz_wahr <- me_num
      tot_var <- k_var/proz_wahr
      sd_to_use <-  sqrt(tot_var - k_var)
      
      data_var[,k] <- data_var[,k] + rnorm(n, 0, sd_to_use)
      
    }
    
    ### binary:
    
    me_sens <- runif(1, me_range$sensitivity[1], me_range$sensitivity[2])
    me_spec <- runif(1, me_range$specificity[1], me_range$specificity[2])
    
    bin_ind <- which(var_type == "Bin")
    
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
      
      #proz_wahr <-  me_num**2
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
    
    outcome <- data$Y
    
    formula <- paste("outcome ~ voi + ", paste(adj_var, collapse = " + "), sep="")
    
    data_me <- data.frame(outcome, data_var)
    
    tryCatch(mod <- summary(do.call("coxph", list(as.formula(formula), data = data_me,
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
      df[i,3] <- mod$coefficients[1,5] # p-value
      df[i,4] <- mod$coefficients[1,4] # z
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



###________________________________________________________###
###   recomputesamplePvalue: calculate the exact p-value   ###
###________________________________________________________###

# subfunction of sample_vib_sim

recomputesamplePvalue <- function(df) {
  zeroPval <- !is.na(df[,3]) & (df[,3] == 0)
  df[zeroPval, 3] <- pnorm(abs(df[zeroPval, 4]), lower.tail=F)*2
  return(df)
}