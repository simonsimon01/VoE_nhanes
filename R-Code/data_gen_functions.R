# Functions generating data of different sample sizes
#
# data_for_mv: generate data of different sample sizes based on nhanes data
#   sample_vib_gen2: subfunction of data_for_mv; data generation based on nhanes data
#     varsim2: subfunction of sample_vib_gen2; covariate data generation based on the nhanes data
# ord_data_for_mv: generate ordinal data of different sample sizes for measurement vibration




####################################################################################
###   data_for_mv: generate data of different sample sizes based on nhanes data  ###
####################################################################################

### Input:
#
# mainTab     Data frame containing original nhanes data
# voi         Character, name of the variable of interest in the nhanes data set
# voi_type    Character specifying the type of the variable of interest
# adj_type    Character vector specifying the type of adjustment variables
# adj_var     Character vector containing names of adjustment variables
#
### Output:
#
# Simulated data with different numbers of observations

data_for_mv <- function(mainTab, voi, voi_type, adj_type, adj_var){
  
  fac_ind <- rep(T, length(adj_type))
  fac_ind[which(adj_type == "Num")] <- F
  fac_ind[which(adj_var == "RIDRETH1")] <- T
  
  
  if(voi_type == "Num"){
    inv_cat = F
  } else {inv_cat = T}
  
  gen_data_500 <- gen_data_1000 <- gen_data_5000 <- list()
  
  for(i in 1:10){
    
    
    gen_data_500[[i]] <- sample_vib_gen2(mainTab, n = 500, voi = paste(voi), voi_cat = inv_cat, 
                                         adj_var = adj_var, fac_ind = fac_ind)
    
    gen_data_1000[[i]] <- sample_vib_gen2(mainTab, n = 1000, voi = paste(voi), voi_cat = inv_cat, 
                                          adj_var = adj_var, fac_ind = fac_ind)
    
    gen_data_5000[[i]] <- sample_vib_gen2(mainTab, n = 5000, voi = paste(voi), voi_cat = inv_cat, 
                                          adj_var = adj_var, fac_ind = fac_ind)
    
  }
  
  
  gen_data_10000 <- sample_vib_gen2(mainTab, n = 10000, voi = paste(voi), voi_cat = inv_cat, 
                                    adj_var = adj_var, fac_ind = fac_ind)
  
  gen_data_50000 <- sample_vib_gen2(mainTab, n = 50000, voi = paste(voi), voi_cat = inv_cat, 
                                    adj_var = adj_var, fac_ind = fac_ind)
  
  gen_data_100000 <- sample_vib_gen2(mainTab, n = 100000, voi = paste(voi), voi_cat = inv_cat, 
                                     adj_var = adj_var, fac_ind = fac_ind)
  
  gen_data_200000 <- sample_vib_gen2(mainTab, n = 200000, voi = paste(voi), voi_cat = inv_cat, 
                                     adj_var = adj_var, fac_ind = fac_ind)
  
  
  
  return(list(gen_data_500 = gen_data_500, gen_data_1000 = gen_data_1000, 
              gen_data_5000 = gen_data_5000, gen_data_10000 = gen_data_10000, 
              gen_data_50000 = gen_data_50000, gen_data_100000 = gen_data_100000, 
              gen_data_200000 = gen_data_200000))
  
  
}



###___________________________________________________________###
###   sample_vib_gen2: data generation based on nhanes data   ###
###___________________________________________________________###

### Input:
#
# mainTab     Data frame containing original nhanes data
# n           Number of observations that should be generated
# voi         Character, name of the variable of interest in the nhanes data set
# voi_cat     Logical, whether voi is binary
# adj_var     Character vector containing names of adjustment variables
# fac_ind     Logical vector, whether variables named in adj_var are binary
#
# Output: 
# 
# data        Data frame with simulated data



sample_vib_gen2 <- function(mainTab, n, voi, adj_var = NULL, voi_cat = F, fac_ind){
  
  Y_old <- Surv(time = mainTab$PERMTH_EXM, event = mainTab$MORTSTAT)
  
  if(voi %in% adj_var){
    main_sub <- mainTab[,c(adj_var)]
    data_old <- data.frame(Y_old, main_sub)
    data_old <- na.exclude(data_old)
    main_sub <- data_old[,-1]
    main_sub_old <- main_sub
    if(sum(fac_ind) > 1){
      main_sub[,fac_ind] <- data.frame(lapply(main_sub[,fac_ind], as.factor))
    } else if (sum(fac_ind) == 1){
      main_sub[,fac_ind] <- unlist(lapply(main_sub[,fac_ind], as.factor))
    }
    
  } else {
    if(voi_cat){
      if(!(voi %in% c("physical_activity", "male", "any_cancer_self_report", "any_ht", "any_diabetes",
                      "drink_five_per_day", "any_cad", "any_family_cad"))){
        mainTab[,voi] <- mainTab[,voi] + 1
      }
    }
    main_sub <- mainTab[,c(adj_var, voi)]
    data_old <- data.frame(Y_old, main_sub)
    data_old <- na.exclude(data_old)
    main_sub <- data_old[,-1]
    main_sub_old <- main_sub
    if(sum(c(fac_ind, voi_cat)) == 1){
      if(length(c(fac_ind, voi_cat)) > 1){
        main_sub[,c(fac_ind, voi_cat)] <- unlist(lapply(main_sub[,c(fac_ind, voi_cat)], as.factor))
      } else {
        main_sub <- data.frame(as.factor(main_sub))
        colnames(main_sub) <- voi
      }
    } else if (sum(c(fac_ind, voi_cat)) > 1){
      main_sub[,c(fac_ind, voi_cat)] <- data.frame(lapply(main_sub[,c(fac_ind, voi_cat)], as.factor))
    } else {
      main_sub <- data.frame(main_sub)
      colnames(main_sub) <- voi
    }
  }
  
  
  if(!voi_cat){
    if(length(adj_var) == 0){
      main_sub <- scale(main_sub)
    } else{
      main_sub[, paste(voi)] <- as.numeric(scale(main_sub[, paste(voi)]))
    }
  }
  
  
  X_sim <- varsim2(n = n, main_sub = main_sub, main_sub_old = main_sub_old,
                   voi = voi, adj_var = adj_var, voi_cat = voi_cat,
                   fac_ind = fac_ind)
  
  
  data_old <- data.frame(Y_old = data_old[,1], main_sub)
  
  coxmod <- coxph(Y_old ~ ., data = data_old)
  beta <- c(coxmod$coefficients)
  
  lambda <- 1/mean(data_old$Y_old[,1][which(data_old$Y_old[,2] == 1)], na.rm=T)
  lambda_c <- 1/mean(data_old$Y_old[,1][which(data_old$Y_old[,2] == 0)], na.rm=T)
  
  simbet <- predict(coxmod, newdata = X_sim)
  nt <- lambda * exp(drop(simbet))
  U <- runif(n)
  Uc <- runif(n)
  
  real.time <- -(log(U))/nt
  
  cens.time <- -(log(Uc))/(lambda_c)
  status <- ifelse(real.time <= cens.time, 1, 0)
  sumev <- sum(status)
  
  if(sumev/n < 0.2 | sumev/n > 0.3){
    
    j <- 2
    
    while(sumev/n < 0.2 | sumev/n > 0.3){
      
      cens.time <- -(log(Uc))/(lambda_c*1*j)
      status <- ifelse(real.time <= cens.time, 1, 0)
      sumev <- sum(status)
      j <- j + 1
      
    }
    
  }
  
  
  obs.time <- ifelse(real.time <= cens.time, real.time, cens.time)
  Y <- Surv(obs.time, status)
  data <- data.frame(Y, X_sim)
  
  return(data)
  
}



###_________________________________________________________________###
###   varsim2: covariate data generation based on the nhanes data   ###
###_________________________________________________________________###


# subfunction of sample_vib_gen2

### Input:
#
# n             Number of observations
# main_sub      Original data, only relevant columns, rows with NA excluded
# main_sub_old  Original data, only relevant columns, rows with NA not excluded
# adj_var       Character vector specifying the adjustment variables
# voi           Character, specifying the variable of interest
# voi_cat       Logical, whether voi is binary, default is FALSE
# fac_ind       Logical vector, whether variables named in adj_var are binary
#
#
### Output:
#
# data frame containing simulated covariates


varsim2 <- function(n, main_sub, main_sub_old, adj_var = NULL, voi, voi_cat = F, fac_ind){
  
  if(length(adj_var) == 15){
    
    m1 <- cumsum(table(main_sub$physical_activity)/dim(main_sub)[1])
    m2 <- cumsum(table(main_sub$RIDRETH1)/dim(main_sub)[1])
    m4 <- cumsum(table(main_sub$male)/dim(main_sub)[1])
    m5 <- cumsum(table(main_sub$any_cancer_self_report)/dim(main_sub)[1])
    m6 <- cumsum(table(main_sub$current_past_smoking)/dim(main_sub)[1])
    m7 <- cumsum(table(main_sub$bmi)/dim(main_sub)[1])
    m8 <- cumsum(table(main_sub$any_ht)/dim(main_sub)[1])
    m9 <- cumsum(table(main_sub$any_diabetes)/dim(main_sub)[1])
    m11 <- cumsum(table(main_sub$drink_five_per_day)/dim(main_sub)[1])
    m12 <- cumsum(table(main_sub$education)/dim(main_sub)[1])
    m13 <- cumsum(table(main_sub$SES_LEVEL)/dim(main_sub)[1])
    m14 <- cumsum(table(main_sub$any_family_cad)/dim(main_sub)[1])
    m15 <- cumsum(table(main_sub$any_cad)/dim(main_sub)[1])
    
    m1 <- m1[-length(m1)]
    m2 <- m2[-length(m2)]
    m4 <- m4[-length(m4)]
    m5 <- m5[-length(m5)]
    m6 <- m6[-length(m6)]
    m7 <- m7[-length(m7)]
    m8 <- m8[-length(m8)]
    m9 <- m9[-length(m9)]
    m11 <- m11[-length(m11)]
    m12 <- m12[-length(m12)]
    m13 <- m13[-length(m13)]
    m14 <- m14[-length(m14)]
    m15 <- m15[-length(m15)]
    
    
    if(voi_cat & !(voi %in% adj_var)){
      m16 <- cumsum(table(main_sub[,paste(voi)])/dim(main_sub)[1])
      m16 <- m16[-length(m16)]
      
      corr <- cor(data.frame(main_sub_old$physical_activity, main_sub_old$RIDRETH1, main_sub_old$male,
                             main_sub_old$any_cancer_self_report, main_sub_old$current_past_smoking,
                             main_sub_old$bmi, main_sub_old$any_ht, main_sub_old$any_diabetes,
                             main_sub_old$drink_five_per_day, main_sub_old$education,
                             main_sub_old$SES_LEVEL, main_sub_old$any_family_cad, main_sub_old$any_cad, main_sub_old[,paste(voi)]))
      
      adj_full_ord <- ordsample(n = n, marginal = list(m1, m2, m4, m5, m6, m7, m8, m9, m11, m12, m13, m14, m15, m16),
                                Sigma = corr)
      
      cont_ind <- which(colnames(main_sub) =="RIDAGEYR" | colnames(main_sub) == "LBXTC")
    }
    
    
    if(voi_cat == F | (voi %in% adj_var)){
      
      corr <- cor(data.frame(main_sub_old$physical_activity, main_sub_old$RIDRETH1, main_sub_old$male,
                             main_sub_old$any_cancer_self_report, main_sub_old$current_past_smoking,
                             main_sub_old$bmi, main_sub_old$any_ht, main_sub_old$any_diabetes,
                             main_sub_old$drink_five_per_day, main_sub_old$education,
                             main_sub_old$SES_LEVEL, main_sub_old$any_family_cad, main_sub_old$any_cad))
      
      adj_full_ord <- ordsample(n = n, marginal = list(m1, m2, m4, m5, m6, m7, m8, m9, m11, m12, m13, m14, m15),
                                Sigma = corr)
      
      if(voi %in% adj_var){
        cont_ind <- which(colnames(main_sub) =="RIDAGEYR" | colnames(main_sub) == "LBXTC")
      } else{
        cont_ind <- which(colnames(main_sub) =="RIDAGEYR" | colnames(main_sub) == "LBXTC" | colnames(main_sub) == paste(voi))
      }
      
    }
    
    
    newdat <- data.frame(adj_full_ord)
    newdat <- data.frame(lapply(newdat, as.factor))
    colnames(newdat) <- colnames(main_sub)[-c(cont_ind)]
    
    
    for(i in 1:length(cont_ind)){
      
      name <- colnames(main_sub)[cont_ind[i]]
      
      if(i < length(cont_ind)){
        linmod <- eval(parse(text = paste("lm(", name, "~ ., data = main_sub[,-c(cont_ind[-c(1:i)])])", sep = "")))
      } else {
        linmod <- eval(parse(text = paste("lm(", name, "~ ., data = main_sub)", sep = "")))
      }
      
      linsum <- summary(linmod)
      
      linpred <- predict(linmod, newdata = newdat)
      newvar<- linpred + rnorm(n, 0, linsum$sigma)
      
      newdat <- data.frame(newdat, newvar)
      colnames(newdat)[ncol(newdat)] <- name
      
    }
  }
  
  
  if(length(adj_var) == 2){
    
    m1 <- cumsum(table(main_sub$male)/dim(main_sub)[1])
    m1 <- m1[-length(m1)]
    
    if(voi_cat & !(voi %in% adj_var)){
      m2 <- cumsum(table(main_sub[,paste(voi)])/dim(main_sub)[1])
      m2 <- m2[-length(m2)]
      
      corr <- cor(data.frame(main_sub_old$male, main_sub_old[,paste(voi)]))
      
      adj_full_ord <- ordsample(n = n, marginal = list(m1, m2),
                                Sigma = corr)#, Spearman = T, cormat = "continuous")
      
      cont_ind <- which(colnames(main_sub) =="RIDAGEYR")
    }
    
    
    if(voi_cat == F | (voi %in% adj_var)){
      
      adj_full_ord <- ordsample(n = n, marginal = list(m1), Sigma = matrix(1), cormat = "continuous")
      
      
      if(voi %in% adj_var){
        cont_ind <- which(colnames(main_sub) =="RIDAGEYR")
      } else {
        cont_ind <- which(colnames(main_sub) =="RIDAGEYR" |  colnames(main_sub) == paste(voi))
      }
      
    }
    
    newdat <- data.frame(adj_full_ord)
    newdat <- data.frame(lapply(newdat, as.factor))
    colnames(newdat) <- colnames(main_sub)[-c(cont_ind)]
    
    for(i in 1:length(cont_ind)){
      
      name <- colnames(main_sub)[cont_ind[i]]
      
      if(i < length(cont_ind)){
        linmod <- eval(parse(text = paste("lm(", name, "~ ., data = main_sub[,-c(cont_ind[-c(1:i)])])", sep = "")))
      } else {
        linmod <- eval(parse(text = paste("lm(", name, "~ ., data = main_sub)", sep = "")))
      }
      linsum <- summary(linmod)
      
      linpred <- predict(linmod, newdata = newdat)
      newvar<- linpred + rnorm(n, 0, linsum$sigma)
      
      newdat <- data.frame(newdat, newvar)
      colnames(newdat)[ncol(newdat)] <- name
      
    }
    
  }
  
  
  if(length(adj_var) == 0){
    
    if(voi_cat){
      m1 <- cumsum(table(main_sub)/nrow(main_sub))
      m1 <- m1[-length(m1)]
      
      newdat <- ordsample(n = n, marginal = list(m1), Sigma = matrix(1), cormat = "continuous")
      newdat <- data.frame(newdat)
      newdat <- data.frame(lapply(newdat, as.factor))
    }
    
    if(voi_cat == F){
      
      var <- rnorm(n, mean(main_sub), sd(main_sub))
      newdat <- data.frame(var)
      
    }
    
    colnames(newdat) <- paste(voi)
  }
  
  
  if(length(adj_var) > 1){
    reorderind <- as.numeric(factor(colnames(main_sub), levels=colnames(newdat)))
    newdat <- newdat[,reorderind]
  }
  
  
  return(data = newdat)
  
}



######################################################################################################
###   ord_data_for_mv: generate ordinal data of different sample sizes for measurement vibration   ###
######################################################################################################

### Input:
#
# data        List of data frames obtained by running data_for_mv
# ord_var     List with ordinal variables obtained by running me_ordinal_gs (from measurement_vib_ordinal.R)
#
### Output:
#
# ordinal variables with different numbers of observations

ord_data_for_mv <- function(data, ord_var){
  
  

  ord_var_500 <- ord_var_1000 <- ord_var_5000 <- list()
  
  for(i in 1:10){
    
    
      ord_var_500[[i]] <- me_ordinal_new(data = data$gen_data_500[[i]], 
                                         ord_var = ord_var, n = 500)
      
      ord_var_1000[[i]] <- me_ordinal_new(data = data$gen_data_1000[[i]], 
                                          ord_var = ord_var, n = 1000)
      
      ord_var_5000[[i]] <- me_ordinal_new(data = data$gen_data_5000[[i]], 
                                          ord_var = ord_var, n = 5000)
    }
  
  

  
  ord_var_10000 <- me_ordinal_new(data = data$gen_data_10000, 
                                  ord_var = ord_var, n = 10000)
  
  ord_var_50000 <- me_ordinal_new(data = data$gen_data_50000, 
                                  ord_var = ord_var, n = 50000)
  
  ord_var_100000 <- me_ordinal_new(data = data$gen_data_100000, 
                                   ord_var = ord_var, n = 10000)
  
  ord_var_200000 <- me_ordinal_new(data = data$gen_data_200000, 
                                   ord_var = ord_var, n = 200000)
  
  
  return(list(ord_var_500 = ord_var_500, ord_var_1000 = ord_var_1000, ord_var_5000 = ord_var_5000,
              ord_var_10000 = ord_var_10000, ord_var_50000 = ord_var_50000, 
              ord_var_100000 = ord_var_100000, ord_var_200000 = ord_var_200000))
  
  
}
