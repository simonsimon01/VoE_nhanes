# Functions for the assessment of sampling vibration on real data

# sample_vib_real: Main function in order to assess sampling vibration on real data
#   weight_function: subfunction of sample_vib_real; calculating weights for cox model
#   addToBase: subfunction of sample_vib_real; updating cox model
#   recomputesamplePvalue: subfunction of sample_vib_real; calculating the exact p-value



#############################################################################################
###  sample_vib_real:  Main function in order to assess sampling vibration on real data   ###
#############################################################################################

### Input:

# mainTab                   Data frame containing the NHANES data
# voi                       Character specifying the variable of interest
# B                         Number of iterations, default is 1000
# replace                   Logical, whether sampling should be performed with replacement (bootstrap). Default is FALSE
# split                     Fraction of observations to be drawn in a subsample, default is 0.5
# adj_var                   Character, specifying the amount of adjustment
# exactPvalue               Logical, whether the pvalue should be recalculated. Default is TRUE
# log                       Logical, whether the variable of interest should be log-transformed. Default is FALSE
# continuous                Logical, whether the variable of interest is continuous. Default is TRUE
#
#
### Output: list with the following elements
#
# results                   Data frame with results for all iterations
# RHR                       Relative hazard ratio
# RP                        Relative p-value
# name                      Name of the variable of interest
# split                     Fraction observations to be drawn in a subsample
# n                         Numbers of observations in the data
# num_adj                   Amount of adjustment
# numev                     Number of events in the data
# dir_neg                   Number of iterations with results in negative direction
# dir_pos                   Number of iterations with results in positive direction
# warnings                  Vector indicating runs that caused a warning


sample_vib_real <- function(mainTab, voi, B = 1000, replace = FALSE, split = 0.5,
                               adj_var = c("full", "sub", "none"), exactPvalue = TRUE,
                               log = F, continuous = T){
  
  num_adj <- adj_var
  
  mainTab <- weight_function(mainTab, paste(voi))
  
  
  voi_ind <- which(colnames(mainTab) == paste(voi))
  colnames(mainTab)[voi_ind] <- "voi"
  
  
  if(continuous){
    if(log){
      basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(voi + 0.1))) + 
        cluster(area)
    } else {
      basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(voi)) + 
        cluster(area)
    }
  } else {
    basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ voi + cluster(area)
  }
  
  
  if(adj_var == "full"){
    
    adj_var <- ~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + as.factor(bmi) + 
      any_cad + any_family_cad + any_diabetes + any_cancer_self_report +  as.factor(current_past_smoking)  + 
      drink_five_per_day + as.factor(physical_activity)  + LBXTC + any_ht + RIDAGEYR + male
    
    var_vec <- c('voi', 'weight', 'area', 'MORTSTAT', 'PERMTH_EXM',
                 'SES_LEVEL', 'RIDAGEYR', 'male', 'current_past_smoking', 'any_cad', 'any_family_cad', 
                 'any_cancer_self_report', 'bmi', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1', 
                 'physical_activity', 'drink_five_per_day', 'LBXTC' )
    
    if(voi %in% var_vec){
      vec_index <- which(var_vec == paste(voi))
      var_vec <- var_vec[-c(vec_index)]
      
      adj_var <- as.formula(gsub(paste(voi), "voi", as.character(adj_var)))
    }
    
    data <- mainTab[, var_vec]
    
    adjustby <- attr(terms(adj_var), 'term.labels')
    form <- addToBase(base_formula = basemodel, adjustingVariables = adjustby) 
    
  } else if (adj_var == "sub"){
    
    adj_var <- ~ RIDAGEYR + male
    
    var_vec <- c('voi', 'weight', 'area', 'MORTSTAT', 'PERMTH_EXM',
                 'RIDAGEYR', 'male' )
    
    if(voi %in% var_vec){
      vec_index <- which(var_vec == paste(voi))
      var_vec <- var_vec[-c(vec_index)]
      
      adj_var <- as.formula(gsub(paste(voi), "voi", as.character(adj_var)))
    }
    
    data <- mainTab[, var_vec]
    
    adjustby <- attr(terms(adj_var), 'term.labels')
    form <- addToBase(base_formula = basemodel, adjustingVariables = adjustby) 
    
  } else if (adj_var == "none"){
    
    data <- mainTab[, c('voi', 'weight', 'area', 'MORTSTAT', 'PERMTH_EXM')]
    
    form <- basemodel
  }
  
  data <- na.exclude(data)
  n <- dim(data)[1]
  
  df <- data.frame(estimate = rep (NA, B), HR = rep(NA, B), pvalue = rep(NA, B), z = rep(NA, B))
  fail <- numeric(B)
  
  for(i in 1:B){
    if(replace == FALSE){
      subs <- sample(n, size = n*split)
    } else {
      subs <- sample(n, size = n, replace = TRUE)
    }
    
    tryCatch(mod <- summary(do.call("coxph", list(as.formula(form), weights = data[subs,'weight'], data = data[subs,]))),
             
             warning = function(w) {
               print(w)
               fail[i] <<- 1
             })
    
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
  
  
  dir_neg <- sum(df$estimate < 0)
  dir_pos <- sum(df$estimate > 0)
  
  qhr <- quantile(df[,2], probs = c(0.01, 0.99))
  RHR <- qhr[2]/qhr[1]
  
  if(exactPvalue){
    df <- recomputesamplePvalue(df)
  } else {
    p0 <- which(df$pvalue == 0)
    df$pvalue[p0] <- 1e-16
  }
  
  logtenp <- -log10(df[,3])
  
  qp <- quantile(logtenp, probs = c(0.01, 0.99))
  RP <- qp[2] - qp[1]
  
  if(!replace){
    split <- as.character(fractions(split))
  } else {
    split = NULL
  }
  
  erg <- list(results = df, RHR = RHR, RP = RP, name = voi,
              split = split, num_adj = num_adj, numev = sum(data$MORTSTAT),
              dir_neg = dir_neg, dir_pos = dir_pos, n = n, warnings = fail)
  
  return(erg)
  
}



###______________________________________________________###
###   weight_function: calculate weights for cox model   ###
###______________________________________________________###


# subfunction of sample_vib_real

### Input:
#
# mainTab     Original data frame with nhanes data
# var         Character specifying the variable of interest

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


###_____________________________________________###
###   addToBase: Update formula for cox model   ###
###_____________________________________________###

# subfunction of sample_vib_real

addToBase <- function(base_formula, adjustingVariables) {
  form <- base_formula
  addStr <- as.formula(sprintf('~ . + %s', paste(adjustingVariables, collapse='+')))
  form <- update.formula(base_formula, addStr)
  return(form)
}


###__________________________________________________________###
###   recomputesamplePvalue: calculating the exact p-value   ###
###__________________________________________________________###

# subfunction of sample_vib_real

recomputesamplePvalue <- function(df){
  zeroPval <- !is.na(df[,3]) & (df[,3] == 0)
  df[zeroPval, 3] <- pnorm(abs(df[zeroPval, 4]), lower.tail=F)*2
  return(df)
}