# Functions for the assessment of sampling vibration on simulated data

# vib_sim_summary: Main function for the assessment of sampling vibration on simulated data
#   sample_vib_sim: subfunction of vib_sim_summary; calculate sample vibration for one data set
#     recomputesamplePvalue: subfunction of sample_vib_sim; calculate exact p-values


#####################################################################################################
###   vib_sim_summary: Main function for the assessment of sampling vibration on simulated data   ###
#####################################################################################################


### Input:
#
# data_new          Data set containing simulated data with different numbers of observations
# voi               Character, specifying the variable of interest
# B                 Number of iterations, default is 1000
# replace           Logical, whether sampling should be performed with replacement (bootstrap). Default is FALSE
# split             Fraction of observations to be drawn in a subsample, default is 0.5
# exactPvalue       Logical, whether the pvalue should be recalculated. Default is TRUE
# family            Character specifying the type of the model, either 'cox' or 'binomial'
# outcome           Character specifying the outcome
# adj_var           Character vector specifying the adjustment variables
#
### Output:
#
# list of results from sample_vib_sim with different numbers of observations


vib_sim_summary <- function(data_new, voi, B = 1000, replace = F, split = 0.5, exactPvalue = T, family = "cox", 
                            outcome = "Y", adj_var){
  
  
  
  vib_500_15 <- vib_1000_15 <- vib_5000_15 <- list()
  
  
  for(i in 1:10){
  
  ###   n = 500 
    
  vib_500_15[[i]] <- sample_vib_sim(data = data_new$gen_data_500[[i]], voi = paste(voi), replace = replace, 
                               adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue, 
                               family = family, outcome = outcome)
  

  ###   n = 1000
  
  vib_1000_15[[i]] <- sample_vib_sim(data = data_new$gen_data_1000[[i]], voi = paste(voi), replace = replace,
                                adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue,
                                family = family, outcome = outcome)


  ###   n = 5000

  vib_5000_15[[i]] <- sample_vib_sim(data = data_new$gen_data_5000[[i]], voi = paste(voi), replace = replace,
                                adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue,
                                family = family, outcome = outcome)

  }

  ###   n = 10000
  
  vib_10000_15 <- sample_vib_sim(data = data_new$gen_data_10000, voi = paste(voi), replace = replace,
                                 adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue,
                                 family = family, outcome = outcome)


  ###   n = 50000

  vib_50000_15 <- sample_vib_sim(data = data_new$gen_data_50000, voi = paste(voi), replace = replace,
                                 adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue,
                                 family = family, outcome = outcome)

  ###   n = 100000

  vib_100000_15 <- sample_vib_sim(data = data_new$gen_data_100000, voi = paste(voi), replace = replace,
                                  adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue,
                                  family = family, outcome = outcome)


  ###   n = 200000
  
  vib_200000_15 <- sample_vib_sim(data = data_new$gen_data_200000, voi = paste(voi), replace = replace,
                                  adj_var = adj_var, B = B, split = split, exactPvalue = exactPvalue,
                                  family = family, outcome = outcome)



  output <- list(vib_500_15 = vib_500_15, vib_1000_15 = vib_1000_15, vib_5000_15 = vib_5000_15, 
                 vib_10000_15 = vib_10000_15, vib_50000_15 = vib_50000_15, vib_100000_15 = vib_100000_15, 
                 vib_200000_15 = vib_200000_15)
  
 
  return(output)
   
}



###___________________________________________________________________###
###   sample_vib_sim: calculate sampling vibration for one data set   ###
###___________________________________________________________________###

# subfunction of vib_sim_summary

### input:
#
# data              Data frame containing simulated data
# voi               Character, specifying the variable of interest
# B                 Number of iterations
# replace           Logical, whether sampling should be performed with replacement (bootstrap). Default is FALSE
# split             Fraction of observations to be drawn in a subsample
# adj_var           Character vector specifying the adjustment variables
# exactPvalue       Logical, whether the pvalue should be recalculated. Default is TRUE
# family            Character specifying the type of the model, either 'cox' or 'binomial'
# outcome           Character specifying the outcome
#
### Output: list with the following elements:
#
# results       Data frame with results for all iterations
# RHR           Relative hazard ratio, only for family = 'cox'
# ROR           Relative odds ratio, only for family = 'binomial'
# RP            Relative p-value
# name          Name of the variable of interest
# n             Numbers of observations in the data
# family        Type of model, either 'cox' or 'binomial'
# split         Fraction observations to be drawn in a subsample
# num_adj       Amount of adjustment
# numev         Number of events in the data
# dir_neg       Number of iterations with results in negative direction
# dir_pos       Number of iterations with results in positive direction
# warnings      Vector indicating runs that caused a warning


sample_vib_sim <- function(data, voi, B, replace, split, adj_var, exactPvalue, 
                           family = c("cox", "binomial"), outcome){

  n <- dim(data)[1]
  num_adj <- dim(data)[2] - 2

  df <- data.frame(estimate = rep (NA, B), HR = rep(NA, B), pvalue = rep(NA, B), z = rep(NA, B))
  fail <- numeric(B)

  out_ind <- which(colnames(data) == paste(outcome))
  colnames(data)[out_ind] <- "outcome"


  if(is.null(adj_var)){
    formula <- paste("outcome ~ ", paste(voi), sep="")
  } else {
    formula <- paste("outcome ~ ", paste(voi, " + ", sep = ""),
                     paste(adj_var, collapse = " + "), sep="")
  }


  for(i in 1:B){
    
    print(i)
    subs <- sample(n, size = n*split, replace = replace)

    if(family == "cox"){

      tryCatch(mod <- summary(do.call("coxph", list(as.formula(formula), data = data[subs,],
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
      
      if(!is.null(mod)){
        df[i,1] <- mod$coefficients[1,1] # estimate
        df[i,2] <- mod$coefficients[1,2] # HR
        df[i,3] <- mod$coefficients[1,5] # p-value
        df[i,4] <- mod$coefficients[1,4] # z
      }

    } else if (family == "binomial"){

      tryCatch(mod <- summary(do.call("glm", list(as.formula(formula), data = data[subs,], family = "binomial"))),
               
               warning = function(w) {
                 print(w)
                 fail[i] <<- 1
               })
      
      if(!is.null(mod)){
        df[i,1] <- mod$coefficients[2,1] # estimate
        df[i,2] <- exp(mod$coefficients[2,1]) # OR
        df[i,3] <- mod$coefficients[2,4] # p-value
        df[i,4] <- mod$coefficients[2,3] # z
      }

    }
  }
  
  dir_neg <- sum(df$estimate < 0, na.rm = T)
  dir_pos <- sum(df$estimate > 0, na.rm = T)

  qhr <- quantile(df[,2], probs = c(0.01, 0.99), na.rm = T)
  RHR <- qhr[2]/qhr[1]

  if(exactPvalue){
    df <- recomputesamplePvalue(df)
  } else {
    p0 <- which(df$pvalue == 0)
    df$pvalue[p0] <- 1e-16
  }

  logtenp <- -log10(df[,3])

  qp <- quantile(logtenp, probs = c(0.01, 0.99), na.rm = T)
  RP <- qp[2] - qp[1]

  if(!replace){
    split <- as.character(fractions(split))
  } else {
    split = NULL
  }

  if(family == "cox"){
    erg <- list(results = df, RHR = RHR, RP = RP, name = voi,
                split = split, family = family, n = n, num_adj = num_adj, numev = sum(data$outcome[,2]),
                dir_neg = dir_neg, dir_pos = dir_pos, warnings = fail)
  } else if(family == "binomial"){
    colnames(df)[2] <- "OR"
    erg <- list(results = df, ROR = RHR, RP = RP, name = voi,
                split = split, family = family, dir_neg = dir_neg, dir_pos = dir_pos, warnings = fail)
  }

  return(erg)

}


###____________________________________________________###
###   recomputesamplePvalue: calculate exact p-values  ###
###____________________________________________________###

# subfunction of sample_vib_sim

recomputesamplePvalue <- function(df) {
  zeroPval <- !is.na(df[,3]) & (df[,3] == 0)
  df[zeroPval, 3] <- pnorm(abs(df[zeroPval, 4]), lower.tail=F)*2
  return(df)
}