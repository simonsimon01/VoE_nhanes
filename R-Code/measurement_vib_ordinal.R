# Functions to generate measurement error for ordinal variables
#
# The implementation is based on the nhanes data and the following variables are considered: 
# bmi, physical activity, SES_LEVEL, education.
# current_past_smoking is considered as a binary variable
#
#
# me_ordinal_gs: Search for parameters of the normal distribution through grid searches
# me_ordinal_new: Generate continuous variables based on the ordinal variables
#   (subfunction of ord_data_for_mv in data_gen_functions.R)
# me_ord_sim: Add measurement error to continous variables and recategorize
#   (subfunction of me_fun in measurement_vib_real_fun.R and measurement_vib_sim_fun.R)


library(NMOF) # for grid search
library(truncnorm)



#################################################################################################
###   me_ordinal_gs: Search for parameters of the normal distribution through grid searches   ###
#################################################################################################

### Input:
#
# data        Original data frame (mainTab)
# var_name    Character specifying the name of the adjustment variable
#
### Output: List of variables with the following values
#
# sol_var     A large number of parameters specifying normal distributions
# quant       Cumulative proportion of observations in each category
# cuts        Cutpoints of the underlying normal distribution specifying the categories of the ordinal variable
# var_name    Character specifying the name of the adjustment variable


me_ordinal_gs <- function(data, var_name){
  
  if(var_name == "bmi"){
    
    ###############
    ###   bmi   ###
    
    # cumulative proportion in each category:
    quant <- cumsum(table(data$bmi))/sum(table(data$bmi))
    quant <- quant[-length(quant)]
    
    # feasible cutpoint for bmi:
    cuts <- c(18.5, 25, 30, 35)
    
    testFun <- function(x){
      # 10000 normal distributions according to the grid specifications:
      ndist <- rnorm(10000, mean = x[1], sd = x[2])
      # calculate quant-quantiles of the new variable:
      sim_quant <- quantile(ndist, quant)
      # calcualte squared distance to cuts:
      sum(cuts - sim_quant)**2

    }
    
    
    levels <- list(seq(18.5, 35, 0.05), seq(1, 20, 0.05))
    sol_bmi <- gridSearch(fun = testFun, levels = levels)
    
    return(list(sol_var = sol_bmi, quant = quant, cuts = cuts, var_name = var_name))
    

  }
  
  if(var_name == "physical_activity"){
    
    #############################
    ###   physical_activity   ###
    
    
    quant <- cumsum(table(data$physical_activity))/sum(table(data$physical_activity))
    quant <- quant[-length(quant)]
    cuts <- c(0, 150, 300)
    
    testFun <- function(x){
      
      ndist <- rnorm(10000, mean = x[1], sd = x[2])
      sim_quant <- quantile(ndist, quant)
      sum(cuts - sim_quant)**2
      
    }
    
    
    levels <- list(seq(100, 200, 0.5), seq(5, 200, 0.5))
    sol_phys_act <- gridSearch(fun = testFun, levels = levels)
    
    return(list(sol_var = sol_phys_act, quant = quant, cuts = cuts, var_name = var_name))

    
  }
  
  if(var_name == "SES_LEVEL"){
    
    #####################
    ###   SES_Level   ###
    
    
    quant <- cumsum(table(data$SES_LEVEL))/sum(table(data$SES_LEVEL))
    quant <- quant[-length(quant)]
    cuts <- c(39203, 90000)
    # https://dqydj.com/united-states-household-income-brackets-percentiles/
    
    testFun <- function(x){
      
      ndist <- rnorm(1000, mean = x[1], sd = x[2])
      sim_quant <- quantile(ndist, quant)
      sum(cuts - sim_quant)**2
      
    }
    
    
    levels <- list(seq(40000, 60000, 50), seq(30000, 50000, 50))
    sol_SES_Level <- gridSearch(fun = testFun, levels = levels)
    
    return(list(sol_var = sol_SES_Level, quant = quant, cuts = cuts, var_name = var_name))
    
  }
  
  if(var_name == "education"){
    
    #####################
    ###   education   ###
    
    
    table(data$education)
    
    quant <- cumsum(table(data$education))/sum(table(data$education))
    quant <- quant[-length(quant)]
    cuts <- c(8, 12)
    # https://www.usa-info.net/usa-wiki/amerikanisches-schulsystem/
    
    testFun <- function(x){
      
      ndist <- rnorm(10000, mean = x[1], sd = x[2])
      sim_quant <- quantile(ndist, quant)
      sum(cuts - sim_quant)**2
      
    }
    
    
    levels <- list(seq(8, 12, 0.1), seq(0.1, 10, 0.1))
    sol_education <- gridSearch(fun = testFun, levels = levels)
    
    return(list(sol_var = sol_education, quant = quant, cuts = cuts, var_name = var_name))
    
  }
  
}

########################################################################################
###   me_ordinal_new: Generate continuous variables based on the ordinal variables   ###
########################################################################################

# used by ord_data_for_mv in data_gen_functions.R

### Input:
#
# data      Data set with simulated data obtained by data_for_mv
# ord_var   Object obtained by running me_ordinal_gs
# n         Sample size

### Output: List of variables with the following values
#
# new_var   New continuous variable
# quant     Cumulative proportion of observations in each category of the original variable
# n         Sample size
# var_name  Character specifying the name of the adjustment variable
# cuts      Cutpoints of the underlying normal distribution specifying the categories of the ordinal variable

me_ordinal_new <- function(data, ord_var, n){

  truncnorm_var <- list()

  for(m in 1:length(ord_var)){

    sol_var <- ord_var[[m]]

    # number of categories:
    ntab <- length(sol_var$cuts) + 1

    new_var <- rep(NA, n)
    cuts_long <- c(-Inf, sol_var$cuts, Inf)

    # for each category...
    for(k in 1:ntab){

      # observations in the corresponding category:
      kat_ind <- which(data[,sol_var$var_name] == k)
      # number of observations in the corresponding category:
      nkat <- length(kat_ind)
      # draw nkat observations from a truncated normal distribution with bounds specifiyed
      # through the corresponding cuts with mean and sd that minimized the target function
      # in the grid search (me_ordinal_gs):
      new_norm <- rtruncnorm(n = nkat, a = cuts_long[k], b = cuts_long[k+1],
                             mean = sol_var$sol_var$minlevels[1], sd = sol_var$sol_var$minlevels[2])
      new_var[kat_ind] <- new_norm
    }


    truncnorm_var[[m]] <- list(new_var = new_var, quant = sol_var$quant,
                               n = n, var_name = sol_var$var_name, cuts = sol_var$cuts)

  }

  return(truncnorm_var)

}



#####################################################################################
###   me_ord_sim: Add measurement error to continous variables and recategorize   ###
#####################################################################################

# subfunction of me_fun in measurement_vib_real_fun.R and measurement_vib_sim_fun.R

### Input:
#
# var_name    Character specifying the name of the adjustment variable
# n           Sample size
# new_var     Continuous variable based on the ordinal variable
# var_sd      Measurement error standard deviation
# quant       Cumulative proportion of observations in each category of the original variable
#
### Output:
#
# var_me      New variable with measurement error


me_ord_sim <- function(var_name, n, new_var, var_sd, quant, cuts){
  
  if(var_name == "bmi"){
    
    # add measurement error to the new continous variable
    new_var_me <- new_var + rnorm(n, 0, var_sd)
    
    # categorization:
    
    new_var_me_ord <- rep(NA, n)
    
    new_var_me_ord[which(new_var_me < cuts[1])] <- 1
    new_var_me_ord[which(new_var_me >= cuts[1] & new_var_me < cuts[2])] <- 2
    new_var_me_ord[which(new_var_me >= cuts[2] & new_var_me < cuts[3])] <- 3
    new_var_me_ord[which(new_var_me >= cuts[3] & new_var_me < cuts[4])] <- 4
    new_var_me_ord[which(new_var_me >= cuts[4])] <- 5
    
    var_me <- new_var_me_ord
    
  }
  
  if(var_name == "physical_activity"){
    
    new_var_me <- new_var + rnorm(n, 0, var_sd)
    
    # categorization:
    
    new_var_me_ord <- rep(NA, n)
    
    new_var_me_ord[which(new_var_me <= cuts[1])] <- 0
    new_var_me_ord[which(new_var_me > cuts[1] 
                         & new_var_me <= cuts[2])] <- 1
    new_var_me_ord[which(new_var_me > cuts[2] 
                         & new_var_me <= cuts[3])] <- 2
    new_var_me_ord[which(new_var_me > cuts[3])] <- 3
    
    
    var_me <- new_var_me_ord
    
  }
  
  if(var_name == "SES_LEVEL"){
    
    new_var_me <- new_var + rnorm(n, 0, var_sd)
    
    # categorization:
    
    new_var_me_ord <- rep(NA, n)
    
    new_var_me_ord[which(new_var_me < cuts[1])] <- 1
    new_var_me_ord[which(new_var_me >= cuts[1] & new_var_me < cuts[2])] <- 2
    new_var_me_ord[which(new_var_me >= cuts[2])] <- 3
    
    var_me <- new_var_me_ord
    
  }
  
  if(var_name == "education"){
    
    new_var_me <- new_var + rnorm(n, 0, var_sd)
    
    # categorization:
    f
    new_var_me_ord <- rep(NA, n)
    
    new_var_me_ord[which(new_var_me < cuts[1])] <- 1
    new_var_me_ord[which(new_var_me >= cuts[1] & new_var_me < cuts[2])] <- 2
    new_var_me_ord[which(new_var_me >= cuts[2])] <- 3
    
    var_me <- new_var_me_ord
    
  }
  
  return(var_me)
  
}