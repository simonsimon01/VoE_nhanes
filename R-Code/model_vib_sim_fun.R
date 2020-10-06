# Functions for the assessment of model vibration on simulated data
#
# mod_vib_sim: Main function in order to assess measurement vibration on simulated data 
#   Subfunctions provided by Patel et al. (2015)


###################################################################################################
###   model_vib_sim: Main function in order to assess measurement vibration on simulated data   ###
###################################################################################################


### Input:
#
# data_new            Data set containing simulated data with different numbers of observations
# voi                 Character, specifying the variable of interest
# adj_var_full        Character vector specifying the adjustment variables
# var_vec             Character, name of the variables that used as adjustment variables for all models
# covariates          Formula of covariates
#
### Output:
#
# list of results from conductvibration with different numbers of observations




mod_vib_sim <- function(data_new, voi, adj_var_full, var_vec, covariates){
  
  if(paste(voi) %in% adj_var_full){
    vec_index <- which(adj_var_full == paste(voi))
    covariates <- as.formula(gsub(paste("\\+ ", voi, sep = ""), "", as.character(covariates)))
  }
  if(paste(voi) %in% var_vec){
    vec_index <- which(var_vec == paste(voi))
    var_vec <- var_vec[-c(vec_index)]
  }
  
  basemodel <- as.formula(paste("Y ~ ", voi, " +", paste(var_vec, collapse = "+")))
  
  vib_500 <- vib_1000 <- vib_5000 <- list()
  
  for(i in 1:10){
  
  
    
    vib_500[[i]] <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_500[[i]], adjustby = covariates, 
                                family = 'cox', print_progress = F, variable = paste(voi)) 
    vib_1000[[i]] <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_1000[[i]], adjustby = covariates,
                                 family = 'cox', print_progress = F, variable = paste(voi))
    vib_5000[[i]] <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_5000[[i]], adjustby = covariates,
                                 family = 'cox', print_progress = F, variable = paste(voi))
    
    
  }
  

  vib_10000 <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_10000, adjustby = covariates,
                                family = 'cox', print_progress = F, variable = paste(voi))
  vib_50000 <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_50000, adjustby = covariates,
                                family = 'cox', print_progress = F, variable = paste(voi))
  vib_100000 <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_100000, adjustby = covariates,
                                 family = 'cox', print_progress = F, variable = paste(voi))
  vib_200000 <- conductVibration(base_formula = basemodel, dataFrame = data_new$gen_data_200000, adjustby = covariates,
                                 family = 'cox', print_progress = F, variable = paste(voi))

  
  output <- list(vib_500 = vib_500, vib_1000 = vib_1000, vib_5000 = vib_5000, 
                 vib_10000 = vib_10000, vib_50000 = vib_50000, vib_100000 = vib_100000,
                 vib_200000 = vib_200000)
  
  return(output)
  
}



######################################################################################
###  The following functions (slightly modified) were provided by Patel et al. (2015)
###  For more information see http://chiragjpgroup.org/voe


library(survival, quietly=T)

run_model <- function(form, data, family='gaussian', ...) {
  args <- list(form, data = data, ...)
  if(family == 'gaussian') {
    return(do.call(lm, args))
  }
  if(family == 'cox') {
    return(do.call(coxph, args))
  }
  if(family == 'binomial') {
    args <- list(form, data, family=binomial(), ...)
    return(do.call(glm, args))
  }
}

#############################################################################################

conductVibrationForK <- function(base_formula, dataFrame, adjustby, k=1, family = c('gaussian', 'binomial', 'cox'),
                                 print_progress=T, fail, ...) {
  initFrame <- function(nrows,ncols) {
    matrix(NA,nrows,ncols)
  }
  
  
  addToBase <- function(base_formula, adjustingVariables) {
    form <- base_formula
    if(length(adjustingVariables)) {
      addStr <- as.formula(sprintf('~ . + %s', paste(adjustingVariables, collapse='+')))
      form <- update.formula(base_formula, addStr)
    }
    return(form)
  }
  
  
  variablename <- attr(terms(base_formula), 'term.labels')[1]     
  varname <- all.vars(as.formula(sprintf('~%s', variablename)))  
  if(print_progress) print(varname);
  
  if(class(adjustby)=='formula') {                                
    adjustby <- attr(terms(adjustby), 'term.labels')            
  }
  n <- length(adjustby)                                            
  varComb <- combn(n, k)                                           
  retFrame <- NULL
  retFrameCounter <- 1
  bicFrame <- NULL
  fail <- rep(0, ncol(varComb))
  for(ii in 1:ncol(varComb)) {                                    
    if(print_progress) cat(sprintf('%i/%i\n',ii, ncol(varComb)));
    
    adjustingVariables <- adjustby[varComb[, ii]]                 
    strComb <- paste(sort(varComb[, ii]), collapse=',')
    form <- addToBase(base_formula, adjustingVariables)           
    if(print_progress) print(form);
    
    args <- list(form, data = dataFrame, control = list(eps = 1e-05, iter.max = 50, timefix = F))
    
    tryCatch(
      est <- do.call(coxph, args),                  
      warning = function(w) {
        print(w)
        est <<- NULL
        fail[ii] <<- 1
      }
    )
    
    if(!is.null(est)) {

      
      frm <- coef(summary(est))                                   
      bicMod <- getBIC(est) # do BIC                           

      rowIndex <- grep(varname, rownames(frm))
      nLevels <- length(rowIndex)
      
      if(length(rowIndex) & is.null(retFrame)) {                
        nrows <- ncol(varComb) * nLevels
        ncols <- ncol(frm)
        retFrame <- initFrame(nrows, ncols + 2) 
        bicFrame <- initFrame(ncol(varComb), 3) 
        colnames(retFrame) <- c(colnames(frm), 'combination_index', 'factor_level')
        colnames(bicFrame) <- c('edf', 'bic', 'combination_index')
      }
      
      bicFrame[ii, 'combination_index'] <- ii                     
      bicFrame[ii, 'edf'] <- bicMod[1]
      bicFrame[ii, 'bic'] <- bicMod[2]
      
      for(jj in 1:length(rowIndex)) {                            
        retFrame[retFrameCounter, 1:ncol(frm)] <- frm[rowIndex[jj], ]
        retFrame[retFrameCounter, ncol(frm)+1] <- ii
        retFrame[retFrameCounter, ncol(frm)+2] <- jj
        retFrameCounter <- retFrameCounter+1
      }
      
    }
    
  }
  return(list(vibration=retFrame, bic=bicFrame, k=k, combinations=varComb, family=family, base_formula=base_formula, adjust=adjustby,
              warn = fail))
}


#############################################################################################

getBIC <- function(mod) {
  return(extractAIC(mod, k=log(mod$nevent))) 
}

#############################################################################################

recomputePvalue <- function(allData, zStatColName, pValColName) {
  zeroPval <- !is.na(allData[,pValColName]) & (allData[,pValColName] == 0)
  allData[zeroPval, pValColName] <- pnorm(abs(allData[zeroPval, zStatColName]), lower.tail=F)*2
  return(allData)
}


#############################################################################################

conductVibration <- function(base_formula, dataFrame, adjustby, family = c('gaussian', 'binomial', 'cox'),
                             kMin=NULL, kMax=NULL, print_progress=T, variable, ...) {
  
  ndata <- dim(dataFrame)[1]
  
  if(is.null(kMin)) {                                                         
    kMin <- 0
  }
  if(is.null(kMax)) {                                                         
    n <- length(attr(terms(adjustby), 'term.labels'))                         
    kMax <- n
  }
  cat(sprintf('running models; k start:%i, k stop:%i\n', kMin, kMax))
  retFrame <- list()
  ii <- 1
  warn <- list()
  for(k in kMin:kMax) {                                                       
    vib <- conductVibrationForK(base_formula, dataFrame, adjustby, k, family, print_progress, ...)
    warn[[ii]] <- vib$warn
    retFrame[[ii]] <- vib
    ii <- ii + 1
  }
  ret <- gatherFrames(retFrame)
  
  
  dir_neg <- sum(ret$vibFrame$estimate < 0, na.rm = T)
  dir_pos <- sum(ret$vibFrame$estimate > 0, na.rm = T)
  nmodels <- dim(ret$vibFrame)[1]
  
  numev <- sum(dataFrame$Y[,2])
  
  qhr <- quantile(ret$vibFrame$HR, probs = c(0.01, 0.99), na.rm = T)
  RHR <- qhr[2]/qhr[1]
  
  logtenp <- -log10(ret$vibFrame$pvalue)
  qp <- quantile(logtenp, probs = c(0.01, 0.99), na.rm = T)
  RP <- qp[2] - qp[1]
  
  ret <- modifyList(ret, list(dir_neg = dir_neg, dir_pos = dir_pos, nmodels = nmodels, numev = numev, name = paste(variable),
                              RHR = RHR, RP = RP, n = ndata, warnings = warn))
  
  return(ret)
}

#############################################################################################

gatherVibration <- function(returnFrames) {                    
  nrows <- c()
  for(ii in 1:length(returnFrames)) {
    nrows <- c(nrows, nrow(returnFrames[[ii]]$vibration))
  }
  
  retFrame <- matrix(nrow=sum(nrows), ncol=ncol(returnFrames[[1]]$vibration)+1)
  colnames(retFrame) <- c(colnames(returnFrames[[1]]$vibration), 'k')
  
  startIndex <- 1
  for(ii in 1:length(returnFrames)) {
    ncols <- ncol(returnFrames[[ii]]$vibration)
    retFrame[startIndex:(startIndex+nrows[ii]-1), 1:ncols] <- returnFrames[[ii]]$vibration
    retFrame[startIndex:(startIndex+nrows[ii]-1), ncols+1] <- returnFrames[[ii]]$k
    startIndex <- startIndex+nrows[ii]
  }
  return(retFrame)
}

#############################################################################################

gatherVibrationBIC <- function(returnFrames) {
  nrows <- c()
  for(ii in 1:length(returnFrames)) {                                   
    nrows <- c(nrows, nrow(returnFrames[[ii]]$bic))                    
  }
  
  retFrame <- matrix(nrow=sum(nrows), ncol=ncol(returnFrames[[1]]$bic)+1) 
  colnames(retFrame) <- c(colnames(returnFrames[[1]]$bic), 'k')
  
  startIndex <- 1
  for(ii in 1:length(returnFrames)) {
    ncols <- ncol(returnFrames[[ii]]$bic)
    retFrame[startIndex:(startIndex+nrows[ii]-1), 1:ncols] <- returnFrames[[ii]]$bic
    retFrame[startIndex:(startIndex+nrows[ii]-1), ncols+1] <- returnFrames[[ii]]$k
    startIndex <- startIndex + nrows[ii]
  }
  return(retFrame)	
}

#############################################################################################


column_headers <- function(vibFrame, family) {
  existingColnames <- colnames(vibFrame)
  newColnames <- NULL
  if(family == 'cox') {
    isRobust <- grep('robust', existingColnames)
    if(length(isRobust) > 0) {
      return(c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'factor_level', 'k'))
    } else {
      return(c('estimate', 'HR', 'se', 'z', 'pvalue', 'combination_index', 'factor_level', 'k'))
    }
  } else if(family == 'gaussian') {
    ## to do
    existingColnames[1] <- 'estimate'
    existingColnames[length(existingColnames) - 4] <- 'pvalue'
    return(existingColnames)
  } else if(family == 'binomial') {
    ## to do
    existingColnames[1] <- 'estimate'
    existingColnames[length(existingColnames) - 4] <- 'pvalue'
    return(existingColnames)
  } 
  ### fill in the rest later for other families
  return(existingColnames)
}

#############################################################################################

harmonizeFrame <- function(vibFrame, family) {
  vibFrame <- as.data.frame(vibFrame)
  colnames(vibFrame) <- column_headers(vibFrame, family)                      
  if(family %in% c('binomial')) {                                              
    vibFrame$HR <- exp(vibFrame$estimate)
  }
  return(vibFrame)
}

#############################################################################################

gatherFrames <- function(returnFrames) {
  bic <- gatherVibrationBIC(returnFrames)                                
  vibration <- gatherVibration(returnFrames)                                 
  combinations <- list()
  for(ii in 1:length(returnFrames)) {                                          
    combinations[[ii]] <- returnFrames[[ii]]$combinations
  }
  family <- returnFrames[[1]]$family
  base_formula <- returnFrames[[1]]$base_formula
  adjust <- returnFrames[[1]]$adjust
  
  vibration <- harmonizeFrame(vibration, family)                              
  vibration <- recomputePvalue(vibration, 'z', 'pvalue')                     
  return(list(vibFrame = vibration, bicFrame = bic, combinations = combinations, adjust = adjust, 
              family = family, base_formula = base_formula))
}