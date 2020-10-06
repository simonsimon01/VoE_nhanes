# Functions for the assessment of model vibration on real data

# model_vib_real: Main function in order to assess measurement vibration on real data
#   Subfunctions provided by Patel et al. (2015)

###############################################################################################
###   model_vib_real: Main function in order to assess measurement vibration on real data   ###
###############################################################################################

### Input:
#
# mainTab             Data frame containing the NHANES data
# voi                 Character, specifying the variable of interest
# log                 Logical, whether the variable of interest should be log-transformed. Default is FALSE
# continuous          Logical, whether the variable of interest is continous. Default is TRUE
#
### Output: Object obtained by conductVibration


model_vib_real <- function(mainTab, voi, log = F, continuous = T){
  
  covariates <- ~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) + 
    as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report + 
    as.factor(current_past_smoking) + drink_five_per_day + as.factor(physical_activity) + 
    LBXTC + any_ht
  
  mainTab <- weight_function(mainTab, paste(voi))
  
  dat <- mainTab[, c('MORTSTAT', 'SDDSRVYR', 'weight', 'PERMTH_EXM', 'SES_LEVEL', 'RIDAGEYR', 
                     'male', 'area', 'current_past_smoking', 'any_cad', 'any_family_cad', 
                     'any_cancer_self_report', 'bmi', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1', 
                     'physical_activity', 
                     'drink_five_per_day', 'LBXTC', paste(voi))]
  
  adj_vec <- c('SES_LEVEL' , 'current_past_smoking', 'any_cad', 'any_family_cad', 'any_cancer_self_report',
               'bmi', 'any_ht', 'any_diabetes', 'education', 'RIDRETH1', 'physical_activity',
               'drink_five_per_day', 'LBXTC')
  
  var_vec <- c('male', 'RIDAGEYR')
  
  if(paste(voi) %in% var_vec){
    var_index <- which(var_vec == paste(voi))
    var_vec <- var_vec[-c(var_index)]
  }
  
  if(paste(voi) %in% adj_vec){
    covariates <- as.formula(gsub(paste("\\+ ", voi, sep = ""), "", as.character(covariates)))
  }
  
  
  if(continuous){
    if(log){
      basemodel <- as.formula(paste("Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(", voi, " + 0.1))) +", paste(var_vec, collapse = "+"), "+ cluster(area)"))
    } else {
      basemodel <- as.formula(paste("Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(", voi, ")) +", paste(var_vec, collapse = "+"), "+ cluster(area)"))
    }
  } else {
    basemodel <- as.formula(paste("Surv(PERMTH_EXM, MORTSTAT) ~ ", voi, " +", paste(var_vec, collapse = "+"), "+ cluster(area)"))
  }
  
  dat <- subset(dat, !is.na(weight))
  dat <- dat[complete.cases(dat), ]
  vib <- conductVibration(base_formula = basemodel, dataFrame = dat, adjustby = covariates, family='cox', 
                          weights=dat$weight, print_progress = T, variable = paste(voi)) 
  
  return(vib)
  
}



#####################################################################################
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
    
    args <- list(form, data = dataFrame)
    
    tryCatch(
      est <- do.call(coxph, args),                
      warning = function(w) {
        print(w)
        fail[ii] <<- 1
      }
    )
    
    if(!is.null(est)) {
      ## collect the result
      ## do unweightedEst here...
      
      frm <- coef(summary(est))                                  
      bicMod <- getBIC(est) # do BIC                             
      ## modify the above...
      ### need to get nlevels of variable
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
  return(list(vibration=retFrame, bic=bicFrame, k=k, combinations=varComb, family=family, 
              base_formula=base_formula, adjust=adjustby, warn = fail))
}


#############################################################################################

getBIC <- function(mod) {
  return(extractAIC(mod, k=log(mod$nevent))) # do BIC
}

#############################################################################################

recomputePvalue <- function(allData, zStatColName, pValColName) {
  ### some pvalues estimated at 0 because test statistics so large; recompute their pvalues
  zeroPval <- !is.na(allData[,pValColName]) & (allData[,pValColName] == 0)
  allData[zeroPval, pValColName] <- pnorm(abs(allData[zeroPval, zStatColName]), lower.tail=F)*2 
  return(allData)
}


#############################################################################################

conductVibration <- function(base_formula, dataFrame, adjustby, family = c('gaussian', 'binomial', 'cox'),
                             kMin=NULL, kMax=NULL, print_progress=T, variable, ...) {
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
    vib <- conductVibrationForK(base_formula, dataFrame, adjustby, k, family, print_progress, fail = fail, ...)
    warn[[ii]] <- vib$warn
    retFrame[[ii]] <- vib
    ii <- ii + 1
  }
  ret <- gatherFrames(retFrame)
  
  
  dir_neg <- sum(ret$vibFrame$estimate < 0)
  dir_pos <- sum(ret$vibFrame$estimate > 0)
  nmodels <- dim(ret$vibFrame)[1]
  
  numev <- sum(dataFrame$MORTSTAT)
  
  qhr <- quantile(ret$vibFrame$HR, probs = c(0.01, 0.99))
  RHR <- qhr[2]/qhr[1]
  
  logtenp <- -log10(ret$vibFrame$pvalue)
  qp <- quantile(logtenp, probs = c(0.01, 0.99))
  RP <- qp[2] - qp[1]
  
  ret <- modifyList(ret, list(dir_neg = dir_neg, dir_pos = dir_pos, nmodels = nmodels, numev = numev,
                              name = paste(variable), RHR = RHR, RP = RP, warnings = warn))
  
  return(ret)
}

#############################################################################################

gatherVibration <- function(returnFrames) {                     
  ## gathers up results from multiple runs; see conductVibration
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