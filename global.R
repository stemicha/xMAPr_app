#                                        .(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((    
#                                      (((((                                                                                                               (((( 
#                                   ((((                                                                                                                     (((
#              ,,,,,,             ((((                                                                                                                        ((
#             ,,, ,,             (((                                                                                                                          ((
#             ,, ,,,           (((                            ,,,,,,     ,,,,,,              ,,,,,,,,          ,,,,,,,,,,,,,,,,                               ((
#            ,,,,,,           (((   ,                 ,,   ,,,,,,,,,,,,,,,,,,,,,,,         ,,,,,,,,,,,,        ,,,,,,,,,,,,,,,,,,                             ((
#       ,,,,.,, ,,          (((      ,               ,,   ,,,,      ,,,,,      ,,,,       ,,,,      ,,,,                      ,,,,                          ((( 
#    ,,,, .,,,,,,, ,,      (((        ,,            ,,   ,,,,                   ,,,      ,,,,        ,,,,                      ,,,    (((((((((((((((((((((((   
#      ,,,,,   ,,, ,,,    (((          ,,         .,     ,,,,                   ,,,     ,,,,          ,,,,                    ,,,,    ((             (((        
#               ,,, ,,,  (((            ,,       .,      ,,,,                   ,,,    .,,,            ,,,.    ,,,,,,,,,,,,,,,,,,     ((              *((       
#    ((((((((((( ,,,    ((               ,  ,,,  ,,      ,,,,                   ,,,    ,,,,    ,,,,    ,,,,    ,,,,,,,,,,,,,,,,       ((                ((      
# /(((,,,,,,,,/(((    /((               ,,        ,      ,,,,        ,,,        ,,,   ,,,,     ,,,,     ,,,,   ,,,                    ((                 ((     
#(((,,,,,,,,,,,,,((  (((               ,,          ,     ,,,,        ,,,        ,,,  ,,,,                ,,,,  ,,,                    ((                  ((    
#((,,,,,,,,,,,,,,/(((((               ,             ,,   ,,,,                   ,,,  ,,,,                ,,,,  ,,,                    ((                   ((   
#((,,,,,,,,,,,,,,,((((              ,,               ,,  ,,,,                   ,,,  ,,,,                ,,,,  ,,,                    ((                    ((. 
#((/,,,,,,,,,,,,,(((                ,                 ,  ,,,,                   ,,,                            ,,,                    ((                     ((/
# (((,,,,,,,,,,(((                                                                                                                                              
#   ((((((((((((                                                                                                                                                

# xMAPr - high dynamic range xMAP® data analysis
#
# global.R
# 
# github: https://github.com/stemicha/
# author: Stephan Michalik#
#

## load libraries -----
# deploy without dependencies fro shinyapps IO
source("dependencies.R")

#if WINDOWS is the OP RTools must be installed the default RTools path will be put into PATH environment to call zip
#---------------------------------------------------------------------------------------------------------------------#
if(Sys.info()[['sysname']]=="Windows"){
  path <- Sys.getenv("PATH")
  Sys.setenv("PATH" = paste(path, "C:/RTools/bin", sep = ";")) #add Rtools/bin path for zip.exe usage
}

#---------------------------------------------------------------------------------------------------------------------#


# Bioplex data extract function -------------------------------------------

#file_path = "demo_data/bioplex_data/210906_Coupling control_10 plex.xlsx"
#file_path = "demo_data/bioplex_data/210812_COVID patients_Plate_1.xlsx"

bioplex_raw_data_extraction <- function(file_path = file_path,
                                        coupling_control = F,
                                        sample_naming_sep = "_"){
  
  data_MFI <- read_excel(path = file_path,skip = 7,sheet = "FI")
  data_count <- read_excel(path = file_path,skip = 7,sheet = "Bead Count")
  data_dilution <- read_excel(path = file_path,skip = 7,sheet = "Dilution")
  
  
  
  # MFI data --
  
  #add colnames
  colnames(data_MFI)[1:3] <- data_MFI[1,1:3]
  # remove row
  data_MFI <- data_MFI[-1,]
  #extract MFI
  mfi_raw <- data_MFI[(which(is.na(data_MFI$Type))[3]+3):which(is.na(data_MFI$Type))[4]-1,]
  #replace comma
  mfi_raw[,-c(1:3)] <- apply(mfi_raw[,-c(1:3)],2,function(x){str_replace_all(string = x,pattern = ",",replacement = ".")})
  #numeric values
  mfi_raw[,-c(1:3)] <- apply(mfi_raw[,-c(1:3)],2,function(x){as.numeric(x)})
  
  # count data --
  
  #add colnames
  colnames(data_count)[1:3] <- data_count[1,1:3]
  # remove row
  data_count <- data_count[-1,]
  #extract count
  count_raw <- data_count[(which(is.na(data_count$Type))[3]+3):which(is.na(data_count$Type))[4]-1,]
  #replace comma
  count_raw[,-c(1:3)] <- apply(count_raw[,-c(1:3)],2,function(x){str_replace_all(string = x,pattern = ",",replacement = ".")})
  #numeric values
  count_raw[,-c(1:3)] <- apply(count_raw[,-c(1:3)],2,function(x){as.numeric(x)})
  
  
  # diluton data --
  #add colnames
  colnames(data_dilution)[1:3] <- data_dilution[1,1:3]
  # remove row
  data_dilution <- data_dilution[-1,]
  #extract MFI
  dilution_raw <- data_dilution[(which(is.na(data_dilution$Type))[3]+3):which(is.na(data_dilution$Type))[4]-1,]
  #replace comma
  dilution_raw[,-c(1:3)] <- apply(dilution_raw[,-c(1:3)],2,function(x){str_replace_all(string = x,pattern = ",",replacement = ".")})
  #numeric values
  dilution_raw[,-c(1:3)] <- apply(dilution_raw[,-c(1:3)],2,function(x){as.numeric(x)})
  
  if(coupling_control==T){ #for coupling control data
    #polishing
    count_raw <- count_raw %>% rename(Sample = Description)
    mfi_raw <- mfi_raw %>% rename(Sample = Description)
    
    count_raw <- count_raw[,-c(1,2)]
    mfi_raw <- mfi_raw[,-c(1,2)]
    
    count_raw$Sample <- unlist(sapply(count_raw$Sample,function(x) unlist(strsplit(x = x,split = sample_naming_sep))[1]))
    mfi_raw$Sample <- unlist(sapply(mfi_raw$Sample,function(x) unlist(strsplit(x = x,split = sample_naming_sep))[1]))
    
  }
  if(coupling_control==F){ #for assay data
    #polishing
    #add Blank naming
    mfi_raw <- mfi_raw %>% 
      rename(Sample = Description) %>% 
      mutate(Sample = if_else(condition = Type=="B" & is.na(Sample),
                              true = "Blank",false = Sample))
    count_raw <- count_raw  %>% 
      rename(Sample = Description) %>% 
      mutate(Sample = if_else(condition = Type=="B" & is.na(Sample),
                              true = "Blank",false = Sample))
    
    dilution_raw <- dilution_raw %>% 
      rename(Sample = Description) %>% 
      mutate(Sample = if_else(condition = Type=="B" & is.na(Sample),
                              true = "Blank",false = Sample))
    
    dilution_raw <- dilution_raw[,1:4]
    colnames(dilution_raw)[4] <- "Dilution"
  
    mfi_raw <- left_join(mfi_raw,dilution_raw, by = c("Type","Well","Sample"))
    count_raw <- left_join(count_raw,dilution_raw, by = c("Type","Well","Sample"))
    
    #add Replicate column
    mfi_raw<- mfi_raw %>% 
      group_by(Sample,Dilution) %>% 
      mutate(Replicate = 1:length(Sample)) %>% 
      ungroup()
    
    count_raw<- count_raw %>% 
      group_by(Sample,Dilution) %>% 
      mutate(Replicate = 1:length(Sample)) %>% 
      ungroup()
    
    
    #order columns
    mfi_raw <- mfi_raw %>% 
      select(-Type,-Well) %>% 
      select(Sample,Replicate,Dilution,everything())
    
    count_raw <- count_raw %>% 
      select(-Type,-Well) %>% 
      select(Sample,Replicate,Dilution,everything())
      

    }
 
  
  return(list(mfi_raw = mfi_raw,
              count_raw = count_raw))
  
}



# function for signal drop dection over dilutions -------------------------

signaldrop<-function(mfi,absolute.dilution){
  # signal drop == TRUE
  # NA = NA
  # signal drop == FALSE --> only increasing intensity, usualy first dilution = highest signal
  
  if(sum(!is.na(mfi))==0){
    return(list(
      mfi=mfi,
      absolute.dilution=absolute.dilution,
      exclude=seq(1,length(absolute.dilution)),
      include=NULL,
      logical=rep(NA,length(absolute.dilution))
    ))
    
    
  }else{
    mfi<-mfi[order(absolute.dilution,decreasing = T)]
    mfi.diff<-mfi-max(mfi,na.rm=T)
    exclude<-which(c(seq(1,length(absolute.dilution))-which(mfi.diff==0))<0 | is.na(mfi))
    include<-which(c(seq(1,length(absolute.dilution))-which(mfi.diff==0))>=0 & !is.na(mfi))
    
    decision<-rep(FALSE,length(absolute.dilution))
    if(length(exclude)!=0){
      decision[exclude]<-TRUE
    }
    decision[is.na(mfi)]<-NA
    
    return(list(
      mfi=mfi,
      absolute.dilution=absolute.dilution[order(absolute.dilution,decreasing = T)],
      mfi.diff=mfi.diff,
      exclude=exclude,
      include=include,
      logical=decision
    ))
  }
  

}




## Wrap for test goodnes of fit as it can produce an error -----
get_modelfit <- function(object, x, method, grouping) {
  ans <- try(get_neilltest(object, x, method, grouping),silent=TRUE)
  if(inherits(ans,"try-error")){
    ans <- NA
  } else{
    if(is.character(ans)){
      if(ans=="error1") ans <- "Too many groups in 'grouping'"
      if(ans=="error2") ans <- "Too few groups in 'grouping'"
    } else{
      ans <- as.numeric(round(ans[,2],3))  
    }        
  }
  return(ans)
}

## Estimate of model fit based on neill.test -----
## The code is an adapatation of drc::neill.test but has been 
## changed in some sections because some errors occurs in the original 
## function with 'nls' class models. The drc::neill.test, version 2.3-96
get_neilltest <- function(object, x, method, grouping){
  noCluster <- floor(length(x)/2)
  if(is.null(grouping)){
    if (method == "finest") {
      lenx <- length(x)
      grouping <- floor((1 + 1:lenx)/2)
      grouping[lenx] <- grouping[lenx - 1]
    }
    if (method == "c-finest") {
      for (i in noCluster:(length(coef(object)) + 1)) {
        grouping <- cutree(hclust(dist(x)), k = i)
        if (all(tapply(x, grouping, length) > 1)) {
          break
        }
      }
    }
    if (method == "percentiles") {
      cutVar <- c(-Inf, quantile(x, c(0.2, 0.4, 0.6, 0.8)), 
                  Inf)
      grouping <- cut(x, cutVar)
    }
  }
  
  
  
  
  M <- length(unique(grouping))
  lhs <- as.character(object$m$formula()[[2]])
  parameters <- names(object$m$getPars())
  allobj <- ls(object$m$getEnv())
  rhs <- allobj[-match(c(parameters,lhs),allobj)]
  ndf <- data.frame(get(lhs, object$m$getEnv()), get(rhs, object$m$getEnv()))
  names(ndf) <- c(lhs, rhs)
  norder <- order(ndf[,rhs], decreasing=TRUE)
  ndf <- ndf[norder,]
  
  N <- nrow(ndf)
  denDF <- N - M
  
  ## Checking the number of groups
  mes <- NULL
  if (denDF <= 0)  # (N <= M) 
  {
    # "error1.Too many groups in 'grouping'"
    mes <- "error1"
  }
  p <- N - df.residual(object)
  numDF <- M - p
  
  if (numDF <= 0)  # (M <= p)
  {
    # "error2. Too few groups in 'grouping'"
    mes <- "error2"
  }
  
  if(is.null(mes)){    
    ## Calculating the test statistic
    resVec <- residuals(object)
    resVec <- resVec[norder]
    resAver0 <- tapply(resVec, grouping, mean)
    resAver <- rep(resAver0, tapply(grouping, grouping, length))
    
    resDiff <- resVec - resAver
    FF <- (denDF/numDF)*(sum(resAver*resAver)/(sum(resDiff*resDiff)))
    p <- pf(FF ,numDF, denDF, lower.tail = FALSE)
    ans <- matrix(c(FF, p), 1, 2)
    colnames(ans) <- c("F","p")
  } else {
    ans <- mes
  }
  return(ans)
}

## Get convergence. The criteria from the model is not enough
get_convergence <- function(x) {
  convergence <- x$convInfo$isConv
  ans <- 2
  if(convergence==TRUE) ans <- 1
  return(ans)
}


## Get fitted values
get_fitted<-function(model, bkg.method, bkg.mean, fct) {
  lhs <- as.character(model$m$formula()[[2]])
  parameters <- names(model$m$getPars())
  allobj <- ls(model$m$getEnv())
  rhs <- allobj[-match(c(parameters,lhs),allobj)]
  ndf <- data.frame(get(lhs, model$m$getEnv()), get(rhs, model$m$getEnv()))
  names(ndf) <- c(lhs, rhs)
  
  if(bkg.method=="constraint")  log10.bkgmean <- log10(bkg.mean)
  
  yvalue <- ndf[,lhs]    
  if(bkg.method!="constraint"){
    inv <- invest.fun(model,"noconstraint", fct, yvalue, parameters, NULL)
  } 
  if(bkg.method=="constraint"){
    inv <- invest.fun(model,"constraint", 
                      fct, yvalue, parameters, log10.bkgmean)
  }
  est <- unlist(lapply(1:length(yvalue), function(x) inv$inv[[x]]$est))
  
  form <- unlist(lapply(1:length(yvalue), function(x) inv$form[[x]]))
  
  se <- lapply(form,
               function(x) msm::deltamethod(x, coef(model), vcov(model)))  
  se <- unlist(se)
  
  ans <- as.data.frame(cbind(est, se))
  names(ans) <- c("log10_concentration.fit", "log10_concentratrion.se")
  return(ans)
}

## Estimate of AIC
get_aic <- function(model){
  ans <- AIC(model)
  ans
}
## Model a nls class model
## This is an auxiliar function that returns a function 
## for estimating the derivatives in the loq_derivatives function
getPredict <- function(model) {
  stopifnot(inherits(model,"nls"))
  lhs <- as.character(model$m$formula()[[2]])
  parameters <- names(model$m$getPars())
  allobj <- ls(model$m$getEnv())
  rhs <- allobj[-match(c(parameters,lhs),allobj)]
  fx <- function(x) {
    newlist <- list(x)
    names(newlist) <- rhs
    predict(model,newdata=newlist)
  }
  assign("model", model, envir=environment(fx))
  assign("rhs", rhs, envir=environment(fx))
  return(fx)  
}



# Estimate of Rsquared -----
# summary(model)$sigma = sqrt(sum(residuals(model)^2)/df)
get_rsq <- function(model, adjusted=TRUE){
  lhs <- as.character(model$m$formula()[[2]])
  parameters <- names(model$m$getPars())
  allobj <- ls(model$m$getEnv())
  rhs <- allobj[-match(c(parameters,lhs),allobj)]
  ndf <- data.frame(get(lhs, model$m$getEnv()), get(rhs, model$m$getEnv()))
  names(ndf) <- c(lhs, rhs)
  
  tss.fit <- var(ndf[,lhs])
  
  if(adjusted==TRUE){
    rss.df <- summary(model)$df[2]-1
    rss.fit <- sum(residuals(model)^2)/rss.df    
  } else {
    rss.fit <- sum(residuals(model)^2)/(length(residuals(model))-1)
  }
  
  rsquare.fit <- 1 - (rss.fit/tss.fit)
  return(rsquare.fit)  
}



#SSL5 selfstart -----
SSl5 <- selfStart( ~ lAsym + (hAsym-lAsym)/(1+10^(Slope*(x-xMid)))^Asymetry,
                   function(mCall, LHS, data)
                   {
                     xy <- sortedXyData(mCall[["x"]], LHS, data)
                     plowl<- NLSstLfAsymptote(xy)
                     phighl<-NLSstRtAsymptote(xy)
                     pslope<- -1
                     pf<-1
                     ped50<-NLSstClosestX(xy, mean(xy[,"y"]))
                     value<-c(pslope,plowl,phighl,ped50,pf)
                     names(value)<-mCall[c("Slope","lAsym","hAsym","xMid","Asymetry")]
                     value
                   },c("Slope","lAsym","hAsym","xMid","Asymetry"))


# Inverse function for the SSl5  (x ~ fx)
# 
# @param fx MFI value
# @param b slope parameter
# @param c low asymptote parameter
# @param d high asymptote parameter
# @param e mid concentration parameter
# @param f asymetry parameter
inSSl5 <- function(fx, b, c, d, e, f){
  ans <- list()  
  ans$est <- (log( ((d-c)/(fx-c))^(1/f) - 1  ) * 1/(log(10)*b)) + e
  t1 <- "~ (log( ((x3-x2)/(fx-x2))^(1/x5) - 1  ) * 1/(log(10)*x1)) + x4"
  ans$formtext <- t1
  t2 <- "~ (log( ((x2-cons)/(fx-cons))^(1/x4) - 1  ) * 1/(log(10)*x1)) + x3"
  ans$formtext.cons <- t2
  ans
}






