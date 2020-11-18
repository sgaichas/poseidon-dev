# Code from Kerim Aydin for diet sampling in AK, 18 Nov 2020

setwd("C:/src/octo2016")
              
require(VGAM)

rawdat   <- read.csv("BS_allPrey_out.csv")
preylook <- read.csv("Ecopath_diet_lookup_octo.csv")
preycol <- "OCTO"

LCLASS <- c(0,40,60,150)
preds    <- list("P.Cod" = "P. Cod")
lenlist  <- c("[0,40)", "[40,60)", "[60,150)")
yrs      <- c(1984:2011,2014:2015)
DOMS     <- c("InnerNW","InnerSE","MiddleNW","MiddleSE","OuterNW","OuterSE")

DETECT <- 0.0001 # Grams of empty prey for proportioning
SAMPLES <- 10000

# Build a crosstab-query (per-predator) using names in GOA_MED column
  guild <- as.character(preylook[,preycol])
  names(guild) <- as.character(preylook$PREYNAME)
  glist <- guild[as.character(rawdat$ECOPATH_PREY)]     
  gdat <- aggregate(rawdat$TWT,list(as.character(rawdat$PREDJOIN),glist),sum)
  gtab <- tapply(rawdat$TWT,list(as.character(rawdat$PREDJOIN),glist),sum)
  gtab[is.na(gtab)]<-0
  predtab <- unique(rawdat[,1:23])
  preycross <- gtab[as.character(predtab$PREDJOIN),]
  allprey   <- colnames(preycross)
  lbin  <- cut(predtab$PRED_LEN,LCLASS,right=F)
  GDAT<-cbind(predtab,lbin,preycross)

# Halibut mm to cm regression for a (6.291e-6)*10^3.24 =  0.01093251
A_L <- c("W.Pollock"=0.00553096, "P.Cod"=0.00411781, "Arrowtooth"=0.00443866,"P.Halibut"=0.01093251)
B_L <- c("W.Pollock"=3.044172,   "P.Cod"=3.25325765, "Arrowtooth"=3.19894001,"P.Halibut"=3.24)

#FinalEst  <- NULL
Nests     <- length(yrs)*length(preds)*length(lenlist)*length(DOMS)
alphalist <- matrix(NA,Nests,length(allprey))
colnames(alphalist)<-allprey
Nstomachs <- Nresamp <- Wdetect <- twtMean  <- twtSD <- cperwMean <- cperwSD  <- rep(NA,Nests) 
ThisSp <- ThisDom <- ThisLen <- ThisYr <- rep(NA,Nests) 

YIND <- 0
for (SPN in 1:(length(preds))){
  sp <- names(preds)[SPN] 
  spcodes <- preds[[sp]]
        TotWt0 <- as.numeric(rowSums(GDAT[,allprey]))
        SURV   <- GDAT[GDAT$CRUISE_TYPE=="Race_Groundfish" & 
                  GDAT$MONTH%in%6:8 & 
                  GDAT$ECOPATH_PRED%in%spcodes &
                  TotWt0>0.0, ] 
  for (yr in yrs){
    for (dom in DOMS){
      for (sbin in lenlist){
        YIND <- YIND + 1
        cat(sp,yr,dom,sbin,YIND,"of",Nests,"\n"); flush.console() 
        SELPRED <- SURV[SURV$lbin%in%sbin &
                        SURV$ECOPATH_PP%in%dom & 
                        SURV$YEAR%in%yr, ]        
        Nind <- length(SELPRED[,1])
        if (Nind>=10){
            allfood <- SELPRED[,allprey]
            goodprey <- allfood[,colSums(allfood)>0]
            #Breaks if only one prey type is >0 (vector conversion); TODO fix
            sptot    <- colSums(goodprey)
            spadd    <- DETECT*sptot/sum(sptot)
            SCI <- t(t(goodprey)+spadd)/ (A_L[sp] * (SELPRED$PRED_LEN ^ B_L[sp]))
            sp_prey  <- colnames(goodprey)
            
            # Make matrix of individual stomachs
              sampmat <- matrix(as.numeric(unlist(SCI)),length(SCI[,1]),length(sp_prey))
              colnames(sampmat)<-sp_prey
              #Nind <- length(sampmat[,1])
      
           # Resample sums 
             resampmat <- matrix(NA,SAMPLES,length(sp_prey)) 
             colnames(resampmat) <- sp_prey
             tot_wt <- cperw <- rep(NA,SAMPLES)
             for (i in 1:SAMPLES){
               IND <- sample.int(Nind, Nind, replace = T)
               tot_wt[i]    <- sum(rowSums(sampmat[IND,]))
               cperw[i]     <- sum(rowSums(sampmat[IND,]))/Nind
               pathsum <- colSums(sampmat[IND,]) #+ spadd
               resampmat[i,]<-pathsum
             }
      
           # Fit resampled matrix to dirichlet
             sampprop <- resampmat/rowSums(resampmat)  
             tfit <- vglm(sampprop~1,dirichlet)
             alphas <- Coef(tfit)

# Final accumulation for loop
  alphalist[YIND,sp_prey] <- alphas
  Nstomachs[YIND]  <- Nind
  Nresamp[YIND]    <- SAMPLES
  Wdetect[YIND]    <- DETECT
  twtMean[YIND]    <- mean(tot_wt) 
  cperwMean[YIND]  <- mean(cperw)
  twtSD[YIND]      <- sd(tot_wt) 
  cperwSD[YIND]    <- sd(cperw)
 }
 else{
  #alphalist[YIND,sp_prey] <- alphas
  Nstomachs[YIND]  <- length(SELPRED[,1])
  Nstomachs[YIND]  <- Nind
  Nresamp[YIND]    <- NA
  Wdetect[YIND]    <- NA
  twtMean[YIND]    <- NA 
  cperwMean[YIND]  <- NA
  twtSD[YIND]      <- NA 
  cperwSD[YIND]    <- NA    
 }
 ThisSp[YIND] <- sp
 ThisYr[YIND] <- yr
 ThisDom[YIND] <- dom
 ThisLen[YIND] <- sbin
}}}}

alphalist[is.na(alphalist)]<-0
alpha0 <- rowSums(alphalist)
OutEst <- data.frame(ThisSp,ThisYr,ThisDom,ThisLen,Nstomachs,Nresamp,Wdetect,twtMean,twtSD,cperwMean,cperwSD,alpha0,alphalist)

write.csv(OutEst,"DietOutEst.csv",row.names=F)

