rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(TexMix)
setwd("E:\\Lectures2021\\GISC7364\\Week13\\ItalyData")

prepIJDf <- function(df, mij=NULL, dij=NULL, logTrans=FALSE, zTrans=FALSE){
  ##############################################################################
  ## Indices:
  ##           '.i'    for origin 
  ##           '.j'    for destination
  ##           '.ij'   for origin/destination ratio
  ##           '.ijF'  factor variable
  ##           '.ijL'  log-transformed variable
  ##           '.ijLZ' log- and z-transformed variable
  ##           '.ijZ'  z-transformed variable
  ## Case treatments:
  ##            1 Factor
  ##            2 NoFactor-possitive
  ##            3 NoFactor-possitive-log
  ##            4 NoFactor-possitive-log-scale
  ##            5 NoFactor-possitive-scale
  ##            6 NoFactor
  ##            7 NoFactor-scale
  ##############################################################################
  if(!is.data.frame(df)) stop("Function prepIJDf: Input not a dataframe")
  
  ## Setup origin and destination ids and unique record id with the format: 
  ## 'ij' for n < 10, iijj' for n < 100 and 'iiijjj' for n < 1000 etc.
  recId <- expand.grid(1:nrow(df),1:nrow(df))
  odDf <- data.frame(ID.i=recId$Var1, ID.j=recId$Var2,
                     ID.ij=recId$Var1*10^trunc(log(nrow(df),base=10)+1)+recId$Var2)
  
  ## Add vectorized migration flows
  if (is.null(mij)) { odDf <- data.frame(odDf, mij=NA) } else {
    odDf <- data.frame(odDf, mij=matrix(as.matrix(mij, nrow=nrow(mij)^2)) ) }
  
  ## Add vectorized distances
  if (is.null(dij)) { odDf <- data.frame(odDf, dij=NA) } else {
    odDf <- data.frame(odDf, dij=matrix(as.matrix(dij, nrow=nrow(dij)^2)) ) }
  
  ## Cycle over all variables
  dfNames <- names(df)  
  for (i in 1:length(dfNames)) {
    odVec <- expand.grid(df[,i],df[,i])
    
    if (is.factor(df[,i])) {                              # process factors
      odVar <- data.frame(odVec[,1],odVec[,2])
      names(odVar) <- c(paste0(dfNames[i],".iF"), paste0(dfNames[i],".jF"))
      odVar[,1] <- factor(odVec[,1], labels=levels(df[,i]))
      odVar[,2] <- factor(odVec[,2], labels=levels(df[,i]))
    } else { 
      if(!(any(df[,i] <= 0))){                      # process all metric positive variables                                
        if (!logTrans & !zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2],log( odVec[,1]/odVec[,2]))
          names(odVar) <- c(paste0(dfNames[i],".i"),paste0(dfNames[i],".j"),
                            paste0(dfNames[i],".ijL"))
        } 
        if (logTrans & !zTrans){
          odVar <- data.frame(log(odVec[,1]),log(odVec[,2]),log(odVec[,1]/odVec[,2]))
          names(odVar) <- c(paste0(dfNames[i],".iL"),paste0(dfNames[i],".jL"),
                            paste0(dfNames[i],".ijL"))        
        }
        if (logTrans & zTrans){
          odVar <- data.frame(log(odVec[,1]),log(odVec[,2]),log( odVec[,1]/odVec[,2]))
          odVar <- as.data.frame(scale(odVar))
          names(odVar) <- c(paste0(dfNames[i],".iLZ"),paste0(dfNames[i],".jLZ"),
                            paste0(dfNames[i],".ijLZ"))           
        }
        if (!logTrans & zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2],log( odVec[,1]/odVec[,2]))
          odVar <- as.data.frame(scale(odVar))
          names(odVar) <- c(paste0(dfNames[i],".iZ"),paste0(dfNames[i],".jZ"),
                            paste0(dfNames[i],".ijLZ")) 
        }           
      } else {
        if (!zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2])
          names(odVar) <- c(paste0(dfNames[i],".i"),paste0(dfNames[i],".j"))             
        }
        if (zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2])
          odVar <- as.data.frame(scale(odVar))
          names(odVar) <- c(paste0(dfNames[i],".iZ"),paste0(dfNames[i],".jZ"))}
      }##end::variable support
    } ##end::isFactor
    odDf <- data.frame(odDf, odVar)
  } #end::for
  return(odDf)
} #end::prepIJDf

##
## Get polygons of neighboring countries
##
neigShp <- rgdal::readOGR(dsn=".",layer = "Neighbors", integer64 ="allow.loss",
                          stringsAsFactors=F)
##
## Get polygons of Italy provinces
##
ItalyShp <- rgdal::readOGR(dsn=".",layer = "Provinces", integer64 = "allow.loss",
                           stringsAsFactors=T)
bBox <- sp::bbox(ItalyShp)                   # bounding box of Italy
centroid <- sp::coordinates(ItalyShp)        # provincial centroids
dij <- sp::spDists(centroid, longlat= TRUE)  # great circle distance based on population centroids

##
## Basic Mapping Functions
##
sp::plot(neigShp, axes = T,col=grey(0.9),border = "white", 
         xlim=bBox[1,], ylim=bBox[2,])
mapColorQual(as.factor(ItalyShp$REGION), ItalyShp, map.title="Regions of Italy", 
             legend.title = "Regions",add.to.map=T)

sp::plot(neigShp, axes = T,col=grey(0.9),border = "white", 
         xlim=bBox[1,], ylim=bBox[2,])
mapColorRamp(ItalyShp$TOTFERTRAT,ItalyShp, breaks=7, map.title="Italy Fertility Rate ",
             legend.title="Fertility Rate",add.to.map=T, legend.cex=0.7)

ItalyShp$logMigRatio <- log(ItalyShp$INFLOW/ItalyShp$OUTFLOW)
hist(ItalyShp$logMigRatio)
sum(ItalyShp$logMigRatio <= 0)
sum(ItalyShp$logMigRatio >= 0)
sp::plot(neigShp, axes = T,col=grey(0.9),border = "white", 
         xlim=bBox[1,], ylim=bBox[2,])
mapBiPolar(ItalyShp$logMigRatio, ItalyShp, neg.breaks=3, pos.breaks=5, break.value=0,
           map.title="Italy Migration Ratio", legend.title="Loss versus Win",add.to.map=T,
           legend.cex=0.9)
##
## Get migration matrix
##
mij <- foreign::read.dbf("ItalyProvMig.dbf") 
mij <- mij[, 2:96]      # Remove origin label

##
## Prep the data
##
dfShort <- ItalyShp@data
dfShort <- dfShort[,c("PROVNAME", "REGION", "TOTPOP94", "TOTFERTRAT",
                  "FEMMARAGE9", "DIVORCERAT", "ILLITERRAT", "TELEPERFAM")]
df <- prepIJDf(dfShort, mij=mij, dij=dij, logTrans=TRUE, zTrans=TRUE)

##
## Classical interaction model
##
mig1.glm <- glm(mij~log(dij)+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, family=poisson, 
                subset=(ID.i!=ID.j))
summary(mig1.glm)
car::vif(mig1.glm)
coefplot::coefplot(mig1.glm, intercept=FALSE, title="Classical Interaction Model")

## Estimate over-dispersion
degFree <- 8926
sum((residuals(mig1.glm, type="pearson"))^2) / degFree

## with over-dispersion
mig2.glm <- glm(mij~log(dij)+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, family=quasipoisson, subset=(ID.i!=ID.j))
summary(mig2.glm)
coefplot::coefplot(mig2.glm, intercept=FALSE, title="Classical Quasi-Interaction Model")

sum(predict(mig2.glm, type="response"))
sum(df$mij[df$ID.i!=df$ID.j])
plot(log(df$mij[df$ID.i!=df$ID.j]), log(predict(mig2.glm, type="response")))
abline(a=0,b=1,col="red")

## Negative binomial specification modelling over-dispersion
mig2nb.glm <- MASS::glm.nb(mij~log(dij)+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, subset=(ID.i!=ID.j),
                           control = glm.control(epsilon = 1e-8, maxit = 100, trace = FALSE))
summary(mig2nb.glm)
coefplot::coefplot(mig2nb.glm, intercept=FALSE, title="Classical NB-Interaction Model")

sum(predict(mig2nb.glm, type="response"))
sum(df$mij[df$ID.i!=df$ID.j])
plot(log(df$mij[df$ID.i!=df$ID.j]), log(predict(mig2nb.glm, type="response")))
abline(a=0,b=1,col="red")

##
## Doubly constrained interaction model in cornered coding scheme
## Observed and estimated inflows and outflow are identically
##
mig3.glm <- glm(mij~log(dij)+PROVNAME.iF+PROVNAME.jF, data=df, family=poisson, subset=(ID.i!=ID.j))
summary(mig3.glm)
car::vif(mig3.glm)

## Doubly constrained interaction model in centered coding scheme
mig4.glm <- glm(mij~log(dij)+PROVNAME.iF+PROVNAME.jF, data=df, family=poisson, subset=(ID.i!=ID.j), 
                contrast = list(PROVNAME.iF="contr.sum", PROVNAME.jF="contr.sum"))
summary(mig4.glm)
plot(log(df$mij[df$ID.i!=df$ID.j]), log(predict(mig4.glm, type="response")))
abline(a=0,b=1,col="red")

## Compare observed and predicted Inflows
ObsInflow <- tapply(df$mij, df$PROVNAME.iF, sum, simplify = T)
PredInflow <- tapply(predict(mig4.glm, type="response"), 
                     df$PROVNAME.iF[df$ID.i!=df$ID.j], sum, simplify = T)
cbind(1:95,ObsInflow,PredInflow)

## Dito: Compare observed and predicted Outflows
ObsOutflow <- tapply(df$mij, df$PROVNAME.jF, sum, simplify = T)
PredOutflow <- tapply(predict(mig4.glm, type="response"), 
                      df$PROVNAME.jF[df$ID.i!=df$ID.j], sum, simplify = T)
cbind(1:95,ObsOutflow,PredOutflow)

##
## Find the optimal transformation parameter dij
##
lambda <- seq(-1.4, -0.4, 0.01) 
df$dij[df$ID.i==df$ID.j] <- 1   # just avoid zero distances in ID.i==ID.j
LR <- c()                       # Likelihood ratio vector
for (i in 1:length(lambda)){
  temp <- glm(mij~car::bcPower(dij,lambda[i])+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, 
              family=poisson, subset=(ID.i!=ID.j))
  LR[i] <- logLik(temp)
}

lambdaMax <- lambda[which.max(LR)]                  #find optimal lambda
plot(lambda, LR, type="l", ylab="log-likelihood", xlab=expression(lambda),
     main="Optimal Lambda")
abline(v = lambdaMax, col="red", lwd=3, lty=2)
text(-0.6, max(LR), expression(paste("max(", lambda, ")=-0.93")),cex = 1.5)

##
## Impact on the region specific distance distributions
##
iOrdProv <- reorder(df$PROVNAME.iF, df$dij, median)
boxplot(dij~iOrdProv, data=df, subset=(ID.i!=ID.j), 
        main="Inter-provincial Distance Distributions by Origin
        \nbefore Box-Cox transformation",
        xlab="Province", ylab="Origin-Destination Distance", las=2)

## boxplot after Box-Cox transformation
boxplot(car::bcPower(dij,lambdaMax)~iOrdProv, data=df, subset=(ID.i!=ID.j),
        main="Inter-provincial Distance Distributions by Origin
        \nafter Box-Cox Transformation",
        xlab="Province", ylab="Transformed-Destination Distance", las=2)

##
## Augmented model
##
mig5.glm <- glm(mij~car::bcPower(dij,lambdaMax)+TOTPOP94.iLZ+TOTPOP94.jLZ+
                  TOTFERTRAT.ijLZ+FEMMARAGE9.ijLZ+DIVORCERAT.ijLZ+
                  ILLITERRAT.ijLZ+TELEPERFAM.ijLZ,
                data=df, family=poisson, subset=(ID.i!=ID.j))
summary(mig5.glm)
car::vif(mig5.glm)
coefplot::coefplot(mig5.glm, intercept=FALSE, title="Augmented Interaction Model")

plot(log(df$mij[df$ID.i!=df$ID.j]), log(predict(mig5.glm, type="response")))
abline(a=0,b=1,col="red")

