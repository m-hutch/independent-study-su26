rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(TexMix)
library(sp)
library(spdep)
setwd("italy_example/ItalyData")

source("../../new_prepIJDF.R")

##
## Get polygons of neighboring countries
##

neigShp <- sf::st_read("Neighbors.shp")
neigShp <- as(neigShp, "Spatial")

##
## Get polygons of Italy provinces
##

ItalyShp <- sf::st_read("Provinces.shp", stringsAsFactors=T)
ItalyShp <- as(ItalyShp, "Spatial")

bBox <- sp::bbox(ItalyShp)                   # bounding box of Italy
centroid <- sp::coordinates(ItalyShp)        # provincial centroids
dij <- sp::spDists(centroid, longlat= TRUE)  # great circle distance based on population centroids
diag(dij) <- NA

######
## Basic Mapping Functions
##

## EX: Regions of Italy
sp::plot(neigShp, axes = T,col=grey(0.9),border = "white", 
         xlim=bBox[1,], ylim=bBox[2,])
mapColorQual(as.factor(ItalyShp$REGION), ItalyShp, map.title="Regions of Italy", 
             legend.title = "Regions",add.to.map=T)

## EX: Italy Fertility Rate
sp::plot(neigShp, axes = T,col=grey(0.9),border = "white", 
         xlim=bBox[1,], ylim=bBox[2,])
mapColorRamp(ItalyShp$TOTFERTRAT,ItalyShp, breaks=7, map.title="Italy Fertility Rate ",
             legend.title="Fertility Rate",add.to.map=T, legend.cex=0.7)

## Ex: Itally Migration Ratio
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
mij <- as.matrix(mij)
diag(mij) <- NA

##
## Mapping dominant migration flows
##

## 1 % largest flows, i.e., > 15197 mij
bigmig <- mij > quantile(as.vector(as.matrix(mij)), prob=0.99, na.rm=TRUE)
diag(bigmig) <- FALSE
prov.link <- mat2listw(as.matrix(bigmig), style="B", zero.policy=TRUE)$neighbours

prov.centroid <- coordinates(ItalyShp)
plot(neigShp,axes=T,col=grey(0.9),border="white",
     xlim=bBox[1,],ylim=bBox[2,])                                # First background
plot(ItalyShp,col="palegreen3" ,border=grey(0.9), axes=T, add=T) # Second plot areas
plot(prov.link,coords=prov.centroid, pch=19, cex=0.1,            # Third plot links focused at centroids
     arrows=TRUE, length=0.05, col="blue", add=T)
title("1% of Largest Migration Flows among Provinces")
box()

##
## Prep the data
##
dfShort <- ItalyShp@data
dfShort <- dfShort[,c("PROVNAME", "REGION", "TOTPOP94", "TOTFERTRAT",
                  "FEMMARAGE9", "DIVORCERAT", "ILLITERRAT", "TELEPERFAM")]
df <- build_od_pairs(dfShort, mij=mij, dij=dij, transform = 'log.standard')


##
## Classical interaction model
##
# no need to subset df, glm na.action is na.omit by default
mig1.glm <- glm(mij~log(dij)+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, family=poisson)

summary(mig1.glm)
car::vif(mig1.glm)
coefplot::coefplot(mig1.glm, intercept=FALSE, title="Classical Interaction Model")

## Estimate over-dispersion
degFree <- mig1.glm$df.residual    # degFree=8926
sum((residuals(mig1.glm, type="pearson"))^2) / degFree

## with over-dispersion
mig2.glm <- glm(mij~log(dij)+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, family=quasipoisson)
summary(mig2.glm)
coefplot::coefplot(mig2.glm, intercept=FALSE, title="Classical Quasi-Interaction Model")

## Number of observed and predicted flows is identical => unbiased prediction
sum(predict(mig2.glm, type="response"))
sum(df$mij, na.rm=T)


## Plot on the log-log scale
plot(na.omit(df$mij), predict(mig2.glm, type="response"), log="xy",
     xlab="Observed Migration Flows", ylab="Predicted Migration Flows")
abline(a=0,b=1,col="red")


## Negative binomial specification modelling over-dispersion
mig2nb.glm <- MASS::glm.nb(mij~log(dij)+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df,
                           control = glm.control(epsilon = 1e-8, maxit = 100, trace = FALSE))
summary(mig2nb.glm)
coefplot::coefplot(mig2nb.glm, intercept=FALSE, title="Classical NB-Interaction Model")

## Number of observed and predicted flows is different => biased prediction
sum(predict(mig2nb.glm, type="response"))
sum(df$mij, na.rm=T)
plot(na.omit(df$mij), predict(mig2nb.glm, type="response"), log="xy",
     xlab="Observed Migration Flows", ylab="Predicted Migration Flows")
abline(a=0,b=1,col="red")

##
## Doubly constrained interaction model in centered coding scheme
## Observed and estimated inflows and outflow are identically
##
mig4.glm <- glm(mij~log(dij)+PROVNAME.iF+PROVNAME.jF, data=df, family=poisson,
                contrast = list(PROVNAME.iF="contr.sum", PROVNAME.jF="contr.sum"))
summary(mig4.glm)
plot(na.omit(df$mij), predict(mig4.glm, type="response"), log="xy",
     xlab="Observed Migration Flows", ylab="Predicted Migration Flows")
abline(a=0,b=1,col="red")

## Compare observed and predicted Inflows
ObsInflow <- tapply(df$mij, df$PROVNAME.iF, sum, simplify = T)
PredInflow <- tapply(predict(mig4.glm, type="response"), 
                     na.omit(df$PROVNAME.iF), sum, simplify = T)
cbind(1:95,ObsInflow,PredInflow)

## Dito: Compare observed and predicted Outflows
ObsOutflow <- tapply(df$mij, df$PROVNAME.jF, sum, simplify = T)
PredOutflow <- tapply(predict(mig4.glm, type="response"), 
                      na.omit(df$PROVNAME.jF), sum, simplify = T)
cbind(1:95,ObsOutflow,PredOutflow)

##
## Find the optimal transformation parameter dij
##
lambda <- seq(-1.4, -0.4, 0.01) 
LR <- c()                       # Likelihood ratio vector
df$dij <- ifelse(df$dij==0, 0.1, df$dij)
for (i in 1:length(lambda)){
  temp <- glm(mij~car::bcPower(dij,lambda[i])+TOTPOP94.iLZ+TOTPOP94.jLZ, data=df, 
              family=poisson)
  LR[i] <- logLik(temp)
}

lambdaMax <- lambda[which.max(LR)]                  #find optimal lambda
plot(lambda, LR, type="l", ylab="log-likelihood", xlab=expression(lambda),
     main="Optimal Lambda")
abline(v = lambdaMax, col="red", lwd=3, lty=2)
text(-0.6, max(LR), bquote(paste("max ", lambda ==.(lambdaMax))), cex = 1.5)

## Consequently, the log-transformation is not sufficient

##
## Impact on the region specific distance distributions
##
iOrdProv <- reorder(df$PROVNAME.iF, df$dij, median)
boxplot(dij~iOrdProv, data=df, 
        main="Inter-provincial Distance Distributions by Origin \nbefore Box-Cox transformation",
        xlab="Province", ylab="Origin-Destination Distance", las=2)

## boxplot after Box-Cox transformation
boxplot(car::bcPower(dij,lambdaMax)~iOrdProv, data=df,
        main="Inter-provincial Distance Distributions by Origin \nafter Box-Cox Transformation",
        xlab="Province", ylab="Transformed-Destination Distance", las=2)

##
## Augmented mode with orgin and destination attributes
##
mig5.glm <- glm(mij~car::bcPower(dij,lambdaMax)+TOTPOP94.iLZ+TOTPOP94.jLZ+
                  TOTFERTRAT.iLZ+TOTFERTRAT.jLZ+
                  FEMMARAGE9.iLZ+ FEMMARAGE9.jLZ+
                  DIVORCERAT.iLZ+DIVORCERAT.jLZ+
                  ILLITERRAT.iLZ+ILLITERRAT.jLZ+
                  TELEPERFAM.iLZ+TELEPERFAM.jLZ,
                data=df, family=poisson)
summary(mig5.glm)
car::vif(mig5.glm)
coefplot::coefplot(mig5.glm, predictors=c("TOTPOP94.iLZ","TOTPOP94.jLZ",
                                          "TOTFERTRAT.iLZ","TOTFERTRAT.jLZ",
                                          "FEMMARAGE9.iLZ","FEMMARAGE9.jLZ",
                                          "DIVORCERAT.iLZ","DIVORCERAT.jLZ",
                                          "ILLITERRAT.iLZ","ILLITERRAT.jLZ",
                                          "TELEPERFAM.iLZ","TELEPERFAM.jLZ"),
                     intercept=FALSE, title="Augmented Interaction Model")

plot(na.omit(df$mij), predict(mig5.glm, type="response"), log="xy",
     xlab="Observed Migration Flows", ylab="Predicted Migration Flows")
abline(a=0,b=1,col="red")
##
## Augmented model with orgin/destination ratio attributes
##
mig6.glm <- glm(mij~car::bcPower(dij,lambdaMax)+TOTPOP94.iLZ+TOTPOP94.jLZ+
                  TOTFERTRAT.ijLZ+FEMMARAGE9.ijLZ+DIVORCERAT.ijLZ+
                  ILLITERRAT.ijLZ+TELEPERFAM.ijLZ,
                data=df, family=poisson)
summary(mig6.glm)
car::vif(mig6.glm)
coefplot::coefplot(mig6.glm, predictors=c("TOTPOP94.iLZ","TOTPOP94.jLZ",
                                          "TOTFERTRAT.ijLZ",
                                          "FEMMARAGE9.ijLZ",
                                          "DIVORCERAT.ijLZ",
                                          "ILLITERRAT.ijLZ",
                                          "TELEPERFAM.ijLZ"),
                   intercept=FALSE, title="Augmented Interaction Model")

plot(na.omit(df$mij), predict(mig6.glm, type="response"), log="xy",
     xlab="Observed Migration Flows", ylab="Predicted Migration Flows")
abline(a=0,b=1,col="red")

