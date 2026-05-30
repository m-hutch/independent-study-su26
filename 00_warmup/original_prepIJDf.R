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