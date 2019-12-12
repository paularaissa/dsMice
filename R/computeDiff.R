#' @export

computeDiff <- function(yHatMissing, varName) {
  
  if(is.character(varName)){
    x <- eval(parse(text=varName))
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }
  yHatMiss <- as.vector(unlist(jsonlite::fromJSON(yHatMissing, simplifyMatrix = FALSE)))
  
  naRows <- which(is.na(x))
  completeValues <- x[-naRows]
  
  #return(list(miss=yHatMiss, complete=completeValues))
  
  idValor <- c()
  topDiff <- list()
  cont <- 1
  return(length(yHatMiss))
  # for (idx in 1:length(yHatMis)) {
  #   # subtract <- data.frame(abs(mapply('-', value, completeValues))) #same x values rownames
  #   # colnames(subtract) <- "dif"
  #   # rownames(subtract) <- rownames(completeValues)
  #   # subtract$names <- rownames(subtract)
  #   # orderedDiff <- subtract[with(subtract, order(dif)), ]
  #   # topDiff[[cont]] <- orderedDiff[1:m,]
  #   #candidateMap <- sample(topDiff[,"names"], 1)
  #   #idValor <- orderedDiff[1,"names"]
  #   #randomValue <- xValuesComplete[candidateMap, 1]
  #   #imputedValues[cont] <- randomValue
  #   return(yHatMiss[idx])
  #   #cont <- cont + 1
  # }
  # return(yHatMiss)
  # 
}