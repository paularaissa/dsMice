#' @export

computeDiff <- function(yHatMissing, varName) {
  
  if(is.character(varName)){
    x <- eval(parse(text=varName))
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }
  yHatMiss <- jsonlite::fromJSON(yHatMissing, simplifyMatrix = FALSE)
  
  naRows <- which(is.na(x))
  completeValues <- x[-naRows]
  
  idValor <- c()
  topDiff <- list()
  for (value in yHatMis) {
    subtract <- data.frame(abs(mapply('-', value, completeValues))) #same x values rownames
    colnames(subtract) <- "dif"
    rownames(subtract) <- rownames(completeValues)
    subtract$names <- rownames(subtract)
    orderedDiff <- subtract[with(subtract, order(dif)), ]
    topDiff[[cont]] <- orderedDiff[1:m,]
    #candidateMap <- sample(topDiff[,"names"], 1)
    #idValor <- orderedDiff[1,"names"]
    #randomValue <- xValuesComplete[candidateMap, 1]
    #imputedValues[cont] <- randomValue
    cont <- cont + 1
  }
  return(topDiff)
  
}