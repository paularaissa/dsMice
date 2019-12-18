#' @export

computeDiff <- function(yHatMissing, varName, m) {
  
  if(is.character(varName)){
    x <- eval(parse(text=varName))
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }
  
  yHatMiss <- as.vector(unlist(jsonlite::fromJSON(yHatMissing, simplifyMatrix = FALSE)))
  naRows <- which(is.na(x))
  completeValues <- x[-naRows,]
  m <- as.numeric(m)
  
  return(completeValues)
  
  idValor <- c()
  topDiff <- list()
  cont <- 1
  for (value in yHatMiss) {
    subtract <- data.frame(abs(mapply('-', value, completeValues))) #same x values rownames
    colnames(subtract) <- "dif"
    rownames(subtract) <- rownames(completeValues)
    subtract$names <- rownames(subtract)
    subtract$miss <- value
    return(list(subtract, completeValues))
    orderedDiff <- subtract[with(subtract, order(dif)), ]
    #topDiff[[cont]] <- orderedDiff[1:m,]
    topDiff[[cont]] <- subtract[1:m,]
    cont <- cont + 1
  }
  #join <-  do.call(rbind, topDiff)
  return(topDiff)

}