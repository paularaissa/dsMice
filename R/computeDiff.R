#' @export

computeDiff <- function(yHatMissing, varName, m) {
  ## receber os ids da função collectDifs
  if(is.character(varName)){
    x <- eval(parse(text=varName))
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }
  
  yHatMiss <- as.vector(unlist(jsonlite::fromJSON(yHatMissing, simplifyMatrix = FALSE)))
  naRows <- which(is.na(x))
  completeValues <- x[-naRows]
  m <- as.numeric(m)
  
  idValor <- c()
  topDiff <- list()
  cont <- 1
  for (idx in 1:length(yHatMiss)) {
    #same x values rownames
    subtract <- data.frame(abs(mapply('-', yHatMiss[idx], completeValues))) 
    colnames(subtract) <- "dif"
    rownames(subtract) <- rownames(completeValues)
    subtract$names <- naRows[idx]
    subtract$miss <- yHatMiss[idx]
    orderedDiff <- subtract[with(subtract, order(dif)), ]
    topDiff[[cont]] <- orderedDiff[1:m,]
    cont <- cont + 1
  }
  join <- do.call(rbind, topDiff)
  join$ids_complete <- rownames(join)
  return(join)

}