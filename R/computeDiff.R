#' @export

computeDiff <- function(yHatMissing, varName, m) {
  ## receber os ids da funcao collectDifs
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
  join$server <- opal::opal.datasource()
  
  ###### Integrar essa parte na fun??o servidor computeDiff ######
  # dataset$ids = as.numeric(row.names(dataset))
  # row.names(dataset) <- NULL
  # complet <- dataset[which(!is.na(dataset$height)),]
  # complet$ids <- as.numeric(rownames(complet))
  # 
  # ids_complete <- unique(as.numeric(join$ids_complete))
  # valuesToImpute <- data.frame(value=complet[which(complet$ids %in% ids_complete),c('height', 'ids')])
  # colnames(valuesToImpute) <- c('height','ids')
  # teste <- merge(x=join, y=valuesToImpute, by.x='ids_complete', by.y='ids')
  # newData <- dataset
  # for (row in 1:nrow(newData)) {
  #   if(row %in% teste$names)
  #     newData[which(row.names(newData) %in% row),'height'] <- teste[sample(which(teste$names %in% row),1),'height']
  # }
  # 
  # ## a partir daqui, buscar a menor diferen?a dentro do pr?prio servidor
  # which(is.na(newData$height))
  
  ## save object newData
  ##############################
  
  #return(environment())
  return(join)

}