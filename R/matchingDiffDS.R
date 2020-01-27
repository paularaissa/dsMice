#'@export
matchingDiffDS <- function(obj, rank, varName) {
  
  ranking <- jsonlite::fromJSON(rank, simplifyMatrix = FALSE) 
  recovery.obj <- eval(parse(text=obj))
  
  idx <- 1
  map <- list()
  tol = 1e-5
  for (value in ranking) {
    map[[idx]] <- recovery.obj[(which((abs(value - recovery.obj$dif)) <= tol)),]
    idx <- idx + 1
  }
  join <- do.call(rbind, map)
  
  rows_to_impute <- unique(join$names)
  
  x <- eval(parse(text=varName))
  missing_values <- which(is.na(x))
  
  values_to_impute <- x[rows_to_impute]
  
  dataset <- eval(parse(text="D"))
  newDataSet <- dataset
  
  #newDataSet[which(rownames(newDataSet) %in% rows_to_impute), varName] <- values_to_impute
  
  dataset$ids = as.numeric(row.names(dataset))
  row.names(dataset) <- NULL
  complet <- dataset[which(!is.na(dataset$height)),]
  complet$ids <- as.numeric(rownames(complet))
  
  ids_complete <- unique(as.numeric(join$ids_complete))
  valuesToImpute <- data.frame(value=complet[which(complet$ids %in% ids_complete),c('height', 'ids')])
  colnames(valuesToImpute) <- c('height','ids')
  merge_data <- merge(x=join, y=valuesToImpute, by.x='ids_complete', by.y='ids')
  newData <- dataset
  for (row in 1:nrow(newData)) {
    if(row %in% merge_data$names)
      newData[which(row.names(newData) %in% row),'height'] <- merge_data[sample(which(merge_data$names %in% row),1),'height']
  }
  
  #test <- which(is.na(newData[,varName]))
  #return(test)
  
  return(newData)
  
  #return(list(x, join, dataset))
}