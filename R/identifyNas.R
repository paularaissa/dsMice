#'
#' @title Identify missing values
#' @description ...
#' @details ...
#'
#' @return the variables names with missing values.
#'
#'
#' @author Paula Silva, Rui Camacho
#' @export
#

identifyNas <- function(vars=NULL) {
  
  if(is.null(vars)) {
    dataset <- eval(parse(text="D"))
  } else {
    dataset <- getVarByName(vars)
  }
  
  # Filter only numeric data
  dataset <- Filter(is.numeric, dataset)
  
  #indice dos registros com variaveis completas
  # listNasbyCol <- NULL
  # for (idxCol in 1:ncol(dataset)) {
  #   row.sums <- rowSums(is.na(dataset[,idxCol]))
  #   na.rows <- names(which(row.sums!=0))
  #   colName <- colnames(nas$mice2$data.nas[1])
  #   listNasbyCol[colName] <- na.rows
  # }
 # naLines <- subset(dataset, is.na(dataset[,1]))
  
  col.sums <- colSums(is.na(dataset))
  naCols <- names(which(col.sums!=0))
  completeCols <- names(which(col.sums==0))
  nas <- is.na(dataset)
  
  data.nas <- as.data.frame(dataset[,naCols])
  data.complete <- as.data.frame(dataset[,completeCols])
  colnames(data.complete) <- completeCols
  rownames(data.complete) <- rownames(dataset)
  
  return(list(naCols=naCols, nas=nas, completeCols=completeCols, data.nas=data.nas, data.complete=data.complete))
  
}