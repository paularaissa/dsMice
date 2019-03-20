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
  col.sums <- colSums(is.na(dataset))
  row.sums <- rowSums(is.na(dataset))
  
  na.rows <- names(which(row.sums!=0))
  
  naCols <- names(which(col.sums!=0))
  completeCols <- names(which(col.sums==0))
  nas <- is.na(dataset)
  
  data.nas <- as.data.frame(dataset[,naCols])
  data.complete <- as.data.frame(dataset[,completeCols])
  colnames(data.complete) <- completeCols
  rownames(data.complete) <- rownames(dataset)
  
  return(list(naCols=naCols, nas=nas, complete=completeCols, data.nas=data.nas, data.complete=data.complete, na.rows=na.rows))
  
}