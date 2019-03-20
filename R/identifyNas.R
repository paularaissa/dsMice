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
  sums <- colSums(is.na(dataset))
  naCols <- names(which(sums!=0))
  completeCols <- names(which(sums==0))
  nas <- is.na(dataset)
  
  data.nas <- dataset[,naCols]
  data.complete <- datasets[,completeCols]
  
  return(list(naCols=naCols, nas=nas, complete=completeCols, data.nas=data.complete))
  
}