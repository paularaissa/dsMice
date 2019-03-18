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

identifyNas <- function() {
  
  dataset <- eval(parse(text="D"))
  #sums <- colSums(is.na(data))
  #naCols <- names(which(sums!=0))
  
  return(dataset)
  
}