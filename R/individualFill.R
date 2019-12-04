#' @export
individualFill <- function(varName, boxes) {
  
  boxes <- as.list(as.numeric(unlist(strsplit(boxes, split = "x"))))
  
  dataColumn <- unlist(getVarByName(varName)[,1])
  countings <- rep(0, length(boxes))
  
  # for (val in dataColumn){
  #   for (i in 1:length(boxes)) {
  #     if (val > boxes[[i]][[1]] & val <= boxes[[i]][[2]]){
  #       countings[i] <- countings[i] + 1
  #       break()
  #     }
  #   }
  # }
  
  return(boxes)
  
}