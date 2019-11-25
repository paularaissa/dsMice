#' @export
individualFill <- function(varName, boxes) {
  
  boxes <- as.numeric(unlist(strsplit(boxes, split = "x")))
  
  dataColumn <- getVarByName(varName)
  countings <- rep(0, length(boxes))
  
  # for (val in dataColumn){
  #   for (i in 1:length(boxes)) {
  #     if (val > boxes[[i]][1] & val <= boxes[[i]][2]){
  #       countings[i] <- countings[i] + 1
  #       break()
  #     }
  #   }
  # }
  
  return(boxes)
  
}