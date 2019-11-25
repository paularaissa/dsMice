#' @export
individualFill <- function(varName, boxes) {
  
  dataColumn <- getVarByName(varName)
  countings <- rep(0, length(boxes))
  
  for (val in dataColumn){
    for (i in 1:length(boxes)) {
      if (float(val) > boxes[[i]][1] & float(val) <= boxes[[i]][2]){
        countings[i] <- countings[i] + 1
        break()
      }
    }
  }
  
  return(countings)
  
}