#' @export
individualFill2 <- function(varName, obj, boxes) {
  
  boxes <- jsonlite::fromJSON(boxes, simplifyMatrix = FALSE)
  
  recovery.obj <- eval(parse(text=obj))
  
  dataColumn <- unlist(recovery.obj$dif)
  
  countings <- rep(0, length(boxes))
  
  for (val in dataColumn){
    for (i in 1:length(boxes)) {
      if (val > boxes[[i]][1] & val <= boxes[[i]][2]){
        countings[i] <- countings[i] + 1
        break()
      }
    }
  }
  
  return(countings)
  
}