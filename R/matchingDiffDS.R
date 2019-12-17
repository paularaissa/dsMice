#'@export
matchingDiffDS <- function(obj, rank) {
  
  ranking <- jsonlite::fromJSON(rank, simplifyMatrix = FALSE) 
  recovery.obj <- eval(parse(text=obj))
  
  
  
  return(recovery.obj)
}