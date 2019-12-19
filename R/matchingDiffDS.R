#'@export
matchingDiffDS <- function(obj, rank) {
  
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
  
  return(join)
}