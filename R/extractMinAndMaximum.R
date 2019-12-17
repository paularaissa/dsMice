#'@export
extractMinAndMaximum <- function(obj) {
  recovery.obj <- eval(parse(text=obj))
  
  min <- min(recovery.obj$dif)
  max <- max(recovery.obj$dif)
  len <- nrow(recovery.obj)
  
  return(list(min=min, max=max, len=len))
  
}