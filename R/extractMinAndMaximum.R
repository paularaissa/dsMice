#'@export
extractMinAndMaximum <- function(obj) {
  recovery.obj <- eval(parse(text=obj))
  
  min <- min(recovery.obj$dif)
  max <- max(recovery.obj$dif)
  
  return(list(min=min, max=max))
  
}