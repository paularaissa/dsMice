#'@export
extractMinAndMaximum <- function(obj) {
  test.obj <- eval(parse(text=obj))
  return(test.obj)
  
}