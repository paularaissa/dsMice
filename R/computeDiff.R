#' @export

computeDiff <- function(yHatMissing, varName) {
  
  if(is.character(varName)){
    x <- eval(parse(text=varName))
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }
  estimatedMissing <- jsonlite::fromJSON(yHatMissing, simplifyMatrix = FALSE)
  
  #completeValues <- !is.na(x)
  
  return(list(x, estimatedMissing))
  
}