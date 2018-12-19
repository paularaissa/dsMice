#'
#' @title Partial Computation for Model Fitted Values
#' @description Calculates the fitted values in each data node.
#' @details Considering \code{y} as a response variable and x as study variable, the fitted values are the y-values that
#' would expect for the given x-values according to the best-fitting straight line.
#'
#' @param beta is a list of the regression coefficients.
#' @param formula a string character to be transformed as an object of class \code{formula}.
#' @param x is a study variable.
#'
#' @return a vector of fitted values.
#'
#' @section Dependencies:
#' \code{\link{getVarbyFormula}}
#'
#' @author Paula R. Costa e Silva
#' @export
#'

getFitted <- function(beta, formula) {
  
  #Data transformations
  if(class(beta)=="character"){
    beta.reg.aux <- as.numeric(unlist(strsplit(beta, split="x")))
    beta.reg <- data.matrix(beta.reg.aux)
  } else {
    beta.reg <- beta
  }
  
  #Retrive the values and variables x
  bindxy <- dsMice::getVarbyFormula(formula)
  # bind.x <- data.matrix(bindxy$x)
  # bind.y <- data.matrix(bindxy$y)
  # 
  #Formula to calculate the fitted values
  estimated <- 0
  teste <- list()

  missPosition <- data.matrix(which(is.na(bindxy[,1])))
  for (pos in missPosition) {
  #   estimated <- bind.x[pos,1]
      teste[pos] <- pos
  }
  
  # for(i in 1:nrow(bind.y)) {
  #   if(is.na(bind.y[i])) {
  #     estimated <- bind.x[i,] %*% beta.reg
  #     #teste <- rbind(teste, estimated)
  #   }
  # }
  # y.hat <- bind.x %*% beta.reg
  # row.names(y.hat) <- rowNames
  
  return(teste)
  
}
