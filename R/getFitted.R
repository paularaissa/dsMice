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
  beta.reg.aux <- as.numeric(unlist(strsplit(beta, split="x")))
  beta.reg <- data.matrix(beta.reg.aux)
  
  #Retrive the values and variables x
  bindxy <- dsMice::getVarbyFormula(formula)
  # 
  #Formula to calculate the fitted values
  estimated <- 0
  teste <- c()
  teste2 <- c()
  
  missPosition <- data.matrix(which(is.na(bindxy[,1])))
  for (i in 1:nrow(missPosition)) {
    pos <- missPosition[i]
    teste[i] <- bindxy[pos,2]
    teste2[i] <- bindxy[pos,3]
  }
  
  xMiss <- data.frame(teste, teste2)
  xMiss <- as.matrix(xMiss)
  estimated <- xMiss %*% as.vector(beta.reg[-1])
  estimated <- data.frame(estimated, missPosition)
  
  vars <- all.vars(as.formula(formula))
  histogram <- dsMice::getHistogram(paste0("D$", vars[2]))
  
  return(histogram)
  
}
