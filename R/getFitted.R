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
  bindxy <- dsMice::getNa(formula)
  
  bindxy <- getVarbyFormula(formula)
  bindxy$ID <- seq.int(nrow(bindxy))
  naLines <- subset(bindxy, is.na(bindxy[,1]))
  
  #Format variables
  vars <- all.vars(formula)
  formatedVars <- paste0(vars[1], "$", vars[2:length(vars)])
  
  #Select subset of missing data
  xValuesMiss <- subset(x=naLines, select=formatedVars[-1])
  
  #Select subset of xValues
  xValues <- unique(subset(x=bindxy, select=c(formatedVars[-1], 'ID')))
  xValues <- xValues[!is.na(xValues)] 
  
  #Formula to compute the estimated values
  xMiss <- as.matrix(xValuesMiss)
  estimated <- xMiss %*% as.vector(beta.reg[-1])
    
  # #Difference between estimates and real values
  cont <- 1
  dfDif <- list()
  # for (value in estimated) {
  #   subtract <- mapply('-', value, xValues)
  #   dfDif <- dif[order(dif)[1:5]]
  #   #difList[[cont]]
  #   cont <- cont + 1
  # }
  
  # estimated <- data.frame(estimated, missPosition)
  # 
  # vars <- all.vars(as.formula(formula))
  # histogram <- dsMice::getHistogram(paste0("D$", vars[2]))
  
  return(xValues)
  
}
