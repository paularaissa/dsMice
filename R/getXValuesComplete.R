#'
#' @title Partial Computation for Model Fitted Values
#' @description Calculates the fitted values in each data node.
#' @details Considering \code{y} as a response variable and x as study variable, the fitted values are the y-values that
#' would expect for the given x-values according to the best-fitting straight line.
#'
#' @param beta is a list of the regression coefficients.
#' @param formula a string character to be transformed as an object of class \code{formula}.
#'
#' @return a vector of fitted values.
#'
#' @section Dependencies:
#' \code{\link{getVarbyFormula}}
#'
#' @author Paula R. Costa e Silva
#' @export
#'

getXValuesComplete <- function(formula, idValuesList) {
  
  #Format variables
  vars <- all.vars(formula)
  vars <- vars[-1]
  xColNames <- vars[-1]
  yColNames <- vars[1]
  
  #Data transformations
  idValues <- as.numeric(unlist(strsplit(idValuesList, split="x")))
  #beta.reg <- data.matrix(beta.reg.aux)
  
  bindxy <- eval(parse(text="D"))
  bindxy <- bindxy[,vars]
   
  row.sums <- rowSums(is.na(bindxy))
  naLines <- names(which(row.sums!=0))
  #Select subset of complete data
  xValuesComplete <- bindxy[-which(rownames(bindxy) %in% naLines), ]
  
  imputedValues <- xValuesComplete[which(rownames(xValuesComplete) %in% idValues),]
  #imputedValues <- xValuesComplete[which(idValues %in% rownames(ValuesComplete))),]
  imputedValuesDF <- as.data.frame(imputedValues[,1])
  rownames(imputedValuesDF) <- rownames(imputedValues)
  colnames(imputedValuesDF) <- "imputedValues"

  # imputedValues <- c()
  # 
  # for (idValor in idValues) {
  #   randomValue <- xValuesComplete[idValor, 1]
  #   imputedValues[cont] <- randomValue
  # }

  # imputedValues <- as.data.frame(imputedValues)
  # rownames(imputedValues) <- naLines

  return(imputedValuesDF)
   
}