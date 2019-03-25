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

getImpute <- function(beta, formula) {
  
  #Format variables
  vars <- all.vars(formula)
  ##formatedVars <- paste0(vars[1], "$", vars[2:length(vars)])
  vars <- vars[-1]
  xColNames <- vars[-1]
  yColNames <- vars[1]
  
  #Data transformations
  beta.reg.aux <- as.numeric(unlist(strsplit(beta, split="x")))
  beta.reg <- data.matrix(beta.reg.aux)
  
  #Retrive the values and variables x
  #bindxy <- dsMice::getNa(formula)
  
  #bindxy <- getVarbyFormula(formula)
  #bindxy$ID <- seq.int(nrow(bindxy))
  bindxy <- eval(parse(text="D"))
  bindxy <- bindxy[,vars]
  
  row.sums <- rowSums(is.na(bindxy))
  naLines <- names(which(row.sums!=0))
  #Select subset of missing data
  xValuesMiss <- bindxy[which(rownames(bindxy) %in% naLines), ]
  #Select subset of complete data
  xValuesComplete <- bindxy[-which(rownames(bindxy) %in% naLines), ]
  
  # #Select subset of xValues
  xValues <- as.data.frame(unique(bindxy[,xColNames]))
  colnames(xValues) <- xColNames

  # #Formula to compute the estimated values
  xMiss <- as.matrix(xValuesMiss[-1]) #x values where y is missing
  xComplete <- as.matrix(xValuesComplete)
  #estimated <- beta.reg[1] + xMiss %*% as.vector(beta.reg[-1])
  # 
  yHatMis <- beta.reg[1] + xMiss %*% as.vector(beta.reg[-1])
  # yHatObs <- xComplete %*% as.vector(beta.reg)
  
  return(yHatMis)
  
  # # #   
  # # # #Difference between estimates and real values
  # cont <- 1
  # imputedValues <- c()
  # valor <- NULL
  # for (value in estimated) {
  #     subtract <- data.frame(mapply('-', value, xValues)) #same x values rownames
  #     colnames(subtract) <- "dif"
  #     rownames(subtract) <- rownames(xValues)
  #     top5 <- subtract[order(subtract$dif)[1:5],]
  #    # top5 <- na.exclude(top5)
  #     # randomValue <- sample(top5, 1)
  #     # matching <- match(randomValue, subtract$dif)
  #     # names <- rownames(subtract) #search the corresponding rowname
  #     # idValor <- names[matching]
  #     # valor <- xValuesComplete[idValor, 1]
  # #    #valor <- bindxy[vars[1]]
  #     # imputedValues[cont] <- valor
  # #    # cont <- cont + 1
  #     return(subtract)
  #  }
  # 
  # imputedValues <- as.data.frame(imputedValues)
  # rownames(imputedValues) <- naLines

  ##NAP USAR ESSA PARTE
  # estimated <- data.frame(estimated, missPosition)
  #
  # vars <- all.vars(as.formula(formula))
  # histogram <- dsMice::getHistogram(paste0("D$", vars[2]))

  #return(valor)
  
}
