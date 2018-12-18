#'
#' @title Access Data from Study and Response Variables by \code{formula}.
#' @description Data access and transformations for dependent and independent variables in regressions and model checking analysis.
#' @details Internal function.
#' Commonly used by methods relationed to regression and model checking analysis.
#' @param formula a string character to be transformed as an object of class \code{\link[stats]{formula}}.
#' @param subset optional vector which specifies the subset of observations to be used in the analysis.
#' @param weight a list or numerical value for weigthed analysis.
#' @param family a string character with the name of the error distribution and link function to be used in the analysis.
#' If \code{family} is set to 'binomial', it transforms the vector of observed responses \code{y.var}
#' into \code{\link[base]{factors}}.
#' If \code{family} is NULL, it does not transform the data.
#'
#' @return A list with components:
#' \item{bind.x}{a data matrix with the independent variables and their respective values.}
#' \item{bind.y}{a data matrix, with the response variable and its respective values.}
#' @author Paula R. Costa e Silva
#' @export
#'

getVarbyFormula <- function(formula, subset=NULL, weight=NULL, family=NULL) {

  model.formula <- as.formula(formula)
  model <- model.frame(formula = model.formula, na.action = na.pass)

  if (is.null(weight)) x.vars <- model[-1] * weight

  bind.x <- data.matrix(cbind(1, x.vars))

  bind.y <- model[1]

  return(list(x=bind.x, y=bind.y))

}
