#' Extends formula's with predictor matrix settings
#' 
#' @inheritParams mice
#' @return A list of formula's
#' @param auxiliary A logical that indicates whether the variables
#' listed in \code{predictors} should be added to the formula as main 
#' effects. The default is \code{TRUE}.
#' @param include.intercept A logical that indicated whether the intercept 
#' should be included in the result.
#' @keywords internal
extend.formulas <- function(formulas, data, blocks, predictorMatrix = NULL, 
                            auxiliary = TRUE,
                            include.intercept = FALSE, 
                            ...) {
  # Extend formulas with predictorMatrix
  if (is.null(predictorMatrix)) return(formulas)
  for (h in names(blocks)) {
    type <- predictorMatrix[h, ]
    predictors <- names(type)[type != 0]
    ff <- extend.formula(formula = formulas[[h]], 
                         predictors = predictors, 
                         auxiliary = auxiliary, 
                         include.intercept = include.intercept)
    formulas[[h]] <- ff
  }
  formulas
}
