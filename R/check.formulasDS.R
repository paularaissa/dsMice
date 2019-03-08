#' Name formula list elements
#'
#' This helper function names any unnamed elements in the \code{formula} 
#' list. This is a convenience function.
#' @inheritParams mice
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return Named list of formulas
#' @seealso \code{\link{mice}}
#' @details 
#' This function will name any unnamed list elements specified in 
#' the optional argument \code{formula}. Unnamed formula's 
#' consisting with just one response variable will be named 
#' after this variable. Unnamed formula's containing more 
#' than one variable will be named by the \code{prefix} 
#' argument, padded by an integer sequence stating at 1.
#' @examples
#' # fully conditionally specified main effects model
#' form1 <- list(bmi ~ age + chl + hyp, 
#'               hyp ~ age + bmi + chl,
#'               chl ~ age + bmi + hyp)
#' form1 <- name.formulas(form1)
#' imp1 <- mice(nhanes, formulas = form1, print = FALSE, m = 1, seed = 12199)
#' 
#' # same model using dot notation
#' form2 <- list(bmi ~ ., hyp ~ ., chl ~ .)
#' form2 <- name.formulas(form2)
#' imp2 <- mice(nhanes, formulas = form2, print = FALSE, m = 1, seed = 12199)
#' identical(complete(imp1), complete(imp2))
#' 
#' # same model using repeated multivariate imputation
#' form3 <- name.blocks(list(all = bmi + hyp + chl ~ .))
#' imp3 <- mice(nhanes, formulas = form3, print = FALSE, m = 1, seed = 12199)
#' cmp3 <- complete(imp3)
#' identical(complete(imp1), complete(imp3))
#' 
#' # same model using predictorMatrix
#' imp4 <- mice(nhanes, print = FALSE, m = 1, seed = 12199, auxiliary = TRUE)
#' identical(complete(imp1), complete(imp4))
#' 
#' # different model: multivariate imputation for chl and bmi
#' form5 <- list(chl + bmi ~ ., hyp ~ bmi + age)
#' form5 <- name.formulas(form5)
#' imp5 <- mice(nhanes, formulas = form5, print = FALSE, m = 1, seed = 71712)
#' @export
#' 
name.formulasDS <- function(formulas, prefix = "F") {
  if (!is.list(formulas))
    stop("Argument `formulas` not a list", call. = FALSE)
  if (!all(sapply(formulas, is.formula) | sapply(formulas, is.list)))
    stop("Not all elements in `formulas` are a formula or a list")
  if (is.null(names(formulas))) names(formulas) <- rep("", length(formulas))
  inc <- 1
  for (i in seq_along(formulas)) {
    if (names(formulas)[i] != "") next
    #if (hasdot(formulas[[i]]) && is.null(data)) 
    #  stop("Formula with dot requires `data` argument", call. = FALSE)
    y <- lhs(formulas[[i]])
    if (length(y) == 1) names(formulas)[i] <- y
    else {
      names(formulas)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  formulas
}


check.formulasDS <- function(formulas, data) {
  formulas <- name.formulas(formulas)
  formulas <- handle.oldstyle.formulas(formulas, data)
  formulas <- lapply(formulas, expand.dots, data)
  # escape if formula is list of two formula's
  if (any(sapply(formulas, is.list))) return(formulas)
  formulas <- lapply(formulas, as.formula)
  formulas
}