#' Creates a \code{formulas} argument
#'
#' This helper function creates a valid \code{formulas} object. The 
#' \code{formulas} object is an argument to the \code{mice} function. 
#' It is a list of formula's that specifies the target variables and 
#' the predictors by means of the standard \code{~} operator.
#' @param data A \code{data.frame} with the source data
#' @param blocks An optional specification for blocks of variables in 
#' the rows. The default assigns each variable in its own block.
#' @param predictorMatrix A \code{predictorMatrix} specified by the user.
#' @return A list of formula's.
#' @seealso \code{\link{make.blocks}}, \code{\link{make.predictorMatrix}}
#' @examples
#' f1 <- make.formulas(nhanes)
#' f1
#' f2 <- make.formulas(nhanes, blocks = make.blocks(nhanes, "collect"))
#' f2
#' 
#' # for editing, it may be easier to work with the character vector
#' c1 <- as.character(f1)
#' c1
#' 
#' # fold it back into a formula list
#' f3 <- name.formulas(lapply(c1, as.formula))
#' f3
#' 
#' @export
#' 
make.formulasDS <- function(data, blocks = make.blocks(data), 
                          predictorMatrix = NULL) {
  data <- check.dataform(data)
  formulas <- as.list(rep("~ 0", length(blocks)))
  names(formulas) <- names(blocks)
  
  for (h in names(blocks)) {
    y <- blocks[[h]]
    if (is.null(predictorMatrix)) {
      predictors <- colnames(data)
    } else {
      type <- predictorMatrix[h, ]
      predictors <- names(type)[type != 0]
    }
    x <- setdiff(predictors, y)
    formulas[[h]] <- paste(paste(y, collapse = "+"), "~", 
                           paste(c("0", x), collapse = "+"))
  }
  
  formulas <- lapply(formulas, as.formula)
  formulas
}