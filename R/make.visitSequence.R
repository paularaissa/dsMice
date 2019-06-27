#' @title Creates a visit sequence argument
#' @description This helper function creates a valid visit sequence. The 
#' \code{visitSequence} is an argument to the mice function that 
#' specifies the sequence in which blocks are imputed.
#' @inheritParams mice
#' @return Vector containing block names
#' @examples
#' make.visitSequence(nhanes)
#' @export
#' 

make.visitSequence <- function(data = NULL, blocks = NULL) {
  
  if (!is.null(blocks)) {
    blocks <- name.blocks(blocks)
    return(names(blocks))
  }
  
  data <- check.dataform(data)
  blocks <- make.blocks(data)
  names(blocks)
}