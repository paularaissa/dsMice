#'@title Multivariate Imputation by Chained Equations (MICE)
#'
#'@description Generates Multivariate Imputations by Chained Equations (MICE)
#'
#'Generates multiple imputations for incomplete multivariate data by Gibbs
#'sampling. Missing data can occur anywhere in the data. The algorithm imputes
#'an incomplete column (the target column) by generating 'plausible' synthetic
#'values given other columns in the data. Each incomplete column must act as a
#'target column, and has its own specific set of predictors. The default set of
#'predictors for a given target consists of all other columns in the data. For
#'predictors that are incomplete themselves, the most recently generated
#'imputations are used to complete the predictors prior to imputation of the
#'target column.
#'
#'A separate univariate imputation model can be specified for each column. The
#'default imputation method depends on the measurement level of the target
#'column. In addition to these, several other methods are provided. You can
#'also write their own imputation functions, and call these from within the
#'algorithm.
#'
#'The data may contain categorical variables that are used in a regressions on
#'other variables. The algorithm creates dummy variables for the categories of
#'these variables, and imputes these from the corresponding categorical
#'variable. 
#'
#'
#'@param data A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as \code{NA}.
#'@param m Number of multiple imputations. The default is \code{m=5}.
#'@param where A data frame or matrix with logicals of the same dimensions 
#'as \code{data} indicating where in the data the imputations should be 
#'created. The default specifies that the
#'missing data should be imputed. The where argument may be used to 
#'overimpute observed data, or to skip imputations for selected missing values.
#'@param blocks List of vectors with variable names per block. List elements 
#'may be named to identify blocks. Variables within a block are 
#'imputed by a multivariate imputation method
#'(see \code{method} argument). By default each variable is placed 
#'into its own block, which is effectively
#'fully conditional specification (FCS) by univariate models 
#'(variable-by-variable imputation). Only variables whose names appear in 
#'\code{blocks} are imputed. The relevant columns in the \code{where} 
#'matrix are set to \code{FALSE} of variables that are not block members. 
#'A variable may appear in multiple blocks. In that case, it is 
#'effectively re-imputed each time that it is visited.
#'@param method Can be either a single string, or a vector of strings with
#'length \code{length(blocks)}, specifying the imputation method to be
#'used for each column in data. If specified as a single string, the same
#'method will be used for all blocks. The default imputation method (when no
#'argument is specified) depends on the measurement level of the target column,
#'as regulated by the \code{defaultMethod} argument. Columns that need
#'not be imputed have the empty method \code{""}. See details.
#'@param predictorMatrix A numeric matrix of \code{length(blocks)} rows 
#'and \code{ncol(data)} columns, containing 0/1 data specifying 
#'the set of predictors to be used for each target column.
#'Each row corresponds to a variable block, i.e., a set of variables 
#'to be imputed. A value of \code{1} means that the column
#'variable is used as a predictor for the target block (in the rows). 
#'By default, the \code{predictorMatrix} is a square matrix of \code{ncol(data)}
#'rows and columns with all 1's, except for the diagonal. 
#'Note: For two-level imputation models (which have \code{"2l"} in their names)
#'other codes (e.g, \code{2} or \code{-2}) are also allowed.
#'@param visitSequence A vector of block names of arbitrary length, specifying the
#'sequence of blocks that are imputed during one iteration of the Gibbs 
#'sampler. A block is a collection of variables. All variables that are 
#'members of the same block are imputed 
#'when the block is visited. A variable that is a member of multiple blocks 
#'is re-imputed within the same iteration. 
#'The default \code{visitSequence = "roman"} visits the blocks (left to right)
#'in the order in which they appear in \code{blocks}. 
#'One may also use one of the following keywords: \code{"arabic"} 
#'(right to left), \code{"monotone"} (ordered low to high proportion 
#'of missing data) and \code{"revmonotone"} (reverse of monotone). 
#'@param formulas A named list of formula's, or expressions that
#'can be converted into formula's by \code{as.formula}. List elements
#'correspond to blocks. The block to which the list element applies is 
#'identified by its name, so list names must correspond to block names.
#'The \code{formulas} argument is an alternative to the 
#'\code{predictorMatrix} argument that allows for more flexibility in 
#'specifying imputation models, e.g., for specifying interaction terms. 
#'@param blots A named \code{list} of \code{alist}'s that can be used 
#'to pass down arguments to lower level imputation function. The entries
#'of element \code{blots[[blockname]]} are passed down to the function
#'called for block \code{blockname}.
#'@param post A vector of strings with length \code{ncol(data)} specifying
#'expressions as strings. Each string is parsed and 
#'executed within the \code{sampler()} function to post-process 
#'imputed values during the iterations. 
#'The default is a vector of empty strings, indicating no post-processing.
#'@param defaultMethod A vector of length 4 containing the default
#'imputation methods for 1) numeric data, 2) factor data with 2 levels, 3) 
#'factor data with > 2 unordered levels, and 4) factor data with > 2 
#'ordered levels. By default, the method uses 
#'\code{pmm}, predictive mean matching (numeric data) \code{logreg}, logistic
#'regression imputation (binary data, factor with 2 levels) \code{polyreg},
#'polytomous regression imputation for unordered categorical data (factor > 2
#'levels) \code{polr}, proportional odds model for (ordered, > 2 levels).
#'@param maxit A scalar giving the number of iterations. The default is 5.
#'@param printFlag If \code{TRUE}, mice will print history on console.
#'Use \code{print=FALSE} for silent computation.
#'@param seed An integer that is used as argument by the \code{set.seed()} for
#'offsetting the random number generator. Default is to leave the random number
#'generator alone.
#'@param data.init A data frame of the same size and type as \code{data},
#'without missing data, used to initialize imputations before the start of the
#'iterative process.  The default \code{NULL} implies that starting imputation
#'are created by a simple random draw from the data. Note that specification of
#'\code{data.init} will start all \code{m} Gibbs sampling streams from the same
#'imputation.
#'@param ... Named arguments that are passed down to the univariate imputation
#'functions.
#'
#'@return Returns an S3 object of class mids}
#'        (multiply imputed data set)
#'@author Stef van Buuren \email{stef.vanbuuren@@tno.nl}, Karin
#'Groothuis-Oudshoorn \email{c.g.m.oudshoorn@@utwente.nl}, 2000-2010, with
#'contributions of Alexander Robitzsch, Gerko Vink, Shahab Jolani, 
#'Roel de Jong, Jason Turner, Lisa Doove, 
#'John Fox, Frank E. Harrell, and Peter Malewski.
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). 
#'
#'Van Buuren, S. (2018). 
#'\href{https://stefvanbuuren.name/fimd/sec-FCS.html#sec:MICE}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#'Chapman & Hall/CRC. Boca Raton, FL.
#'
#'Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#'Fully conditional specification in multivariate imputation.  \emph{Journal of
#'Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'
#'Van Buuren, S. (2007) Multiple imputation of discrete and continuous data by
#'fully conditional specification.  \emph{Statistical Methods in Medical
#'Research}, \bold{16}, 3, 219--242.
#'
#'Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple imputation of
#'missing blood pressure covariates in survival analysis.  \emph{Statistics in
#'Medicine}, \bold{18}, 681--694.
#'
#'Brand, J.P.L. (1999) \emph{Development, implementation and evaluation of
#'multiple imputation strategies for the statistical analysis of incomplete
#'data sets.} Dissertation. Rotterdam: Erasmus University.
#'@keywords iteration
#'@export


miceDS <- function(vars=NULL, m = 5, 
                    method = NULL,
                    predictorMatrix = NULL,
                    where = NULL,
                    blocks = NULL,
                    visitSequence = NULL,
                    formulas = NULL,
                    blots = NULL,
                    post = NULL,
                    defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                    maxit = 5, printFlag = TRUE, seed = NA,
                    data.init = NULL) {
  
  #call <- match.call()
  #check.deprecated(...)
  if (!is.na(seed)) set.seed(seed)
  
  # check form of data and m
  
  if(!is.null(vars)){
    data <- getVarByName(vars)
  } else {
    data <- eval(parse(text="D")) 
  }
  
  data <- check.dataform(data)
  m <- check.m(m)

  # determine input combination: predictorMatrix, blocks, formulas
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)
  
  # case A
  if (mp & mb & mf) {
    # blocks lead
    blocks <- make.blocksDS(colnames(data))
    predictorMatrix <- make.predictorMatrixDS(data, blocks)
  }
  
  return(list(blocks=blocks, predictorMatrix=predictorMatrix))
}