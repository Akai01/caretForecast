#' @title Suggested methods for ARml
#' @return A character vector of Suggested methods
#' @author Resul Akay
#' @examples
#' \dontrun{
#' suggested_methods()
#' }
#' @export

suggested_methods <- function(){
  message(
    "In general user can train any method which supported by caret.
The following methods are suggested")
  caret_methods <- c("cubist", "svmLinear", "enet", "bridge", "glmboost",
                     "ridge", "lasso", "relaxo", "M5Rules", "M5", "lm",
                     "gaussprLinear", "glm", "glmnet", "pcr", "ppr", "foba",
                     "gbm", "svmLinear2", "glm.nb", "gcvEarth", "lars2", "lars",
                     "icr", "ctree2", "ctree", "bayesglm")

  return(caret_methods)
}
