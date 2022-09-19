#' @title Suggested methods for ARml
#' @return A character vector of Suggested methods
#' @author Resul Akay
#' @examples
#'
#' suggested_methods()
#'
#' @export
suggested_methods <- function() {
  message("In general user can train any method which supported by caret.
          \nThe following methods are suggested"
  )
  caret_methods <- c("spikeslab", "bagEarth", "bagEarthGCV", "blasso",
                     "cforest", "earth","extraTrees", "gbm_h2o", "glmStepAIC",
                     "parRF", "qrf", "Rborist", "rf", "rqlasso", "rqnc",
                     "spikeslab", "xgbDART", "xgbLinear", "ranger", "cubist",
                     "svmLinear", "enet", "bridge", "glmboost", "ridge",
                     "lasso", "relaxo", "M5Rules", "M5", "lm", "gaussprLinear",
                     "glm", "glmnet", "pcr", "ppr", "foba", "gbm", "svmLinear2",
                     "glm.nb", "gcvEarth", "lars2", "lars", "icr", "ctree2",
                     "ctree", "bayesglm")

  return(caret_methods)
}
