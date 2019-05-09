#' An S4 class to represent a reputation models. It
#' contains (i.e., inherits) everything from the lavaan class.
#'
#' @slot model_type A character string containing the type of model.
#'
#' @seealso \linkS4class{lavaan}
#' @export
rep_model <- setClass("rep_model",
         slots = list(model_type = "character"),
         contains = "lavaan")
