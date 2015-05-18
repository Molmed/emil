#' Variable importance of a fitted model
#'
#' Note that different methods calculates variable importance in different
#' ways and that they are not directly comparable.
#'
#' @param model Fitted model.
#' @param ... Sent on to the procedure's variable importance scoring function.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A vector of length p or an p-x-c matrix of variable importance
#'   scores where p is the number of descriptors and c is the number of classes.
#' @examples
#' procedure <- modeling_procedure("pamr")
#' model <- fit(procedure, x=iris[-5], y=iris$Species)
#' importance(model)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}
#' @export
importance <- function(model, ..., .verbose=TRUE){
    model$procedure$importance_fun(model$fit, ...)
}

