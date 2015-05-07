#' Variable importance of a fitted model
#'
#' Note that different methods calculates variable importance in different
#' ways and that they are not directly comparable.
#'
#' @param procedure Modeling procedure.
#' @param model Fitted model.
#' @param ... Sent on to the procedure's variable importance scoring function.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A vector of length p or an p-x-c matrix of variable importance
#'   scores where p is the number of descriptors and c is the number of classes.
#' @examples
#' procedure <- modeling_procedure("pamr")
#' model <- fit(procedure, x=iris[-5], y=iris$Species)
#' importance(procedure, model)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}
#' @export
importance <- function(procedure, model, ..., .verbose=TRUE){
    procedure$importance_fun(model, ...)
}


#' Tidy up variable importance estimates
#' 
#' Only intended for internal use.
#' 
#' @param x Variable importance estimates.
#' @param feat A logical vector indicating which features passed feature
#'   selection in the pre-processing.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
fixvimp <- function(x, feat){
    if(is.null(feat)){
        return(x)
    } else if(is.matrix(x)){
        y <- matrix(NA, length(feat), ncol(x))
        y[feat,] <- x
    } else if(is.vector(x)){
        y <- rep(NA, length(feat))
        y[feat] <- x
    } else {
        stop("Invalid variable importance format.")
    }
    y
}

