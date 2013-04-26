##' Variable importance of a fitted classifier.
##'
##' \code{vimp} is a generic function for extracting variable importance from
##' fitted prediction models. The function invokes particular _methods_ which depend
##' on the 'class' of the first argument. Note that different _method_ calculates
##' variable importance in different ways and that they are not directly comparable.
##'
##' @param object Fitted model.
##' @param ... Sent on to class method.
##' @return Variable importance vector.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
vimp <- function(object, ...){
    # if(!any(sapply(sprintf("vimp.%s", class(object)), exists)))
    #     stop(sprintf("No variable importance measure is implemented for classifier type \"%s\".", class(object$fit)[1]))
    UseMethod("vimp", object)
}

