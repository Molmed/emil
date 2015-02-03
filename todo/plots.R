#' Plot the decision border of a 2-d classifier
#' 
#' @param fit Fitted classifier.
#' @param x A dataset of the kind that was used for classifier design.
#' @param n Resolution.
#' @param ... Sent to \code{\link{contour}}.
#' @return Nothing, produces a plot.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso fit
#' @export
decision.border <- function(fit, x, n=c(100, 100), ...){
    if(ncol(x) != 2){
        stop("The decision border can only be plotted for 2-d classification problems.")
    }
    u <- par("usr")
    xi <- seq(u[1], u[2], length.out=n[1])
    yi <- seq(u[3], u[4], length.out=n[2])
    g <- expand.grid(xi, yi)
    colnames(g) <- colnames(x)
    if(is.matrix(x)) g <- as.matrix(g)
    p <- predict(fit, g)
    z <- matrix(p$prob[,2], n[1])
    contour(xi, yi, z, add=TRUE, ...)
}

