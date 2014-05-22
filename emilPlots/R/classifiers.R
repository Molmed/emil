##' Plot decision border
##' 
##' Only implemented for 2-d problems.
##' 
##' @param x Modeling procedure.
##' @param dx Dataset descriptors. Not needed if an already trained model is
##'   supplied.
##' @param dy Response vector for classification.
##' @param model Trained model. If omitted it will be trained from the data.
##' @param n Resolution.
##' @param plot.data Whether or not to plot the data in addition to the border.
##' @param pch Plot characters, see \code{\link{par}}.
##' @param col Color palette, see \code{\link{get.colors.factor}}.
##' @param ... Sent to \code{\link{contour}}.
##' @return Nothing, produces a plot.
##' @examples
##' contour(modeling.procedure("qda"), iris[c(1,3)], iris$Species)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @import emil
##' @export
contour.modeling.procedure <- function(x, dx, dy, model, n=c(100,100), plot.data=!missing(dx), pch, col="Set1", ...){
    if(missing(model)){
        if(missing(dx) || missing(dy))
            stop("You must suppy either a fitted model or data to use for fitting.")
        model <- fit(x, dx, dy)
    }
    if(plot.data){
        if(ncol(dx) != 2)
            stop("Only 2-d problems can be plotted.")
        plot(dx[,1], dx[,2], type="n", xlab = colnames(dx)[1], ylab=colnames(dx)[2],
             bty="n", axes=FALSE)
    }
    u <- par("usr")
    gx <- seq(u[1], u[2], length.out=n[1])
    gy <- seq(u[3], u[4], length.out=n[2])
    grid <- expand.grid(gx, gy)
    if(!missing(dx)){
        colnames(grid) <- colnames(dx)
        if(is.matrix(dx)) grid <- as.matrix(grid)
    }
    pred <- predict(x, model, grid)

    if(plot.data){
        image(gx, gy, matrix(apply(pred$prob, 1, which.max), n[1]), useRaster=TRUE,
              col = get.colors(pred$pred, levels=TRUE, s=-.83, v=1), add=TRUE)
        if(missing(pch)) pch <- as.integer(dy)
        points(dx[,1], dx[,2], col=get.colors(dy), pch=pch)
    }
    class.col <- get.colors(pred$pred, levels=TRUE)
    for(i in seq_along(levels(pred$pred))){
        z <- matrix(pred$prob[,i]-apply(pred$prob[,-i, drop=FALSE], 1, max), n[1])
        contour(gx, gy, z, levels=0, add=TRUE, drawlabels=FALSE, lty=2, col=class.col[i], ...)
    }
    nice.axis(1)
    nice.axis(2)
    nice.box()
}

