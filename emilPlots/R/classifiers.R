##' Plot decision border
##' 
##' Only implemented for 2-d problems.
##' 
##' @param proc Modeling procedure.
##' @param x Dataset descriptors. Not needed if an already trained model is
##'   supplied.
##' @param y Response vector for classification.
##' @param model Trained model. If omitted it will be trained from the data.
##' @param n Resolution.
##' @param plot.data Whether or not to plot the data in addition to the border.
##' @param col Color palette. Can either be a vector with colors for each class
##'   or the name of a color brewer palette, see \code{name} argument of
##'   \code{\link{brewer.pal}} for a list of possible values.
##' @return Nothing, produces a plot.
##' @examples
##' contour(modeling.procedure("qda"), iris[c(1,3)], iris$Species)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
contour.modeling.procedure <- function(proc, x, y, model, n=c(100,100), plot.data=!missing(x), col="Set1"){
    if(missing(model)){
        if(missing(x) || missing(y))
            stop("You must suppy either a fitted model or data to use for fitting.")
        model <- fit(proc, x, y)
    }
    if(plot.data){
        if(ncol(x) != 2)
            stop("Only 2-d problems can be plotted.")
        plot(x[,1], x[,2], type="n", xlab = colnames(x)[1], ylab=colnames(x)[2],
             bty="n", axes=FALSE)
    }
    u <- par("usr")
    gx <- seq(u[1], u[2], length.out=n[1])
    gy <- seq(u[3], u[4], length.out=n[2])
    grid <- expand.grid(gx, gy)
    if(!missing(x)){
        colnames(grid) <- colnames(x)
        if(is.matrix(x)) grid <- as.matrix(grid)
    }
    pred <- predict(proc, model, grid)

    if(plot.data){
        image(gx, gy, matrix(apply(pred$prob, 1, which.max), n[1]), useRaster=TRUE,
              col = get.class.colors(pred$pred, s=-.83, v=1, levels=TRUE), add=TRUE)
        points(x[,1], x[,2], col=get.class.colors(y), pch=as.integer(y))
    }
    class.col <- get.class.colors(pred$pred, levels=TRUE)
    for(i in seq_along(levels(pred$pred))){
        z <- matrix(pred$prob[,i]-apply(pred$prob[,-i, drop=FALSE], 1, max), n[1])
        contour(gx, gy, z, levels=0, add=TRUE, drawlabels=FALSE, lty=2, col=class.col[i])
    }
    nice.axis(1)
    nice.axis(2)
    nice.box()
}

