##' @import lattice 
{}

##' An overview of plot functions
##' 
##' \tabular{ll}{
##'   \code{\link{confusionplot}} \tab Plots the confusion matrices of a batch
##'     of fitted classifiers.\cr
##'   \code{\link{contour.classifier}} \tab Plots the decision boundary in 2-d
##'     for a fitted classifier.\cr
##'   \code{\link{rocplot}} \tab Plots ROC-curves of predictions form a set of
##'     classifiers fitted with \code{\link{batch.design}}.\cr
##'   \code{\link{vimpplot}} \tab Plots variable importance of batch of fitted
##'     classifiers.\cr
##' }
##' @name plots
{}

##' Plot classification border
##' 
##' The awkward naming of the arguments is to keep S3 consistency. A more
##' conventional definition using model formulas might replace it in the future.
##'
##' @method contour classifier
##' @param x The fitted classifier.
##' @param data Dataset to display together with border (optional).
##' @param y Class labels for \code{data} (optional).
##' @param xlim Limits of x-axis, numeric vector of length 2. Calculated
##'   automatically if \code{data} is supplied.
##' @param ylim Limits of y-axis, works just like \code{xlim}.
##' @param subset Subset of data to plot.
##' @param res Resolution, numeric vector of length 3, corresponding to x,y,z where z
##'   is the number of colors in the palette.
##' @param thres Thresholds to plot.
##' @param minor.thres Additional thresholds to plot in a discrete style.
##' @param ... Sent to \code{\link{contour}}.
##' @return Nothing
##' @example examples/contour.classifier.R
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso rocplot, design
##' @export
contour.classifier <- function(x, data, y, subset=TRUE, xlim, ylim, res=rep(101, 3), thres=.5, minor.thres=NULL, ...){
    if(length(x$descriptors) != 2) stop("Only 2d data can be plotted.")
    if(!identical(subset, TRUE)){
        data <- data[subset,]
        y <- y[subset]
    }

    if(missing(xlim) && !missing(data)){
        xlim <- range(data[,1], na.rm=TRUE)
        xlim <- c(1.1*xlim[1] - .1*xlim[2], 1.1*xlim[2] - .1*xlim[1])
    }
    if(missing(ylim) && !missing(data)){
        ylim <- range(data[,2], na.rm=TRUE)
        ylim <- c(1.1*ylim[1] - .1*ylim[2], 1.1*ylim[2] - .1*ylim[1])
    }
    xi <- seq(xlim[1], xlim[2], length=res[1])
    xj <- seq(ylim[1], ylim[2], length=res[2])
    di <- xi[2] - xi[1]
    dj <- xj[2] - xj[1]
    zi <- expand.grid(xi, xj)
    names(zi) <- x$descriptors
    z <- matrix(predict(x, zi)$prob[,1], length(xi), length(xj))
    pal <- c(hsv(2/3, seq(.25, 0, length=ceiling(res[3]/2))),
             hsv(1/6, seq(0, .4, length=floor(res[3]/2))[-1], 1))
    image(c(xi[1]-di/2, xi+di/2), c(xj[1]-dj/2, xj+dj/2), z,
          zlim = c(-1, 1)*max(abs(z-.5))+.5, col=pal,
          main = "Decision boundary",
          xlab = x$descriptors[1],
          ylab = x$descriptors[2],
          sub = if(all((z[1] < .5) == (z < .5))) sprintf("All class %s", x$responses[1+(z[1] >= .5)]) else NULL,
          useRaster=TRUE)
    contour(xi, xj, z, add=TRUE, levels=thres, ...)
    if(!is.blank(minor.thres)){
        contour(xi, xj, z, add=TRUE, levels=minor.thres, col="#6666bb", lty=3, ...)
    }
    if(!missing(data)){
        points(data[,1], data[,2],
               pch = if(missing(y)) 1 else c(1,3)[y],
               col = if(missing(y)) "black" else c("#666600", "#000066")[y])
    }
}


##' Plot empirical ROC curve
##' 
##' @param pred Prediction object with roc measures calculated, as returned by
##'   \code{\link{roc.measures}}
##' @param class Which class to draw ROC curve for.
##' @param main Plot title.
##' @param key Custom key (optional), e.g. created by \code{link{simpleKey}}.
##' @param col Custom line colors (optional).
##' @param lty Line type, see \code{link{par}}.
##' @param lwd Line width, see \code{link{par}}.
##' @param ... Sent to \code{\link{xyplot}}.
##' @return Nothing
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso roc.measures, predict
##' @export
rocplot <- function(pred, class=2, main="Empirical ROC curve", key, col, lty=1, lwd=1, ...){
    if("pred" %in% names(pred)){
        # Only one classifier inputed
        pred <- list(pred)
    }
    while(!"pred" %in% names(pred[[1]])){
        if(!is.list(pred)) stop("Malformed")
        pred <- unlist(pred, recursive=FALSE)
    }
    plot.data <- foreach(i=1:length(pred), pr=pred, .combine=rbind) %do% {
        if(class==1) pr$conf <- rev(pr$conf)
        with(pr, data.frame(id=i,
            fp = conf[[4]]/(conf[[2]]+conf[[4]]),
            tp = conf[[5]]/(conf[[3]]+conf[[5]])))
    }
    if(is.blank(names(pred))){
        if(missing(key)) key <- NULL
        col <- 1
    } else {
        if(missing(key)){
            key <- simpleKey(sapply(unique(names(pred)), function(my.type){
                sprintf("%s, %sAUC = %.3g",
                    toupper(my.type),
                    if(sum(names(pred) == my.type) > 1) "mean " else "",
                    mean(sapply(pred[names(pred) == my.type], function(x) x$auc)))
                }), points=FALSE, lines=TRUE)
        }
        if(missing(col)){
            col <- key$lines$col[(1:length(pred)-1) %% length(key$lines$col) + 1]
        }
    }
    xyplot(tp ~ fp, plot.data, groups=id, type="l", 
        key = key,
        #col = c("#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff", "#ff00ff"),
        col  = col,
        main = main,
        sub  = sprintf("Class: %s", levels(pred[[1]]$pred)[class]),
        xlab = "False positive rate (false alarm)",
        ylab = "True positive rate (probability of detection)",
        panel = function(...){
            panel.xyplot(0:1, 0:1, type="l", lty=2, col="#000000")
            panel.xyplot(...)
        },
        lty=lty,
        lwd=lwd,
        ...)
}


##' Plot variable importance of a batch
##' 
##' @param batch Classification batch with variable importance as returned by
##'   \code{\link{batch.design}}.
##' @param style Plot type. One of the following or a uniquely identifiable
##'   abbreviation thereof:
##'   \tabular{ll}{
##'     "bw" \tab Box and whisker.\cr
##'     "violin" \tab Violin plot. Similar to "bw" but with density estimates instead
##'                   of quantiles.\cr
##'     "strip" \tab Strip plot.\cr
##'     "mean" \tab Bar plot showing mean importance.\cr
##'   }
##' @param ... Sent to lattice functions.
##' @return A plot object of \code{trellis} class.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso plots
##' @export
vimpplot <- function(batch, style, ...){
    if(missing(style)) style <- if(length(batch)*length(batch[[1]]) > 20) "bw" else "strip"
    if(is.character(style)) style <- pmatch(style, c("bw", "violin", "strip", "mean"))
    type <- names(batch[[1]])
    descriptors <- rownames(batch[[1]][[1]]$vimp)
    n.desc <- length(descriptors)

    if(style < 4){
        plot.data <- do.call(rbind, lapply(type, function(my.type){
            data.frame(type=my.type, do.call(rbind, lapply(1:n.desc, function(i){
                data.frame(descriptor=descriptors[i], imp=as.vector(sapply(batch, function(pr){
                    as.matrix(pr[[my.type]]$vimp)[i,]
                })))
            })))
        }))
        if(style < 3){
            return(bwplot(descriptor ~ imp | type, plot.data, scales=list(x="free"),
                          panel=if(style==1) panel.bwplot else panel.violin, ...))
        } else {
            plot.data <- transform(plot.data, imp=imp/sapply(split(imp, type), mean)[type])
            return(stripplot(descriptor ~ imp, plot.data, groups=type, jitter=.8,
                             auto.key=TRUE, panel=function(...){
                                 panel.rect(-1,                   2*1:floor(n.desc/2)-.5,
                                            2*max(plot.data$imp), 2*1:floor(n.desc/2) + .5,
                                            col="#eeeeee", border=NA)
                                 panel.stripplot(...)
                             }, ...))
        }
    } else if(style == 4){
        plot.data <- do.call(rbind, lapply(type, function(my.type){
            data.frame(type=my.type, do.call(rbind, lapply(1:n.desc, function(i){
                data.frame(descriptor=descriptors[i], imp=mean(sapply(batch, function(pr){
                    as.matrix(pr[[my.type]]$vimp)[i,]
                })))
            })))
        }))
        plot.data <- transform(plot.data, imp=imp/sapply(split(imp, type), mean)[type])
        return(barchart(descriptor ~ imp, plot.data, groups=type, auto.key=TRUE,
                        xlim=c(0, 1.1*max(plot.data$imp)), ...))
    }
}


##' Confusion matrix plot
##' 
##' @param y True class labels, i.e. the same vector that was used for testing.
##' @param batch A batch of fitted classifiers.
##' @param las Axis label orientation, see \code{\link{par}} for details.
##' @param labline The location of axis titles, see the \code{line} argument of
##'   \code{\link{mtext}} for details.
##' @param layout In case you are not happy with the default.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso plots
##' @export
confusionplot <- function(y, batch, las=c(2,1), labline=3, layout){
    nlev <- length(levels(y))
    confmat <- lapply(names(batch[[1]]), function(type){
        cm <- Reduce("+", lapply(batch, function(b) table(true=y, predicted=b[[type]]$pred))) / length(batch)
        if(length(batch) > 1) round(cm, digits=1)
        return(cm)
    })
    names(confmat) <- names(batch[[1]])
    if(missing(layout)){
        nside <- ceiling(sqrt(length(confmat)))
        layout <- matrix(c(1:length(confmat), rep(0, nside^2-length(confmat))), nside, nside)
    }
    layout(layout)
    for(type in names(confmat)){
        numm <- sweep(confmat[[type]], 1, table(y), "/")
        diag(numm) <- -diag(numm)
        chrm <- as.character(confmat[[type]])
        chrm[confmat[[type]] == 0] <- ""
        image(0:nlev + .5, 0:nlev + .5, numm, zlim=c(-1,1)*max(abs(numm)), useRaster=TRUE,
              col=colorRampPalette(c("#00bb00", "#ffffff", "#bb0000"))(99), axes=FALSE,
              xlab="", ylab="", main=if(length(confmat) > 1) type else "")
        mtext("True class", 1, line=labline)
        axis(1, 1:nlev, levels(y), las=las[1])
        mtext("Predicted class", 2, line=labline)
        axis(2, 1:nlev, levels(y), las=las[1])
        text(rep(1:nlev, nlev), rep(1:nlev, rep(nlev, nlev)), chrm)
    }
}

