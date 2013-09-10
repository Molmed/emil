
decision.border <- function(fit, n=c(100, 100), ...){
    u <- par("usr")
    xi <- seq(u[1], u[2], length.out=n[1])
    yi <- seq(u[3], u[4], length.out=n[2])
    g <- expand.grid(x=xi, y=yi)
    p <- predict(fit, g)
    z <- matrix(p$prob[,2], n[1])
    contour(xi, yi, z, add=TRUE, ...)
}

