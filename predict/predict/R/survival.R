##' Gray's test
##' 
##' An extraction from \code{\link{cuminc}} that only performs the test for the
##' event of interest.
##' 
##' @param ftime Times or outcome vector, see \code{\link{cuminc}}.
##' @param fstatus Event types if \code{ftime} is numeric, see
##'   \code{\link{cuminc}}. Event of interest if \code{ftime} is of class
##'   outcome.
##' @param group See \code{\link{cuminc}}.
##' @param strata See \code{\link{cuminc}}.
##' @param rho See \code{\link{cuminc}}.
##' @param cencode See \code{\link{cuminc}}.
##' @param failcode See \code{\link{cuminc}}.
##' @param subset See \code{\link{cuminc}}.
##' @param na.action See \code{\link{cuminc}}.
##' @return P-value for event of interest.
##' @examples
##' set.seed(2)
##' ss <- rexp(100)
##' gg <- factor(sample(1:3,100,replace=TRUE),1:3,c('a','b','c'))
##' cc <- sample(0:2,100,replace=TRUE)
##' strt <- sample(1:2,100,replace=TRUE)
##' print(cmprsk::cuminc(ss,cc,gg,strt))
##' print(cuminc.test(ss,cc,gg,strt))
##' @author Eva Freyhult
##' @export
cuminc.test <- function (ftime, fstatus, group, strata, rho = 0, cencode = 0, failcode = 1,
                        subset, na.action = na.omit){
    library(cmprsk)                        
    if(is.outcome(ftime)){
        y <- ftime
        ftime <- y$time
        if(!fstatus %in% levels(y$event) &&
           !fstatus %in% 1:length(levels(y$event)))
            stop(sprintf("Invalid event type `%s`", as.character(fstatus)))
        if(missing(fstatus)){
            fstatus <- na.fill(as.integer(y$event), 0)
        } else {
            # Make our event of interest, `fstatus`, have number 1.
            fstatus <- match(y$event, c(NA, fstatus, setdiff(levels(y$event), fstatus)))-1
        }
    }
    d <- data.frame(time = ftime, cause = fstatus, group = as.factor(if (missing(group)) 
        rep(1, length(ftime))
    else group), strata = as.factor(if (missing(strata)) 
        rep(1, length(ftime))
    else strata))
    if (!missing(subset)) 
        d <- d[subset, ]
    tmp <- nrow(d)
    d <- na.action(d)
    if (nrow(d) != tmp) 
        cat(format(tmp - nrow(d)), "cases omitted due to missing values\n")
    no <- nrow(d)
    cg <- "  "
    nst <- length(levels(d$strata))
    d <- d[order(d$time), ]
    ugg <- table(d$group)
    d$group <- factor(d$group, names(ugg)[ugg > 0])
    ugg <- levels(d$group)
    censind <- 1*(d$cause != cencode)
    ng <- length(ugg)
    if (ng<2)
      stop("There must be at least two groups")
    ng1 <- ng - 1
    ng2 <- ng * ng1/2
    v <- matrix(0, nrow = ng1, ncol = ng1)
    storage.mode(v) <- "double"
    vt <- double(ng2)
    s <- double(ng1)
    stat <- double(1)
    ii <- 1
    causeind <- ifelse(d$cause == failcode, 1, 0)
    causeind <- 2 * censind - causeind
    z2 <- .Fortran("crstm", as.double(d$time), as.integer(causeind), 
                   as.integer(d$group), as.integer(d$strata), as.integer(no), 
                   as.double(rho), as.integer(nst), as.integer(ng), 
                   s, v, as.double(d$time), as.integer(causeind), 
                   as.integer(d$group), vt, s, vt, double((4 + 3 * 
                         ng) * ng), integer(4 * ng), PACKAGE = "cmprsk")
    stat[ii] <- -1
    a <- qr(z2[[10]])
    if (a$rank == ncol(a$qr)) {
      b <- diag(dim(a$qr)[1])
      stat[ii] <- z2[[9]] %*% qr.coef(a, b) %*% z2[[9]]
    }
    
    pchisq(stat, ng-1, lower.tail=FALSE)
}

##' Extraction of p-value from a statistical test
##'
##' These calculations are written in such a way that they avoid rounding off errors
##' that plague the \code{survival} and \code{cmprsk} packages. 
##'
##' @param x Test, i.e. a fitted object of a supported type.
##' @param log If \code{TRUE} \code{log(p.value)} is returned. If numeric it denotes
##'   the base in which the logarithm should be calculated (defaults to natural log).
##' @param ... Sent to class method.
##' @return p-value.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso p.value.crr, p.value.survdiff
##' @export
p.value <- function(x, log=FALSE, ...) UseMethod("p.value")

##' Extracts p-value from a competing risk model
##' 
##' @method p.value crr
##' @param x Fitted crr model, as returned by \code{\link[cmprsk]{crr}}.
##' @param log See \code{\link{p.value}}.
##' @param ... Ignored. Kept for S3 consistency.
##' @return Two-sided p-value.
##' @examples
##' library(cmprsk)
##' time <- 1:20
##' event <- c(rep(0, 9), rep(2, 3), rep(1, 8))
##' data <- rep(0:1, each=10)
##' x <- crr(time, event, data)
##' 
##' # Compare p-values of implementations
##' print(x)
##' p.value(x)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
p.value.crr <- function(x, log=FALSE, ...){
    if(log){
        pval <- log(2) + pnorm(abs(x$coef)/sqrt(diag(x$var)), lower.tail=FALSE, log.p=TRUE)
        return( if(is.numeric(log)) pval/log(log) else pval )
    } else {
        return(2 * pnorm(abs(x$coef)/sqrt(diag(x$var)), lower.tail=FALSE))
    }
}

##' Extracts p-value from a logrank test
##' 
##' @method p.value survdiff
##' @param x Logrank test result, as returned by \code{\link{survdiff}}.
##' @param log See \code{\link{p.value}}.
##' @param ... Ignored. Kept for S3 consistency.
##' @return p-value.
##' y <- Surv(time=1:100, event=rep(1:0, each=50))
##' groups <- rep(1:2, each=50)
##' x <- survdiff(y ~ groups)
##' 
##' # Compare p-values of implementations
##' print(x)
##' p.value(x)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
p.value.survdiff <- function(x, log=FALSE, ...){
    if(log){
        pval <- pchisq(x$chisq, length(x$n) - 1, lower.tail=FALSE, log.p=TRUE)
        return( if(is.numeric(log)) pval/log(log) else pval )
    } else {
        return(pchisq(x$chisq, length(x$n) - 1, lower.tail=FALSE))
    }
}


# ##' Design competing risk regression model
# ##' 
# ##' @param x Dataset.
# ##' @param y Response vector of class \code{\link{outcome}}.
# ##' @param ... Sent to \code{\link{crr}}
# ##' @author Christofer \enc{Bäcklin}{Backlin}
# ##' @export
# design.crr <- function(x, y, ...){
#     cmprsk::crr(y$time, integer.events(y)+1, x, ...)
# }
# 
# 
# ##' Predictions using competing risk regression model
# ##' 
# ##' Not implemented yet.
# ##' 
# ##' @param ... Ignored.
# ##' @author Christofer \enc{Bäcklin}{Backlin}
# ##' @export
# predict.crr <- function(...){
#     stop("predict.crr is not implemented yet.")
# }
 
