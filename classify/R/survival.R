##' Risk score split point tuning for survival analysis
##' 
##' Given a set of observations with estimated risks of an event occurring, this
##' function estimates the ideal group division in respect to differential
##' outcome. It returns one or two split points at which the risk scores are to
##' be split to maximize difference in survival according to a statistical test.
##' 
##' All possible split points are considered and the ones minimizing the p-value
##' between the groups are returned.
##' 
##' Note that to report results found using this method you must validate them
##' on an external testset to not risk overfitting, i.e. a nonsensical result
##' that only holds for you dataset but not the underlying population.
##' 
##' @param x Estimated risk of the observations or similar. This is variable that
##'   the split points will be tuned on.
##' @param y \code{\link{outcome}} vector.
##' @param method The test that will be used for evaluating the groups produced
##'   by each choice of split points.
##' @param reject Are observations allowed to be rejected from the model? If
##'   \code{TRUE} two extreme groups and one uncertain group is formed, otherwise
##'   the uncertain group is divided up into the extreme groups.
##' @param min.group.size Split points producing groups smaller than this value
##'   will not be investigated. Can be scalar (same for both groups) or a vector
##'   of length 2. If < 1 it is interpreted as a fraction and if >= 1 as an
##'   absolute group size.
##' @param multi If \code{TRUE} ties for best split point will all be returned,
##'   otherwise ties are settled by maximizing the geometric mean of the group
##'   sizes.
##' @param subset Subset of observations to work on.
##' @return One or two split points depending on if rejects are allowed.
##' @example examples/tune_survival.R
##' @seealso auto.risk, plot.auto.risk, image.auto.risk, refit.auto.risk
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
tune.survival <- function(x, y, method=c("gray", "logrank"), reject=FALSE,
                          min.group.size=.1, multi=FALSE, subset=TRUE){
    # Setup data
    if(is.Surv(y)) y <- as.outcome(y)
    if(is.outcome(y)){
        event <- integer.events(y)
        time <- y$time
    } else {
        stop("Invalid `y`.")
    }

    subset <- subset & !is.na(time) & !is.na(event) & !is.na(x)
    time <- time[subset]
    event <- event[subset]
    x <- x[subset]

    n <- length(x)
    if(length(min.group.size) == 1) min.group.size <- rep(min.group.size, 2)
    min.group.size <- sapply(min.group.size, function(mgs) if(mgs < 1) round(n*mgs) else mgs)

    # Setup the tuning function that is to be minimized
    method <- match.arg(method)
    tune.func <- switch(method,
        gray = function(g) log10(cuminc.test(time, event, g)),
        logrank = function(g) p.value(survdiff(Surv(time, event) ~ g))
    )

    # Calculate support surface
    thres <- sort(unique(x))
    n.thres <- length(thres)
    thres.n <- table(x)
    thres <- thres[cumsum(thres.n) >= min.group.size[1] &
                   rev(cumsum(rev(thres.n)) >= min.group.size[2])]
    if(length(thres) == 1) stop("All objects have the same risk.")
    thres <- thres[-1] - diff(thres)/2
    sink("/dev/null")
    support <- sapply(thres, function(t2){
        sapply(if(reject) thres else t2, function(t1){
            groups <- (x > t1) + (x > t2)
            if(t1 > t2 | any(sapply(c(0,2), function(i) sum(groups == i)) < min.group.size)) return(NA)
            groups[groups == 1] <- NA
            tryCatch({
                tune.func(groups)
            }, error=function(err){
                warning(paste("Support calculation", err))
                return(NA)
            })
        })
    })
    sink()

    # Choose the threshold combination with the best score
    best.thres <- which(support == min(support, na.rm=TRUE), arr.ind=reject)
    if(nrow(as.matrix(best.thres)) > 1 && !multi)
        best.thres <- as.matrix(best.thres)[which.max(apply(best.thres, 1, function(x) exp(mean(log(x)))))[1], ]

    return(thres[best.thres])
}


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
##' print(cuminc(ss,cc,gg,strt))
##' print(cuminc.test(ss,cc,gg,strt))
##' @author Eva Freyhult
##' @export
cuminc.test <- function (ftime, fstatus, group, strata, rho = 0, cencode = 0, failcode = 1,
                        subset, na.action = na.omit){
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


