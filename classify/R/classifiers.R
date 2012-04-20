##' @import foreach
{}

##' Design a clasifier.
##' 
##' Fit a classifier to data with parameter selection using internal crossvalidation.
##' 
##' @param type Character, the kind of model to design. This package includes
##'   the following algorithms and custom ones can easily be implemented by the
##'   user, see below.
##'   \tabular{rl}{
##'     \code{lda} \tab Linear discriminant.\cr
##'     \code{qda} \tab Quadratic discriminant.\cr
##'     \code{nsc} \tab Nearest shrunken centroid as implemented by Hastie et al.\cr
##'     \code{rf} \tab Random forest as implemented by Breiman et al.\cr
##'   }
##'   Each of these have corresponding design functions \code{design.<type>}
##'   which takes care of the classifier specific details. To implement a custom
##'   classifier \code{myClassifier} simply make a new design function
##'   \code{design.myClassifier(x, y, ...)} and it will be callable from this wrapper.
##' @param x Dataset, matrix or data.frame.
##' @param y Class labels, factor or other type that can be converted to factor.
##' @param param List with additional parameters sent to the design function. If multiple
##'   values are given all combinations are evaluated using crossvalidation and
##'   a final classifier is designed with the best scoring combination.
##' 
##'   Example: \code{param=list(dimension=1:3, depth=c(10,20))} results in 6
##'   combinations, e.g. \code{dimension=3} and \code{depth=10}, and chooses the
##'   one with lowest crossvalidated error.
##' 
##'   If a parameter is non-scalar its values are to be supplied as a list.
##'   The parameter \code{threshold} in the call
##'   \code{design("nsc", ..., param=list(threshold=list(seq(0, 2, 30))))}
##'   will for instance be interpreted as having a single value, a 30-element-vector,
##'   instead of being subjected to parameter selection among 30 scalars, which
##'   would be the case if the call was
##'   \code{design("nsc", ..., param=list(threshold=seq(0, 2, 30)))}
##'   i.e. without putting \code{seq} in a list. This is
##'   important to keep in mind since \code{"nsc"} has its own native parameter
##'   selection incorporated into the design procedure which is faster than the
##'   general framework in \code{\link{design}}.
##' @param ncv Number of crossvaldiation folds used for parameter estimation.
##' @param subset Subset of data to work on.
##' @param pre.trans Transformation to be applied to the data in \code{x} before
##'   design and prediction. If the transformation depends on parameters to be
##'   estimated from the data their values are saved in the fitted classifier
##'   and reused when it is used for prediction. This is to avoid bias resulting
##'   from using the test data for parameter estimation.
##'   
##'   The internal selection of classifier parameters will also calculate and
##'   apply the transformation in the same way i.e. transformation parameters are
##'   estimated on the design folds and the transformation is applied to both
##'   design and test folds.
##'   
##'   Either select a transform function from the ones included in this package
##'   (see \code{\link{transforms}}) or write your own.
##' @param mode Type of model to fit, "classification", "regression" or
##'   "survival". If omitted it will be guessed from \code{y}.
##' @return A fitted classifier or regression model.
##' @example examples/design.R
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
design <- function(type, x, y, param=NULL, ncv=10, subset=TRUE, pre.trans=NULL, mode){
    if(length(type) > 1) stop("Use batch.design to fit multiple classifiers")

    # Set mode
    if(missing(mode)){
        mode <- switch(class(y), numeric="regression", integer="regression",
                       factor="classification", outcome="survival", Surv="survival")
    } else {
        modes <- c("classification", "regression", "survival")
        i <- pmatch(mode, modes)
        if(is.na(mode)) stop(sprintf("Unknown mode `%s`.", mode))
        mode <- modes[i]
        rm(i, modes)
    }
    if(mode == "classification"){
        if(!is.factor(y)) stop("`y` must be a factor in classification mode.")
        # Empircal error rate
        error.fun <- function(true,pred) mean(true != pred)
    } else if(mode == "regression"){
        if(!is.numeric(y)) stop("`y` must be numeric in regression mode.")
        # Mean square error
        error.fun <- function(true,pred) mean((true-pred)^2)
    } else if(mode == "survival"){
        if(is.Surv(y)) y <- as.outcome(y)
        if(!is.outcome(y)) stop("`y` must be of class outcome or Surv in survival mode.")
        # log P-value of Gray's test
        error.fun <- function(true, pred) log10(cuminc.test(true, , pred))
    }

    if(nrow(x) != length(y)){
       stop(sprintf("Dataset and response vector do not contain the same number of observations%s",
           if(ncol(x) == length(y)) sprintf(", check orientation of `%s`?", deparse(substitute(x))) else "."))
    }

    # Check subset for problems
    if(!is.logical(subset)){
        if(all(c(-1, 1) %in% sign(subset))) stop("test.subset contains mixed positive and negative values.")
        subset <- xor(1:length(y) %in% abs(subset), subset[1] < 0) # Takes care of negative numeric indexing
    }
    subset <- subset & !is.na(y) # If we don't know class we can't do anything
    if(length(unique(y[subset])) < 2) stop("Response `y` must contain at least two classes.")



    # Parameter estimation
    #
    #   Make sure parameter values are stored as a list regardless of
    #   if they were supplied as vectors or lists.
    #
    param <- lapply(param, function(x) if(is.list(x)) x else as.list(x))
    param.est <- any(sapply(param, length) > 1)
    if(param.est){
        param.combos <- expand.grid(param)
        cv.errors <- foreach(validation.subset = as.data.frame(crossval.groups(y,
                             nfold=ncv, subset=subset)), .combine=cbind) %do% {
            do.call(foreach, list(inner.param = apply(param.combos, 1, as.list), .combine=c)) %do% {
                tryCatch({
                    fit <- design(type, x, y, inner.param, ncv, subset=subset & !validation.subset, pre.trans=pre.trans)
                    error.fun(y[subset & validation.subset], predict(fit, x[subset & validation.subset,, drop=FALSE])$pred)
                }, error=function(...){
                    return(NA)
                })
            }
        }
        combo.error <- apply(cv.errors, 1, mean)
        if(all(is.na(combo.error))){
            stop(sprintf("%s could not be designed, all parameter combinations failed.", type))
        }
        param <- as.list(param.combos[which.min(combo.error),])
        names(param) <- names(param.combos)
    }

    # Setup transformation
    if(is.blank(pre.trans)){
        pre.trans <- identity
    } else {
        pre.trans <- pre.trans(x[subset,,drop=FALSE], y[subset])
    }

    # Design final classifier
    #
    #   The fit.class workaround is to preserve the original method's class.
    #
    fit <- do.call(paste("design", type, sep="."),
                   c(list(pre.trans(x[subset,, drop=FALSE]),
                          y[subset]),
                     unlist(param, recursive=FALSE)))
    fit.class <- class(fit)
    fit <- c(list(descriptors = if(is.blank(colnames(x))) 1:ncol(x) else colnames(x),
                  responses = levels(y),
                  pre.trans=pre.trans),
             if(param.est) list(param=param) else NULL,
             fit)
    class(fit) <- c(type, "classifier", fit.class)
    return(fit)
}


##' Combine predictions from several classifiers.
##' 
##' @param pred List of predictions, as returned by \code{link{predict}} called on
##'   a classifier from this package.
##' @param method One of the following or an abbreviation thereof.
##'   \tabular{rl}{
##'     "majority" \tab Majority voting (default).\cr
##'     "weighted" \tab Mean of estimated class probabilities.
##'   }
##' @return A single predictions object combined from all classifiers.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
ensemble <- function(pred, method="majority"){
    ensemble <- switch(pmatch(method, c("majority", "weighted")),
        {
            stop("Not yet implemented.")
        }, {
            add <- function(a, b) a + b
            lapply(names(pred[[1]]), function(my.type){
                res <- list(pred=NULL, prob=1/length(pred)*foreach(p=pred, .combine=add) %do% p[[my.type]]$prob)
                res$pred <- factor(levels(y)[1 + (res$prob[,2] >= .5)], levels=levels(y))
                return(res)
            })
        })
    if(length(levels(pred[[1]][[1]]$pred)) == 2) ensemble <- roc.measures(ensemble)
    names(ensemble) <- names(pred[[1]])
    return(ensemble)
}

##' Empirical ROC curve and AUC calculation.
##' 
##' Only possible for two-class-problems. No modifications of the ROC curve are
##' made, e.g. convex hull, although some have been advised in the litterature.
##' 
##' @param y True class labels, factor.
##' @param pred Predictions of \code{y} as returned by \code{\link{predict}} when
##'   called on a classifier from this package.
# @param combine Logical, whether to combine the predictions of all classifiers
#   of the same kind into a single curve. \emph{Note that this way of combining
#   the predictions corresponds to the "weighted" mode of \code{link{ensemble}}}.
#   although the confusion table has higher resolution.
# @param verbose Logical, whether to show progressbar when combining many
#   predictions which can take a while.
##' @return A modifier predictions list with the following additional arguments
##'   \item{conf}{A matrix with each row corresponding to confusion tables as
##'     the decision threshold moves from 0 to 1. This is essentially all the
##'     needed to draw a ROC curve.}
##'   \item{auc}{Area under ROC curve.}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
roc.measures <- function(y, pred){
    if(length(levels(y)) != 2)
        stop("ROC curve can only be calculated for two-class-problems.")
    if(!is.list(pred))
        stop("Invalid `pred` object.")
    if(!"prob" %in% names(pred))
        return(lapply(pred, function(p) roc.measures(y, p)))

    levs <- levels(y)
    y <- y == levs[2]
    prob <- pred$prob[,2]
    thres <- rev(c(-Inf, sort(unique(prob))))
    pred$conf <- data.frame(thres=thres, t(sapply(thres, function(thr){
        yhat <- prob > thr
        c(sum(!y & !yhat, na.rm=TRUE),
          sum( y & !yhat, na.rm=TRUE),
          sum(!y &  yhat, na.rm=TRUE),
          sum( y &  yhat, na.rm=TRUE))
    })))
    # For simplicity `thres` is saved here too even though it can be
    # derived from `prob` and `y`.

    if(all(pred$conf == 0)){
        pred$conf <- NA
        pred$auc <- NA
    } else {
        # Remove duplicates, this way is faster than `unique` since 
        # we only need to check the previous row and not all of them
        conf.rows <- c(TRUE, sapply(2:nrow(pred$conf), function(i)
            sum(pred$conf[i,-1] != pred$conf[i-1,-1]) > 0))
        pred$conf <- pred$conf[conf.rows,]
        colnames(pred$conf)[-1] <- paste(c(T,F,F,T), rep(levs, c(2,2)), sep=".")
        pred$auc <- with(pred, trapz(conf[[4]]/(conf[[2]]+conf[[4]]), conf[[5]]/(conf[[3]]+conf[[5]])))
    }
    return(pred)
}

##' Get performance of a prediction
##' 
##' @param pred Predictions, as returned by \code{\link{predict}} called on a
##'   trained classifier from this package.
##' @param measure Which performance measures to return, character vector with
##'   any combination of the following elements. Leave blank for all.
##'   \tabular{ll}{
##'     \code{tpr} \tab True positive rate, i.e. sensitivity.\cr
##'     \code{tnr} \tab True negative rate, i.e. specificity.\cr
##'     \code{fpr} \tab False positive rate.\cr
##'     \code{fnr} \tab False negative rate.\cr
##'     \code{ppv} \tab Positive predictive value.\cr
##'     \code{npv} \tab Negative predictive value.\cr
##'   }
##' @param ... Constraints to put on the measures, e.g. \code{tpr=.9} can be
##'   given to only look at thresholds that has a true positive rate of 90% or
##'   better. Note that \code{fpr} and \code{fnr} have opposite sign, i.e.
##'   {fpr=.1} is interpreted as false positive rate of 10% or less.
##' @param class Which class to be considered as positive.
##' @param silent Logical, wether to ignore errors and return \code{NULL}.
##' @return If a single measure under some constraint(s) was requested the best
##'   scalar value is returned, otherwise the entire curves of the choosen
##'   measures as functions of classification threshold is returned.
##' 
##'   If all constrains cannot be met simultaneously \code{NULL} is returned.
##'   under the imposed constraints in \code{...}.
##' @examples
##' \dontrun{
##'   performance(pred, c("ppa", "npv"))
##'   performance(pred, "fpr", tpr=.95)
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
performance <- function(pred, measure=TRUE, ..., class=2, silent=FALSE){
    if(!is.list(pred)){
        if(silent){
            return(NULL)
        } else {
            stop("Invalid \"pred\" value.")
        }
    } else {
        if(!"conf" %in% names(pred))
            lapply(pred, performance, measure=measure, ..., class=class, silent=silent)
    }
    if(!"conf" %in% names(pred)){
        return(lapply(pred, function(p) performance(p, measure, ..., class=class)))
    }
    const.sign <- c(tpr=1, tnr=1, fpr=2, fnr=2, ppv=1, npv=1)
    constraints <- list(...)
    if(any(!names(constraints) %in% names(const.sign))
    || any(constraints < 0) || any(constraints > 1))
        stop("Invalid constraints.")

    curves <- with(pred, data.frame(thres = conf$thres,
        tpr = conf[[5]]/(conf[[3]] + conf[[5]]),
        tnr = conf[[2]]/(conf[[2]] + conf[[4]]),
        fpr = conf[[4]]/(conf[[2]] + conf[[4]]),
        fnr = conf[[3]]/(conf[[3]] + conf[[5]]),
        ppv = conf[[5]]/(conf[[4]] + conf[[5]]),
        npv = conf[[2]]/(conf[[2]] + conf[[3]])))
    if(class == 1)
       curves <- with(curves, data.frame(tpr=tnr, tnr=tpr, fnr=fpr,
                                         fpr=fnr, ppv=npv, npv=ppv))
    idx <- rep(TRUE, nrow(curves))
    for(const in names(constraints)){
        # Choose type of comparison for the given constraint
        idx <- idx & do.call(list(`>=`, `<=`)[[const.sign[const]]], 
                             list(curves[[const]], constraints[[const]]))
    }

    if(!any(idx))
        return(NULL)
    if(!missing(measure) && length(measure) == 1 && !is.blank(constraints)){
        if(measure == "ppv") idx[1] <- FALSE
        if(measure == "npv") idx[length(idx)] <- FALSE
        return(do.call(list(max, min)[[const.sign[measure]]], list(curves[idx,measure])))
    }
    #if(is.character(measure)) measure <- c("thres", measure)
    return(curves[idx,measure])
}


##' Sensitivity/specificity of a classifier
##' 
##' Calculates true positive rate (TPR) i.e. sensitivity given a false
##' positive rate (FPR) i.e. 1 - specificity or true negative rate (TNR)
##' i.e. specificity given a false negative rate (FNR) 1 - sensitivity.
##' 
##' An example: If there are two classes A and B, the TPR is the fraction of
##' objects in A that the classifier can correctly classify as A while not
##' falsely classifiy objects in B as A more often than a given FPR.
##'
##' \emph{Only implemented for two-class problem.}
##'
##' @param pred Predictions as returned by \code{\link{predict}} or
##'   \code{\link{batch.design}}.
##' @param class The class to be considered.
##' @param fpr The FPR to get sensitivity at. If omitted the enire TPR(FPR)
##'   curve will be returned.
##' @return sensitivity: A sensitivity value or the entire TPR(FPR) curve if \code{fpr} is
##'   omitted.
##' @rdname sens.spec
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
sensitivity <- function(pred, class=2, fpr){
    if(!is.list(pred)) stop("Invalid \"pred\" value.")
    if(!"conf" %in% names(pred)){
        return(lapply(pred, function(p) sensitivity(p, class, fpr)))
    }
    
    tpr.curve <- with(pred, conf[[4]]/(conf[[2]] + conf[[4]]))
    fpr.curve <- with(pred, conf[[3]]/(conf[[1]] + conf[[3]]))
    if(class == 1){
        temp <- tpr.curve
        tpr.curve <- fpr.curve
        fpr.curve <- temp
        rm(temp)
    }
    if(missing(fpr)){
        return(list(tpr=tpr.curve, fpr=fpr.curve))
    } else {
        return(approx(fpr.curve, tpr.curve, fpr)$y)
    }
}


# Sensitivity/specificity of a classifier
#
##' @param fnr The FNR to get specificity at. If omitted the enire TNR(FNR)
##'   curve will be returned.
##' @return specificity: A specificity value or the entire TNR(FNR) curve if
##'   \code{fnr} is omitted.
##' @rdname sens.spec
# @author Christofer \enc{Bäcklin}{Backlin}
##' @export
specificity <- function(pred, class=2, fnr){
    if(missing(fnr)){
        curve <- sensitivity(pred, class=3-class)
        names(curve) <- c("tnr", "fnr")
        return(curve)
    } else {
        return(sensitivity(pred, class=3-class, fpr=fnr))
    }
}


##' Variable importance of a fitted classifier.
##'
##' \code{vimp} is a generic function for extracting variable importance from
##' fitted classifiers. The function invokes particular _methods_ which depend
##' on the 'class' of the first argument. Note that different _method_ calculates
##' variable importance in different ways and that they are not directly comparable.
##'
##' @param object Fitted classifier.
##' @param ... Sent on to class method.
##' @return Variable importance vector.
##' @seealso batch.vimp
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
vimp <- function(object, ...){
    if(length(object$responses) != 2)
        stop("Variable importance is only implemented for binary classification problems.")

    if(!any(sapply(sprintf("vimp.%s", class(object)), exists)))
        stop(sprintf("No variable importance measure is implemented for classifier type \"%s\".", class(object$fit)[1]))
    
    UseMethod("vimp", object)
}

