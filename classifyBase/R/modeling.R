##' Run a whole design procedure
##' 
##' Design and evaluate the performance of a collection of prediction models with
##' resampling.
##' 
##' @param x Dataset with observations as rows and features as columns.
##' @param y Response vector. The class of \code{y} determines the type of the
##'   prediction, \code{\link{factor}} = classification, \code{\link{numeric}}
##'   = regression and \code{Surv} or \code{\link{outcome}} = survival analysis.
##'   Notice however that you also need to specify an appropriate model.
##' @param models The type of models to design. This parameter allows for the
##'   construction of several different models on the same data and also
##'   internal tuning of parameters.
##'   
##'   Models are to be specified as a list where each element's name determine
##'   the type (e.g. \code{"rf"} for random forest) and each element's content
##'   sets parameters (e.g. \code{rf=list(mtry=5000)}).
##' 
##'   To tune a parameter specify its possible values as in a list, e.g.
##'   \code{rf=list(mtry=list(500, 1000, 2000), ntree=list(5000))}.
##'       
##'   If more than one parameter is to be tuned all combinations of parameter
##'   values will be tested. If only specifc combinations should be tested they
##'   should be supplied as individual lists on this form:
##'   \code{rf=list(list(mtry=500, ntree=100), list(mtry=2000, ntree=50))}
##' @param test.subset A resampling scheme as returned from
##'   \code{\link{resample.crossval}} or \code{\link{resample.holdout}}.
##' @param error.fun An optimization function. If omitted it will be selected
##'   according to prediction type: \code{\link{error.rate}} for classification,
##'   and \code{\link{rmse}} for regression. Since there is no obvious choice
##'   for how to evalute survival analysis models it has to be specified
##'   manually.
##' @param pre.trans This function will be used to extract design and test sets
##'   from the entire dataset including customized data preprocessing
##'   e.g. imputation, centering, compression etc. See \code{\link{pre.trans}}
##'   for details.
##' @param save.fits Whether to include the designed models in the returned
##'   results. Note that the size of these depend on the model type and could
##'   potentially be very large.
##' @param save.vimp Whether to calculate and return variable importance.
##' @examples
##' x <- sweep(matrix(rnorm(60*10), 60), 1, rep(c(0,.4), each=30))
##' y <- gl(2,30)
##' pred <- batch.model(x, y, "lda", resample.crossval(y, nfold=3, nrep=3))
##'
##' \dontrun{
##' models <- list(lda=list(pi=list(c(.25, .75), c(.5, .5))), lda=list())
##' pred <- batch.model(x, y, models, resample.crossval(y, nfold=3, nrep=3))
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
batch.model <- function(x, y, models, test.subset, error.fun, pre.trans=pre.split,
                   save.fits = FALSE, save.vimp = FALSE){
    # This elaborate construction allows us to call an arbitrary design function
    # with a dataset and a list of parameters without combining them in a single
    # list (i.e. `do.call(fun, c(list(data), params))`), which would result in
    # an unnecessary copy of the dataset in the memory.
    do.design.call <- function(type, x, y, param){
        f <- function(...) get(sprintf("design.%s", type))(x=x, y=y, ...)
        structure(do.call("f", param), class=c(type, "classifier"))
    }

    # Set up tuning
    if(is.character(models))
        models <- structure(vector("list", length(models)), names=models)
    out <- list(models = lapply(models, function(m){
        if(is.null(names(m))){
            # Every element correspond to a complete parameter setup
            m
        } else {
            # Every element correspond to one parameter, return all combinations
            apply(do.call(expand.grid, lapply(m, seq)), 1, function(i){
                mapply("[[", m, i, SIMPLIFY=FALSE) 
            })
        }
    }))
    out$models <- lapply(out$models, function(x) if(is.blank(x)) list() else x)
    do.tuning <- sapply(out$models, length) > 1

    if(missing(error.fun))
        error.fun <- switch(class(y),
            factor=error.rate,
            numeric=rmse,
            stop("You must explicitly provide an error function when the response not factor or numeric."))

    out$cv <- lapply(test.subset, function(fold){
        if(any(do.tuning)){
            tuning.cv <- if(inherits(test.subset, "crossval")){
                resample.crossval(y, nfold=attr(test.subset, "nfold"),
                    nrep=attr(test.subset, "nrep"), balanced=attr(test.subset, "balanced"),
                    subset=!fold)
            } else if(inherits(test.subset, "holdout")){
                resample.holdout(y, frac=attr(test.subset, "frac"), nrep=ncol(test.subset),
                    balanced=attr(test.subset, "balanced"), subset=!fold)
            } else {
                stop("Cannot determine resampling scheme, needed for parameter tuning.")
            }
            tuning <- lapply(tuning.cv, function(tuning.fold){
                sets <- pre.trans(x, tuning.fold)
                lapply(which(do.tuning), function(i){
                    foreach(my.param=out$models[[i]], .combine=c) %do% {
                        error.fun(y[na.fill(tuning.fold, FALSE)],
                            predict(do.design.call(names(out$models)[i], sets$design,
                                y[na.fill(!tuning.fold, FALSE)], my.param), sets$test))
                    }
                })
            })
            tuning.err <- lapply(seq(do.tuning), function(i)
                if(do.tuning[i]) ssubtree(tuning, T, i) else NULL)
            names(tuning.err) <- names(do.tuning)
            my.param <- mapply(function(p, e){
                if(is.blank(e)){
                    p
                } else {
                    e <- apply(e, 1, mean)
                    p[[sample(which(e == min(e)), 1)]]
                }
            }, out$models, tuning.err)
        } else {
            my.param <- out$models
        }
        sets <- pre.trans(x, fold)
        res <- foreach(i = seq(out$models)) %do% {
            fit <- do.design.call(names(out$models)[i], sets$design,
                                  y[na.fill(!fold, FALSE)], my.param[[i]])
            pred <- predict(fit, sets$test)
            err <- error.fun(y[na.fill(fold, FALSE)], pred)
            c(pred, list(error=err), if(save.fits) fit else NULL,
              if(save.vimp) vimp(fit) else NULL,
              if(do.tuning[i]) list(param=my.param[[i]], tuning.err=tuning.err[[i]]) else NULL)
        }
        names(res) <- names(out$models)
        res
    })
    out
}

