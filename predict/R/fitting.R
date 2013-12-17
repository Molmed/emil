##' Setup a fitting.procedure
##'
##' A fitting.procedure is an object containing all information necessary to
##' carry out and evaluate the performance of a predictive modelling task with
##' \code{batch.predict}.
##' 
##' To use an out-of-the box algorithm with default values, only the
##' \code{method} argument needs to be set. To deviate from the defaults, e.g.
##' by using a custom function for model fitting, or an alternative tuning
##' grid, set the appropriate parameter with the desired value.
##' 
##' @param method The name of the modelling method.
##' @param param A list of model parameters. These will be fed to the design
##'   function after the dataset (\code{x} and \code{y} parameters). To tune a
##'   parameter, supply the candidate values in a vector or list.
##' 
##'   When tuning more than one parameter, all combinations of parameter values
##'   will be tested, if the elements of \code{param} are named. To manually
##'   specify which parameter value combinations to try, leave the the elements
##'   unnamed (see example 3 and 4).
##'   
##'   Parameters that should have vectors or lists as values, e.g. \code{trControl}
##'   when using
##'   \code{\link{design.caret}} to train caret models, must be wrapped in an
##'   additional list. That is, to set a parameter value to a list, but not tune it,
##'   make it a list of length 1 containing the list to be used (see example 6).
##' @param design The function to be used for model fitting.
##' @param predict The function to be used for model prediction.
##' @param vimp The function to be used for calculating or extracting variable
##'   importance scores.
##' @param error.fun Performance measure. Only set this if you wish to carry out
##'   modelling with multiple fitting.procedures with different performance measures
##'   in the same call to \code{\link{batch.predict}}. An example of such a
##'   situation is if you wish to design both regression and classification models.
##' @return A list of functions and attributes that define a fitting.procedure.
##' @examples
##' # 1: Design linear discriminants without tuning any parameter,
##' # since it has none
##' fitting.procedure("lda")
##' 
##' # 2: Tune random forest's `mtry` parameter, with 3 possible values
##' fitting.procedure("rf", list(mtry = list(100, 250, 1000)))
##' 
##' # 3: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # with 3 values each, testing all 9 possible combinations
##' fitting.procedure("rf", list(mtry = list(100, 250, 1000),
##'                              maxnodes = list(5, 10, 25)))
##' 
##' # 4: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # but only test 3 manually specified combinations of the two
##' fitting.procedure("rf", list(list(mtry = 100, maxnodes = 5),
##'                              list(mtry = 250, maxnodes = 10),
##'                              list(mtry = 1000, maxnodes = 25)))
##' 
##' # 5: Tune elastic net's `alpha` and `lambda` parameters. Since elastic net's
##' # design function can tune `lambda` internally in a more efficient way
##' # than the general framework is able to do, only tune `alpha` and pass all
##' # `lambda` values as a single argument.
##' fitting.procedure("glmnet", list(alpha = seq(0, 1, length.out=6),
##'                                  lambda = list(seq(0, 5, length.out=30))))
##' 
##' # 6: Train elastic nets using the caret package's model fitting framework
##' fitting.procedure("caret", list(method = "glmnet",
##'     trControl = list(trainControl(verboseIter = TRUE, classProbs = TRUE))))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
fitting.procedure <- function(method, param=list(), error.fun=NULL, fit.fun, predict.fun, vimp.fun){
    if(any(sapply(list(NA, NULL, FALSE), identical, param))){
        warning("`param` must be supplied as a list. Assuming you really want `list()` i.e. to not set any parameters.")
        param <- list()
    }
    if(!is.null(names(param))){
        param <- apply(do.call(expand.grid, lapply(param, seq_along)), 1, function(i){
            mapply("[[", param, i, SIMPLIFY=FALSE)
        })
    }
    structure(class = "fitting.procedure", .Data = list(
        method = method,
        param = if(length(param) < 2) param else NULL,
        tuning = if(length(param) < 2) NA else list(param = param, error = NULL),
        fit.fun =
            if(missing(fit.fun)){
                tryCatch(get(sprintf("fit.%s", method)), error=function(err) err)
            } else fit.fun,
        predict.fun =
            if(missing(predict.fun)){
                tryCatch(get(sprintf("predict.%s", method)), error=function(err) err)
            } else predict.fun,
        vimp.fun =
            if(missing(vimp.fun)){
                tryCatch(get(sprintf("vimp.%s", method)), error=function(err) err)
            } else vimp.fun,
        error.fun = NULL
    ))
}


##' @param proc Model fitting procedure, as produced by
##'   \code{\link{fitting.procedure}}.
is.tuned <- function(proc)
    !is.tunable(proc) || !is.null(proc$param)
is.tunable <- function(proc)
    !identical(proc$tuning, NA)


##' @param test.subset The test subsets used for parameter tuning. Leave blank to
##'   randomly generate a resampling scheme of the same kind as is used by
##'   \code{\link{batch.predict}} to assess the performance of the whole design
##'   procedure.

if(FALSE){
    x <- sweep(matrix(rnorm(60*10), 60), 1, rep(c(0,.8), each=30))
    y <- gl(2,30)
    proc <- list(fitting.procedure("lda"),
                 fitting.procedure("elasticNet", list(alpha=0:4/4)))
    tun <- tune(x, y, proc, .save=list(pred=TRUE),
                test.subset=resample.crossval(y, 2, 2), .verbose=TRUE)
    fits <- fit(x, y, tun)
    fits <- fit(x, y, proc)
}


tune <- function(x, y, proc,
    test.subset=resample.crossval(y, nfold=2, nrep=2), pre.process=pre.split,
    .save=list(fit=FALSE, pred=FALSE, vimp=FALSE), .verbose=FALSE){

    msg <- if(.verbose){
        function(level, ...) trace.msg(.verbose + level, ...)
    } else {
        function(...) invisible()
    }
    .save <- lapply(c(fit="fit", pred="pred", vimp="vimp"), function(lab){
        if(!is.blank(.save[[lab]]) && .save[[lab]]) TRUE else FALSE
    })

#------------------------------------------------------------------------------o
#   Make sure all plug-ins exist before we start crunching

    if(inherits(proc, "fitting.procedure"))
        proc <- list(proc)
    do.tuning <- sapply(proc, is.tunable)
    if(!any(do.tuning)){
        warning("All fitting procedures already have parameters selected, no need to tune.")
        return(proc)
    }

    missing.fun <- unlist(lapply(seq_along(proc), function(i) c(
        if(!is.function(proc[[i]]$fit.fun))
            sprintf("fit.%s", proc[[i]]$method)
        else NULL,
        if(do.tuning[i] && !is.function(proc[[i]]$predict.fun))
            sprintf("predict.%s", proc[[i]]$method)
        else NULL,
        if(do.tuning[i] && .save$vimp && !is.function(proc[[i]]$vimp.fun))
            sprintf("vimp.%s", proc[[i]]$method)
        else NULL
    )))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))

    for(i in seq_along(proc)){
        if(is.null(proc[[i]]$error.fun))
            proc[[i]]$error.fun <- if(class(y) == "factor"){
                error.rate
            } else if(inherits(y, c("outcome", "Surv"))){
                harrell.C
            if(is.numeric(y)){
                mse
            } else {
                stop("Unknown type of respone vector, cannot guess performance measure. Please set `error.fun` manually.")
            }
        }
    }

#------------------------------------------------------------------------------o
#   Tune parameters

    if(any(do.tuning)){
        counter <- 0
        tuning <- lapply(test.subset, function(fold){
            counter <<- counter + 1
            if(inherits(test.subset, "crossval")){
                msg(0, sub("^fold(\\d):(\\d)$", "Replicate \\1, fold \\2", colnames(test.subset)[counter]))
            } else if(inherits(test.subset, "holdout")){
                msg(0, sub("^fold(\\d)$", "Fold \\2", colnames(test.subset)[counter]))
            } else {
                msg(0, colnames(test.subset)[counter])
            }
            sets <- pre.process(x, y, fold)
            lapply(seq_along(proc), function(i){
                if(do.tuning[i]){
                    p <- proc[[i]]
                    lapply(p$tuning$param, function(param){
                        model <- structure(class="fitted.model",
                            .Data = do.call(function(...)
                                p$fit.fun(sets$fit, y[na.fill(!fold, FALSE)], ...), param))
                        predictions <- p$predict.fun(model, sets$test)
                        c(
                            list(error = p$error.fun(y[na.fill(fold, FALSE)], predictions)),
                            if(.save$fit) list(fit = model) else NULL,
                            if(.save$pred) list(pred = predictions) else NULL,
                            if(.save$vimp) list(vimp = p$vimp.fun(model)) else NULL
                        )
                    })
                } else {
                    NULL
                }
            })
        })
    }

#------------------------------------------------------------------------------o
#   Return the best performing parameters and the error

    for(i in which(do.tuning)){
        proc[[i]]$tuning$error <- ssubtree(tuning, T, i, T, "error")
        param.err <- apply(proc[[i]]$tuning$error, 2, mean)
        best.param <- which(param.err == min(param.err))
        if(length(best.param) > 1) best.param <- sample(best.param, 1)
        proc[[i]]$param <- proc[[i]]$tuning$param[[best.param]]

        for(lab in names(.save)[unlist(.save)])
            proc[[i]]$tuning[[lab]] <-
                lapply(tuning, function(tun) subtree(tun[[i]], T, lab))
    }
    proc
}

fit <- function(x, y, proc, ..., .verbose=TRUE){
    missing.fun <- unlist(lapply(proc, function(p)
        if(!is.function(p$fit.fun)) sprintf("fit.%s", p$method) else NULL))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))

    need.tuning <- !sapply(proc, is.tuned)
    if(any(needs.tuning)){
        if(.verbose) trace.msg(.verbose, "Tuning %i procedure%s",
            sum(need.tuning), if(sum(need.tuning) > 1) "s" else "")
        proc[need.tuning] <- tune(x, y, proc[need.tuning], ...,
            .verbose=.verbose * (.verbose+1))
    }
    lapply(proc, function(p){
        structure(class="fitted.model",
            .Data = do.call(function(...) p$fit.fun(x, y, ...), p$param))
    })
}

evaluate.modelling <- function(x, y, proc,
    test.subset=resample.crossval(y, nfold=2, nrep=2), pre.process=pre.split,
    .save=list(fit=FALSE, pred=FALSE, vimp=FALSE), .verbose=FALSE){

    msg <- if(.verbose){
        function(level, ...) trace.msg(.verbose + level, ...)
    } else {
        function(...) invisible()
    }
    .save <- lapply(c(fit="fit", pred="pred", vimp="vimp"), function(lab){
        if(!is.blank(.save[[lab]]) && .save[[lab]]) TRUE else FALSE
    })

#------------------------------------------------------------------------------o
#   Make sure all plug-ins exist before we start crunching

    if(inherits(proc, "fitting.procedure"))
        proc <- list(proc)
    do.tuning <- sapply(proc, is.tunable)

    missing.fun <- unlist(lapply(seq_along(proc), function(i) c(
        if(!is.function(proc[[i]]$fit.fun))
            sprintf("fit.%s", proc[[i]]$method)
        else NULL,
        if(do.tuning[i] && !is.function(proc[[i]]$predict.fun))
            sprintf("predict.%s", proc[[i]]$method)
        else NULL,
        if(do.tuning[i] && .save$vimp && !is.function(proc[[i]]$vimp.fun))
            sprintf("vimp.%s", proc[[i]]$method)
        else NULL
    )))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))

    for(i in seq_along(proc)){
        if(is.null(proc[[i]]$error.fun))
            proc[[i]]$error.fun <- if(class(y) == "factor"){
                error.rate
            } else if(inherits(y, c("outcome", "Surv"))){
                harrell.C
            if(is.numeric(y)){
                mse
            } else {
                stop("Unknown type of respone vector, cannot guess performance measure. Please set `error.fun` manually.")
            }
        }
    }

#------------------------------------------------------------------------------o
#   Run tests

    counter <- 0
    performance <- lapply(test.subset, function(fold){
        counter <<- counter + 1
        if(inherits(test.subset, "crossval")){
            msg(0, sub("^fold(\\d):(\\d)$", "Replicate \\1, fold \\2", colnames(test.subset)[counter]))
        } else if(inherits(test.subset, "holdout")){
            msg(0, sub("^fold(\\d)$", "Fold \\2", colnames(test.subset)[counter]))
        } else {
            msg(0, colnames(test.subset)[counter])
        }
        o

        sets <- pre.process(x, y, fold)
        lapply(seq_along(proc), function(i){
            if(do.tuning[i]){
                p <- proc[[i]]
                lapply(p$tuning$param, function(param){
                    model <- structure(class="fitted.model",
                        .Data = do.call(function(...)
                            p$fit.fun(sets$fit, y[na.fill(!fold, FALSE)], ...), param))
                    predictions <- p$predict.fun(model, sets$test)
                    c(
                        list(error = p$error.fun(y[na.fill(fold, FALSE)], predictions)),
                        if(.save$fit) list(fit = model) else NULL,
                        if(.save$pred) list(pred = predictions) else NULL,
                        if(.save$vimp) list(vimp = p$vimp.fun(model)) else NULL
                    )
                })
            } else {
                NULL
            }
        })
    })


}

