##' Setup a modelling.procedure
##'
##' A modelling.procedure is an object containing all information necessary to
##' carry out and evaluate the performance of a predictive modelling task with
##' \code{batch.predict}.
##' 
##' To use an out-of-the box algorithm with default values, only the
##' \code{method} argument needs to be set. To deviate from the defaults, e.g.
##' by using a custom function for model fitting, or an alternative tuning
##' grid, set the appropriate parameter with the desired value.
##' 
##' @param method The name of the modelling method. Only needed to identify
##'   plug-in functions, i.e. if you supply them yourself there is no need to
##'   set \code{method}.
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
##'   modelling with multiple modelling.procedures with different performance measures
##'   in the same call to \code{\link{batch.predict}}. An example of such a
##'   situation is if you wish to design both regression and classification models.
##' @return A list of functions and attributes that define a modelling.procedure.
##' @examples
##' # 1: Design linear discriminants without tuning any parameter,
##' # since it has none
##' modelling.procedure("lda")
##' 
##' # 2: Tune random forest's `mtry` parameter, with 3 possible values
##' modelling.procedure("rf", list(mtry = list(100, 250, 1000)))
##' 
##' # 3: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # with 3 values each, testing all 9 possible combinations
##' modelling.procedure("rf", list(mtry = list(100, 250, 1000),
##'                              maxnodes = list(5, 10, 25)))
##' 
##' # 4: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # but only test 3 manually specified combinations of the two
##' modelling.procedure("rf", list(list(mtry = 100, maxnodes = 5),
##'                              list(mtry = 250, maxnodes = 10),
##'                              list(mtry = 1000, maxnodes = 25)))
##' 
##' # 5: Tune elastic net's `alpha` and `lambda` parameters. Since elastic net's
##' # design function can tune `lambda` internally in a more efficient way
##' # than the general framework is able to do, only tune `alpha` and pass all
##' # `lambda` values as a single argument.
##' modelling.procedure("glmnet", list(alpha = seq(0, 1, length.out=6),
##'                                  lambda = list(seq(0, 5, length.out=30))))
##' 
##' # 6: Train elastic nets using the caret package's model fitting framework
##' modelling.procedure("caret", list(method = "glmnet",
##'     trControl = list(trainControl(verboseIter = TRUE, classProbs = TRUE))))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
modelling.procedure <- function(method, param=list(), error.fun=NULL, fit.fun, predict.fun, vimp.fun){
    if(any(sapply(list(NA, NULL, FALSE), identical, param))){
        warning("`param` must be supplied as a list. Assuming you really want `list()` i.e. to not set any parameters.")
        param <- list()
    }
    if(!is.null(names(param))){
        param <- apply(do.call(expand.grid, lapply(param, seq_along)), 1, function(i){
            mapply("[[", param, i, SIMPLIFY=FALSE)
        })
    }
    structure(class = "modelling.procedure", .Data = list(
        method = if(missing(method)) NULL else method,
        param = if(length(param) < 2) param else NULL,
        tuning = if(length(param) < 2) NULL else list(param = param, error = NULL),
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
##'   \code{\link{modelling.procedure}}.
is.tuned <- function(proc)
    !is.tunable(proc) || !is.null(proc$param)
is.tunable <- function(proc)
    !is.null(proc$tuning)
detune <- function(proc){
    proc$tuning <- NULL
    proc
}


##' @param test.subset The test subsets used for parameter tuning. Leave blank to
##'   randomly generate a resampling scheme of the same kind as is used by
##'   \code{\link{batch.predict}} to assess the performance of the whole design
##'   procedure.

if(FALSE){
    x <- sweep(matrix(rnorm(60*10), 60), 1, rep(c(0,.8), each=30))
    y <- gl(2,30)
    proc <- list(modelling.procedure("lda"),
                 modelling.procedure("elasticNet", list(alpha=0:4/4)))
    tun <- tune(proc, x, y, .save=list(pred=TRUE),
                test.subset=resample.crossval(y, 3, 3), .verbose=TRUE)
    fits <- fit(tun, x, y)
    fits <- fit(proc, x, y)

    ev <- evaluate.modelling(proc, x, y)
    
}

##' @param proc Tuned model fitting procedure.
batch.model <- function(proc, x, y,
    test.subset=resample.crossval(y, nfold=2, nrep=2), pre.process=pre.split,
    .save=list(fit=FALSE, pred=FALSE, vimp=FALSE, tuning=FALSE), .verbose=FALSE){

    if(inherits(proc, "modelling.procedure"))
        proc <- list(proc)
    .save <- lapply(c(fit="fit", pred="pred", vimp="vimp", tuning="tuning"), function(lab){
        if(!is.blank(.save[[lab]]) && .save[[lab]]) TRUE else FALSE
    })

#------------------------------------------------------------------------------o
#   Make sure all plug-ins exist before we start crunching

    do.tuning <- !sapply(proc, is.tuned)
    missing.fun <- unlist(mapply(function(p, to.be.tuned) c(
        if(!is.function(p$fit.fun))
            sprintf("fit.%s", p$method)
        else NULL,
        if(!is.function(p$predict.fun) && (.save$pred || to.be.tuned))
            sprintf("predict.%s", p$method)
        else NULL,
        if(!is.function(p$vimp.fun) && .save$vimp)
            sprintf("vimp.%s", p$method)
        else NULL
    ), proc, do.tuning, SIMPLIFY=FALSE))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))

    for(i in seq_along(proc)){
        if(is.null(proc[[i]]$error.fun)){
            proc[[i]]$error.fun <- if(class(y) == "factor"){
                error.rate
            } else if(inherits(y, c("outcome", "Surv"))){
                neg.harrell.C
            } else if(is.numeric(y)){
                mse
            } else {
                stop("Unknown type of respone vector, cannot guess performance measure. Please set `error.fun` manually.")
            }
        }
    }

#------------------------------------------------------------------------------o
#   Build and test models

    counter <- 0
    structure(class="modelling.result", .Data=lapply(test.subset, function(fold){
        counter <<- counter + 1
        if(inherits(test.subset, "crossval")){
            trace.msg(.verbose, sub("^fold(\\d):(\\d)$", "Replicate \\1, fold \\2:", colnames(test.subset)[counter]), time=FALSE)
        } else if(inherits(test.subset, "holdout")){
            trace.msg(.verbose, sub("^rep(\\d)$", "Replicate \\1:", colnames(test.subset)[counter]), time=FALSE)
        } else {
            trace.msg(.verbose, colnames(test.subset)[counter], time=FALSE)
        }
        if(any(do.tuning)){
            tune.subset <- sub.resample(y, prototype=test.subset, fold=fold)
            fold.proc <- tune(proc, x, y, test.subset=tune.subset,
                pre.process=pre.process, .save=NULL, .verbose=increase(.verbose))
        } else {
            fold.proc <- proc
        }
        trace.msg(increase(.verbose, 1), "Extracting fitting and testing datasets.")
        sets <- pre.process(x, y, fold)
        trace.msg(increase(.verbose, 1), "Fitting models.")
        res <- lapply(fold.proc, function(p){
            model <- do.call(function(...)
                    p$fit.fun(sets$fit, y[na.fill(!fold, FALSE)], ...), p$param)
            predictions <- p$predict.fun(model, sets$test)
            c(
                list(error = p$error.fun(y[na.fill(fold, FALSE)], predictions)),
                if(.save$fit) list(fit = model) else NULL,
                if(.save$pred) list(pred = predictions) else NULL,
                if(.save$vimp) list(vimp = p$vimp.fun(model)) else NULL
            )
        })
        if(any(do.tuning) && .save$tuning) res$tuning <- fold.proc
        if(counter == 1 && .verbose){
            os <- object.size(res)
            os.i <- trunc(log(os)/log(1024))
            trace.msg(increase(.verbose, 1), "Result size is %.2f %s.",
                exp(log(os) - os.i * log(1024)), c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB")[os.i + 1],
                time=FALSE)
        }
        res
    }))
}


##' @param ... Sent to \code{\link{tune}}, which will pass them on to
##'   \code{\link{batch.model}}.
fit <- function(proc, x, y, ..., .verbose){
    if(inherits(proc, "modelling.procedure"))
        proc <- list(proc)
    missing.fun <- unlist(lapply(proc, function(p)
        if(!is.function(p$fit.fun)) sprintf("fit.%s", p$method) else NULL))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))
    need.tuning <- !sapply(proc, is.tuned)
    if(missing(.verbose)) .verbose <- any(need.tuning)
    trace.msg(.verbose, "Model fitting:", length(proc), time=FALSE)
    if(any(need.tuning)){
        proc[need.tuning] <- tune(proc[need.tuning], x, y, ...,
            .verbose=increase(.verbose))
        trace.msg(increase(.verbose, 1), "Fitting final models.")
    } else {
        trace.msg(increase(.verbose, 1),
            if(any(sapply(proc, is.tunable))){
                if(length(proc) == 1){
                    "Process already tuned."
                } else {
                    paste(if(length(proc) == 2) "Both" else "All",
                          "processes already tuned.")
                }
            } else {
                "No tuning needed."
            },
            time=FALSE)
    }
    lapply(proc, function(p){
        do.call(function(...) p$fit.fun(x, y, ...), p$param)
    })
}


##' @param ... Sent to \code{\link{batch.model}}.
tune <- function(proc, ..., .retune=FALSE, .verbose=FALSE){
    trace.msg(.verbose, "Parameter tuning:", time=FALSE)
    if(inherits(proc, "modelling.procedure"))
        proc <- list(proc)
    if(.retune){
        do.tuning <- sapply(proc, is.tunable)
        discard.tuning <- do.tuning & sapply(proc, is.tuned)
        if(any(discard.tuning)){
            trace.msg(increase(.verbose, 1), "Discarding previous tuning of %i procedures.",
                sum(discard.tuning))
            for(i in which(discard.tuning))
                proc[[i]]$tuning <- NULL
        }
    } else {
        do.tuning <- !sapply(proc, is.tuned)
        not.tuned <- !do.tuning & sapply(proc, is.tunable)
        if(any(not.tuned)){
            trace.msg(increase(.verbose, 1), "%i procedures are already tuned and will not be retuned.",
                sum(not.tuned), time=FALSE)
        }
    }
    tune.proc <- unlist(lapply(proc[do.tuning], function(p){
        lapply(p$tuning$param, function(pp){
            p$param <- pp
            p$tuning <- NULL
            p
        })
    }), recursive=FALSE)
    tuning <- batch.model(tune.proc, ..., .verbose=increase(.verbose))
    proc.id <- rep(which(do.tuning),
                   sapply(proc[do.tuning], function(p) length(p$tuning$param)))
    for(i in which(do.tuning)){
        proc[[i]]$tuning$error <- ssubtree(tuning, T, proc.id == i, "error")
        rownames(proc[[i]]$tuning$error) <- NULL
        mean.err <- apply(proc[[i]]$tuning$error, 1, mean)
        best.param <- which(mean.err == min(mean.err))
        if(length(best.param) > 1) best.param <- sample(best.param, 1)
        proc[[i]]$param <- proc[[i]]$tuning$param[[best.param]]

        proc[[i]]$tuning$result <- tuning[proc.id == i]
    }
    proc
}


##' @param ... Sent to \code{\link{tune}} and \code{\link{batch.model}}.
evaluate.modelling <- function(proc, x, y, ...,
    .save=list(fit=FALSE, pred=TRUE, vimp=FALSE), .verbose=TRUE){

    trace.msg(.verbose, "Evaluating modelling performance:", time=FALSE)
    if(inherits(proc, "modelling.procedure"))
        proc <- list(proc)
    do.tuning <- sapply(proc, is.tunable)
    discard.tuning <- do.tuning & sapply(proc, is.tuned)
    if(any(discard.tuning)){
        trace.msg(increase(.verbose, 1),
            "Discarding previous tuning of %i modelling procedures.",
            sum(discard.tuning))
        for(i in which(discard.tuning))
            proc[[i]]$tuning <- NULL
    }
    batch.model(proc, x, y, ..., .save=.save, .verbose=increase(.verbose))
}

