##' Setup a modeling procedure
##'
##' A modeling.procedure is an object containing all information necessary to
##' carry out and evaluate the performance of a predictive modeling task, with
##' \code{\link{fit}}, \code{\link{tune}}, or \code{\link{evaluate.modeling}}.
##' 
##' To use an out-of-the box algorithm with default values, only the
##' \code{method} argument needs to be set. To deviate from the defaults, e.g.
##' by using a custom function for model fitting, or an alternative tuning
##' grid, set the appropriate parameter with the desired value. See
##' \code{\link{emil.extensions}} for a guide on how these functions should be
##' written.
##' 
##' @param method The name of the modeling method. Only needed to identify
##'   plug-in functions, i.e. if you supply them yourself there is no need to
##'   set \code{method}.
##' @param param A list of model parameters. These will be fed to the fitting
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
##'   \code{\link{fit.caret}} to train caret models, must be wrapped in an
##'   additional list. That is, to set a parameter value to a list, but not tune it,
##'   make it a list of length 1 containing the list to be used (see example 6).
##' @param fit.fun The function to be used for model fitting.
##' @param predict.fun The function to be used for model prediction.
##' @param vimp.fun The function to be used for calculating or extracting variable
##'   importance scores.
##' @param error.fun Performance measure used to evaluate modeling procedures
##'   and to tune parameters. See \code{\link{error.fun}} for details.
##' @return An object of class \code{modeling.procedure}.
##' @examples
##' # 1: Fit linear discriminants without tuning any parameter,
##' # since it has none
##' modeling.procedure("lda")
##' 
##' # 2: Tune random forest's `mtry` parameter, with 3 possible values
##' modeling.procedure("randomForest", list(mtry = list(100, 250, 1000)))
##' 
##' # 3: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # with 3 values each, testing all 9 possible combinations
##' modeling.procedure("randomForest", list(mtry = list(100, 250, 1000),
##'                                          maxnodes = list(5, 10, 25)))
##' 
##' # 4: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # but only test 3 manually specified combinations of the two
##' modeling.procedure("randomForest", list(list(mtry = 100, maxnodes = 5),
##'                                     list(mtry = 250, maxnodes = 10),
##'                                     list(mtry = 1000, maxnodes = 25)))
##' 
##' # 5: Tune elastic net's `alpha` and `lambda` parameters. Since elastic net's
##' # fitting function can tune `lambda` internally in a more efficient way
##' # than the general framework is able to do, only tune `alpha` and pass all
##' # `lambda` values as a single argument.
##' modeling.procedure("glmnet", list(alpha = seq(0, 1, length.out=6),
##'                                    lambda = list(seq(0, 5, length.out=30))))
##' 
##' # 6: Train elastic nets using the caret package's model fitting framework
##' library(caret)
##' modeling.procedure("caret", list(method = "glmnet",
##'     trControl = list(trainControl(verboseIter = TRUE, classProbs = TRUE))))
##' @seealso fit, tune, evaluate.modeling, batch.model, emil.extensions
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
modeling.procedure <- function(method, param=list(), error.fun=NULL, fit.fun, predict.fun, vimp.fun){
    if(any(sapply(list(NA, FALSE), identical, param))){
        warning("`param` must be supplied as a list. Assuming you really want `list()` i.e. to not set any parameters.")
        param <- list()
    }
    if(!is.null(names(param))){
        param <- apply(do.call(expand.grid, lapply(param, seq_along)), 1, function(i){
            mapply("[[", param, i, SIMPLIFY=FALSE)
        })
    }
    proc <- structure(class = "modeling.procedure", .Data = list(
        method = if(missing(method)) NULL else method,
        param = if(length(param) == 0) list() else if(length(param) == 1) param[[1]] else NULL,
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
    if(!is.function(proc$fit.fun))
        stop("No fitting function found.")
    proc
}


##' Print method for modeling procedure
##' 
##' @param x modeling procedure.
##' @param ... Ignored (kept for S3 consistency).
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
print.modeling.procedure <- function(x, ...){
    yn <- function(x) if(is.null(x) || inherits(x, "error")) "no" else "yes"
    cat(sprintf(
"%s modeling procedure.

   model fitting function:       %s
   prediction function:          %s
   variable importance function: %s
   individual error function:    %s
   
   number of parameter sets to tune over: %i
   tuned: %s\n",
        if(is.null(x$method)) "Custom" else paste0("`", x$method, "`"),
        yn(x$fit.fun), yn(x$predict.fun), yn(x$vimp.fun), yn(x$error.fun),
        if(is.null(x$tuning$param)) 1 else length(x$tuning$param),
        if(is.tunable(x)) if(is.tuned(x)) "yes" else "no" else "not needed"
    ))
}


##' Perform modeling
##' 
##' This function is the core of the framework, carrying out most of the work.
##' It fits and evaluates models according to a resampling scheme, and extracts
##' variable importance scores.
##'
##' Note that the typical user does not have to call this function
##' directly, but should use \code{\link{fit}}, \code{\link{tune}} or
##' \code{\link{evaluate.modeling}} instead.
##' 
##' @param proc modeling procedure, or list of modeling procedures, as
##'   produced by \code{\link{modeling.procedure}}.
##' @param x Dataset, observations as rows and descriptors as columns.
##' @param y Response vector.
##' @param resample The test subsets used for parameter tuning. Leave blank to
##'   randomly generate a resampling scheme of the same kind as is used by
##'   \code{\link{batch.model}} to assess the performance of the whole
##'   modeling procedure.
##' @param pre.process Function that performs pre-processing and splits dataset
##'   into fitting and test subsets.
##' @param .save What aspects of the modeling to perform and return to the
##'   user.
##' @param .parallel.cores Number of CPU-cores to use for parallel computation.
##' @param .checkpoint.dir Directory to save intermediate results to. If set
##'   the computation can be restarted with minimal loss of results.
##' @param .verbose Whether to print an activity log.
##' @return A list tree where the top level corresponds to folds (in case of
##'   multiple folds), the next level corresponds to the modeling procedures
##'   (in case of multiple procedures), and the final level is specified by the
##'   \code{.save} parameter. It typically contains a subset of the following
##'   elements:
##'   \describe{
##'       \item{\code{error}}{Performance estimate of the fitted model. See
##'           \code{\link{error.fun}} for more information.}
##'       \item{\code{fit}}{Fitted model.}
##'       \item{\code{pred}}{Predictions given by the model.}
##'       \item{\code{vimp}}{Variable importance scores.}
##'       \item{\code{tune}}{Results from the parameter tuning. See
##'           \code{\link{tune}} for details.}
##'   }
##' @seealso modeling.procedure, fit, tune, evaluate.modeling
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
batch.model <- function(proc, x, y,
    resample=resample.crossval(y, nfold=2, nrep=2), pre.process=pre.split,
    .save=list(fit=FALSE, pred=FALSE, vimp=FALSE, tuning=FALSE),
    .parallel.cores=1, .checkpoint.dir=NULL, .verbose=FALSE){

    if(inherits(proc, "modeling.procedure")){
        multi.proc <- FALSE
        proc <- listify(proc)
    } else {
        multi.proc <- TRUE
    }
    debug.flags <- get.debug.flags(proc)
    if(is.logical(resample)){
        multi.fold <- FALSE
        resample <- structure(list(resample),
            class=c(setdiff(class(resample), "fold"), "list"))
    } else {
        multi.fold <- TRUE
    }
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
    proc <- set.debug.flags(proc, debug.flags)

#------------------------------------------------------------------------------o
#   Set up parallelization and checkpointing

    if(.parallel.cores > 1){
        nice.require("parallel")
        options(mc.cores = .parallel.cores)
        mapply.FUN <- parallel::mcmapply
    } else {
        mapply.FUN <- mapply
    }
    if(!is.null(.checkpoint.dir)){
        if(!file.exists(.checkpoint.dir))
            dir.create(.checkpoint.dir)
        checkpoint.files <- sprintf("%s/%s.Rdata",
            .checkpoint.dir, gsub("\\W+", "-", names(resample)))
    } else {
        checkpoint.files <- list(NULL)
    }

#------------------------------------------------------------------------------o
#   Build and test models

    counter <- 0
    res <- structure(class="modeling.result", .Data=mapply.FUN(function(fold, fold.name, checkpoint.file){
        if(inherits(resample, "crossval")){
            trace.msg(.verbose, sub("^fold(\\d+):(\\d+)$", "Replicate \\1, fold \\2:", fold.name),
                      time=.parallel.cores > 1, linebreak=FALSE)
        } else if(inherits(resample, "holdout")){
            trace.msg(.verbose, sub("^fold(\\d+)$", "Fold \\1:", fold.name),
                      time=.parallel.cores > 1, linebreak=FALSE)
        } else {
            trace.msg(.verbose, fold.name, time=.parallel.cores > 1, linebreak=FALSE)
        }
        if(!is.null(checkpoint.file) && file.exists(checkpoint.file)){
            if(.verbose) cat(" Already completed.\n")
            en <- new.env()
            load(checkpoint.file, envir=en)
            return(en$res)
        } else if(.verbose) cat("\n")
        if(.parallel.cores > 1) .verbose = FALSE
        if(any(do.tuning)){
            tune.subset <- resample.subset(y, fold)
            fold.proc <- tune(proc, x, y, resample=tune.subset,
                pre.process=pre.process, .save=NULL, .verbose=increase(.verbose))
        } else {
            fold.proc <- proc
        }
        trace.msg(increase(.verbose, 1), "Extracting fitting and testing datasets.")
        sets <- pre.process(x, y, fold)
        trace.msg(increase(.verbose, 1), "Fitting models.")
        res <- lapply(fold.proc, function(p){
            model <- do.call(function(...)
                    p$fit.fun(sets$fit, y[index.fit(fold)], ...), p$param)
            predictions <- p$predict.fun(model, sets$test)
            c(
                list(error = p$error.fun(y[index.test(fold)], predictions)),
                if(.save$fit) list(fit = model) else NULL,
                if(.save$pred) list(pred = predictions) else NULL,
                if(.save$vimp) list(vimp = fixvimp(p$vimp.fun(model), sets$features)) else NULL,
                if(.save$tuning && is.tunable(p))
                    list(param=p$param, tuning = p$tuning) else NULL
            )
        })
        if(.parallel.cores == 1 && is.null(checkpoint.file)){
            counter <<- counter + 1
            if(counter == 1) t1 <- Sys.time()
            if(.verbose && counter == 1 && is.data.frame(resample) && ncol(resample) > 1){
                t2 <- t1 + difftime(Sys.time(), t1, units="sec")*ncol(resample)
                fmt <- if(difftime(t2, t1, units="days") < 1){
                    "%H:%M"
                } else if(difftime(t2, t1, units="days") < 2 &&
                          as.integer(strftime(t2, "%d")) -
                          as.integer(strftime(t1, "%d")) == 1){
                    "%H:%M tomorrow"
                } else if(difftime(t2, t1, units="days") < 365){
                    "%H:%M, %b %d"
                } else {
                    "%H:%M, %b %d, %Y"
                }
                trace.msg(increase(.verbose, 1),
                          "Estimated completion time %sis %s.",
                          if("tune" %in% sapply(sys.calls(), function(x) as.character(x)[1]))
                              "of tuning " else "",
                          strftime(t2, fmt), time=FALSE)
                os <- object.size(res)
                os.i <- trunc(log(os)/log(1024))
                if(os.i == 0){
                    trace.msg(increase(.verbose, 1), "Result size is %i B.", os, time=FALSE)
                } else {
                    trace.msg(increase(.verbose, 1), "Result size is %.2f %s.",
                        exp(log(os) - os.i * log(1024)), c("KiB", "MiB", "GiB", "TiB", "PiB", "EiB")[os.i],
                        time=FALSE)
                }
            }
        }
        if(!multi.proc) res <- res[[1]]
        if(!is.null(checkpoint.file)){
            save(res, file=checkpoint.file)
        }
        res
    }, resample, names(resample), checkpoint.files, SIMPLIFY=FALSE))
    if(multi.fold) res else res[[1]]
}


##' Extractor function for modeling result
##' 
##' As opposed to the standard extractor function, this will keep the class.
##' 
##' @param x modeling result object, as produced by \code{\link{batch.model}}.
##' @param ... Sent to \code{\link{`[`}}.
##' @noRd
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
`[.modeling.result` <- function(x, ...){
    structure(unclass(x)[...], class="modeling.results")
}


##' Fit a model
##' 
##' @param proc modeling procedure, or list of modeling procedures, as
##'   produced by \code{\link{modeling.procedure}}.
##' @param x Dataset, observations as rows and descriptors as columns.
##' @param y Response vector.
##' @param ... Sent to \code{\link{tune}}, in case tuning is required,
##'   which will pass them on to \code{\link{batch.model}}.
##' @param .verbose Whether to print an activity log.
##' @return A list of fitted models.
##' @seealso modeling.procedure, tune, evaluate.modeling, batch.model
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
fit <- function(proc, x, y, ..., .verbose){
    reset.warn.once()
    if(inherits(proc, "modeling.procedure")){
        multi.proc <- FALSE
        proc <- listify(proc)
    } else {
        multi.proc <- TRUE
    }
    missing.fun <- unlist(lapply(proc, function(p)
        if(!is.function(p$fit.fun)) sprintf("fit.%s", p$method) else NULL))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))
    need.tuning <- !sapply(proc, is.tuned)
    if(missing(.verbose)) .verbose <- any(need.tuning)
    trace.msg(.verbose, "Model fitting:", length(proc))
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
    res <- lapply(proc, function(p){
        do.call(function(...) p$fit.fun(x, y, ...), p$param)
    })
    if(multi.proc) res else res[[1]]
}


##' Tune parameters of modeling procedures
##' 
##' @param proc modeling procedure, or list of modeling procedures, as
##'   produced by \code{\link{modeling.procedure}}.
##' @param ... Sent to \code{\link{batch.model}}.
##' @param .retune Whether to retune already tuned processes.
##' @param .verbose Whether to print an activity log.
##' @return A list of tuned modeling procedures.
##' @seealso modeling.procedure, fit, evaluate.modeling, batch.model
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
tune <- function(proc, ..., .retune=FALSE, .verbose=FALSE){
    trace.msg(.verbose, "Parameter tuning:")
    if(inherits(proc, "modeling.procedure")){
        multi.proc <- FALSE
        proc <- listify(proc)
    } else {
        multi.proc <- TRUE
    }
    debug.flags <- get.debug.flags(proc)
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
    proc <- set.debug.flags(proc, debug.flags)
    tuning <- batch.model(tune.proc, ..., .verbose=increase(.verbose))
    proc.id <- rep(which(do.tuning),
                   sapply(proc[do.tuning], function(p) length(p$tuning$param)))
    for(i in which(do.tuning)){
        proc[[i]]$tuning$error <-
            sapply(tuning, function(tun) unlist(subtree(tun, proc.id == i, "error")))
        rownames(proc[[i]]$tuning$error) <- NULL
        mean.err <- apply(proc[[i]]$tuning$error, 1, mean)
        best.param <- which(mean.err == min(mean.err))
        if(length(best.param) > 1) best.param <- sample(best.param, 1)
        proc[[i]]$param <- proc[[i]]$tuning$param[[best.param]]
        proc[[i]]$tuning$result <- tuning[proc.id == i]
    }
    proc <- set.debug.flags(proc, debug.flags)
    if(multi.proc) proc else proc[[1]]
}
##' @rdname tune
##' @return Logical indicating if the procedure(s) are tuned.
##' @export
is.tuned <- function(proc)
    !is.tunable(proc) || !is.null(proc$param)
##' @rdname tune
##' @return Logical indicating if the has tunable parameters.
##' @export
is.tunable <- function(proc)
    !is.null(proc$tuning)
##' @rdname tune
##' @return A list of untuned modeling procedures.
##' @export
detune <- function(proc){
    debug.flags <- sapply(proc, function(p) is.function(p) && isdebugged(p))
    proc$tuning <- NULL
    lapply(proc[debug.flags], debug)
    proc
}


##' Performance estimation of modeling procedures
##' 
##' This function performs the important task of evaluating the performance of
##' a modeling procedure with resampling, including tuning and pre-processing
##' to not bias the results by information leakage.
##' 
##' @param proc modeling procedure, or list of modeling procedures, as
##'   produced by \code{\link{modeling.procedure}}.
##' @param x Dataset, observations as rows and descriptors as columns.
##' @param y Response vector.
##' @param ... Sent to \code{\link{tune}} and \code{\link{batch.model}}.
##' @param .save See \code{\link{batch.model}}.
##' @param .verbose Whether to print an activity log.
##' @return A list of fitted models.
##' @seealso modeling.procedure, fit, tune, batch.model
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
evaluate.modeling <- function(proc, x, y, ...,
    .save=list(fit=FALSE, pred=TRUE, vimp=FALSE, tuning=TRUE), .verbose=TRUE){

    reset.warn.once()
    trace.msg(.verbose, "Evaluating modeling performance:")
    if(inherits(proc, "modeling.procedure")){
        multi.proc <- FALSE
        proc <- listify(proc)
    } else {
        multi.proc <- TRUE
    }
    do.tuning <- sapply(proc, is.tunable)
    discard.tuning <- do.tuning & sapply(proc, is.tuned)
    if(any(discard.tuning)){
        trace.msg(increase(.verbose, 1),
            "Discarding previous tuning of %i modeling procedures.",
            sum(discard.tuning))
        for(i in which(discard.tuning))
            proc[[i]]$tuning <- NULL
    }

    return(batch.model(if(multi.proc) proc else proc[[1]], x, y, ...,
           .save=.save, .verbose=increase(.verbose)))
}


##' Preserve debugging flags when manipulating lists
##'
##' \emph{In R version >= 3.1.0 these functions are no longer needed due to
##' changed behavior of the debugging facilities.}
##'
##' These functions are designed to copy and reset debugging flags when
##' manipulating modeling procedures or lists of such. See the code of
##' \code{\link{tune}}, e.g. by calling \code{page(tune)} for an example
##' of how to work with them.
##'
##' By default, when performing an operation on a list, in whole or in part, the
##' whole list will be overwritten by a new list containing the results of the
##' operation. This will discard debugging flags set by \code{\link{debug}},
##' which is not desirable when working with lists containing functions.
##'
##' @param proc A modeling procedure to be wrapped in a one-element-list.
##' @param proc.list A list of modeling procedures.
##' @param debug.flags A list of debugging flags, as produced by
##'   \code{\link{get.debug.flags}}
##' @return \code{list(x)} with debug flags maintained.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
listify <- function(proc){
    debug.flags <- sapply(proc, function(p) is.function(p) && isdebugged(p))
    proc <- list(proc)
    lapply(proc[[1]][debug.flags], debug)
    proc
}
# @rdname listify
##' @noRd
get.debug.flags <- function(proc.list){
    lapply(proc.list, sapply, function(p) is.function(p) && isdebugged(p))
}
# @rdname listify
##' @noRd
set.debug.flags <- function(proc.list, debug.flags){
    if(as.integer(R.Version()$major) < 3 || as.numeric(R.Version()$minor) < 1){
        for(i in seq_along(debug.flags)){
            for(j in seq_along(debug.flags[[i]])){
                if(debug.flags[[i]][j]) debug(proc.list[[i]][[j]])
            }
        }
    }
    proc.list
}
