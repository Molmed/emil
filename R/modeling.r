#' Setup a modeling procedure
#'
#' A modeling procedure is an object containing all information necessary to
#' carry out and evaluate the performance of a predictive modeling task with
#' \code{\link{fit}}, \code{\link{tune}}, or \code{\link{evaluate}}.
#' To use an out-of-the box algorithm with default values, only the
#' \code{method} argument needs to be set. See \code{\link{emil}} for a
#' list of available methods. To deviate from the defaults, e.g. by tuning
#' variables or using a custom function for model fitting, set the appropriate
#' parameters as described below.
#' For a guide on how to implement a custom method see the documentaion page
#' \code{\link{extension}}.
#' 
#' @param method The name of the modeling method. Only needed to identify
#'   plug-in functions, i.e. if you supply them yourself there is no need to
#'   set \code{method}.
#' @param param A list of model parameters. These will be fed to the fitting
#'   function after the dataset (\code{x} and \code{y} parameters). To tune a
#'   parameter, supply the candidate values in a vector or list.
#' 
#'   When tuning more than one parameter, all combinations of parameter values
#'   will be tested, if the elements of \code{param} are named. To manually
#'   specify which parameter value combinations to try, leave the the elements
#'   unnamed (see example 3 and 4).
#'   
#'   Parameters that should have vectors or lists as values, e.g. \code{trControl}
#'   when using
#'   \code{\link{fit_caret}} to train caret models, must be wrapped in an
#'   additional list. That is, to set a parameter value to a list, but not tune it,
#'   make it a list of length 1 containing the list to be used (see example 6).
#' @param fit_fun The function to be used for model fitting.
#' @param predict_fun The function to be used for model prediction.
#' @param importance_fun The function to be used for calculating or extracting variable
#'   prediction scores.
#' @param error_fun Performance measure used to evaluate procedures
#'   and to tune parameters. See \code{\link{error_fun}} for details.
#' @return An object of class \code{modeling_procedure}.
#' @example examples/modeling_procedure.r
#' @seealso \code{\link{emil}}, \code{\link{evaluate}},
#'   \code{\link{fit}}, \code{\link{tune}},
#'   \code{\link[=predict.modeling_procedure]{predict}}, \code{\link{importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
modeling_procedure <- function(method, param=list(), error_fun=NULL, fit_fun, predict_fun, importance_fun){
    if(any(sapply(list(NA, FALSE), identical, param))){
        warning("`param` must be supplied as a list. Assuming you really want `list()` i.e. to not set any parameters.")
        param <- list()
    }
    if(!is.null(names(param))){
        param <- apply(do.call(expand.grid, lapply(param, seq_along)), 1, function(i){
            Map("[[", param, i)
        })
    }
    procedure <- structure(class = "modeling_procedure", .Data = list(
        method = if(missing(method)) NULL else method,
        param = if(length(param) == 0) list() else if(length(param) == 1) param[[1]] else NULL,
        tuning = if(length(param) < 2) NULL else list(param = param, error = NULL),
        fit_fun =
            if(missing(fit_fun)){
                tryCatch(get(sprintf("fit_%s", method)), error=function(err) err)
            } else fit_fun,
        predict_fun =
            if(missing(predict_fun)){
                tryCatch(get(sprintf("predict_%s", method)), error=function(err) err)
            } else predict_fun,
        importance_fun =
            if(missing(importance_fun)){
                tryCatch(get(sprintf("importance_%s", method)), error=function(err) err)
            } else importance_fun,
        error_fun = error_fun
    ))
    if(!is.function(procedure$fit_fun))
        stop("No fitting function found.")
    procedure
}


#' Print method for modeling procedure
#' 
#' @param x modeling procedure.
#' @param ... Ignored (kept for S3 consistency).
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
print.modeling_procedure <- function(x, ...){
    yn <- function(x){
        if(is.null(x) || inherits(x, "error")){
            "no"
        } else {
            if(isdebugged(x)) "yes (debug)" else "yes"
        }
    }
    cat(sprintf(
"%s modeling procedure.

   model fitting function:       %s
   prediction function:          %s
   variable importance function: %s
   individual error function:    %s
   
   number of parameter sets to tune over: %i
   tuned: %s\n",
        if(is.null(x$method)) "Custom" else paste0("`", x$method, "`"),
        yn(x$fit_fun), yn(x$predict_fun), yn(x$importance_fun), yn(x$error_fun),
        if(is.null(x$tuning$param)) 1 else length(x$tuning$param),
        if(is_tunable(x)) if(is_tuned(x)) "yes" else "no" else "not needed"
    ))
}


#' Perform modeling
#' 
#' This function is the core of the framework, carrying out most of the work.
#' It fits and evaluates models according to a resampling scheme, and extracts
#' variable importance scores.
#' Note that the typical user does not have to call this function
#' directly, but should use \code{\link{fit}}, \code{\link{tune}} or
#' \code{\link{evaluate}} instead.
#' 
#' @param procedure Modeling procedure, or list of modeling procedures, as
#'   produced by \code{\link{modeling_procedure}}.
#' @param x Dataset, observations as rows and descriptors as columns.
#' @param y Response vector.
#' @param resample The test subsets used for parameter tuning. Leave blank to
#'   randomly generate a resampling scheme of the same kind as is used by
#'   \code{\link{batch_model}} to assess the performance of the whole
#'   modeling_procedure.
#' @param pre_process Function that performs pre-processing and splits dataset
#'   into fitting and test subsets.
#' @param .save What aspects of the modeling to perform and return to the
#'   user.
#' @param .cores Number of CPU-cores to use for parallel computation.
#'   The current implementation is based on \code{\link{mcMap}}, which
#'   unfortunatelly do not work on Windows systems. It can however be
#'   re-implemented by the user fairly easily by setting up a PSOCK cluster and
#'   calling \code{\link{parLapply}} as in the example below. This solution
#'   might be included in future versions of the package, after further
#'   investigation.
#' @param .checkpoint_dir Directory to save intermediate results to, after
#'   every completed fold. The directory will be created if it doesn't exist,
#'   but not recursively.
#' @param .return_error If \code{FALSE} the entire modeling is aborted upon an
#'   error. If \code{TRUE} the modeling of the particular fold is aborted and
#'   the error message is returned instead of its results.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   also suppress output generated from the procedure's functions.
#' @return A list tree where the top level corresponds to folds (in case of
#'   multiple folds), the next level corresponds to the modeling procedures
#'   (in case of multiple procedures), and the final level is specified by the
#'   \code{.save} parameter. It typically contains a subset of the following
#'   elements:
#'   \describe{
#'       \item{\code{error}}{Performance estimate of the fitted model. See
#'           \code{\link{error_fun}} for more information.}
#'       \item{\code{fit}}{Fitted model.}
#'       \item{\code{prediction}}{Predictions given by the model.}
#'       \item{\code{importance}}{Variable importance scores.}
#'       \item{\code{tune}}{Results from the parameter tuning. See
#'           \code{\link{tune}} for details.}
#'   }
#' @example examples/batch_model.r
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
batch_model <- function(procedure, x, y,
    resample=emil::resample("crossvalidation", y, nfold=2, nreplicate=2), pre_process=pre_split,
    .save=list(fit=FALSE, prediction=FALSE, importance=FALSE, tuning=FALSE),
    .cores=1, .checkpoint_dir=NULL, .return_error=.cores > 1,
    .verbose=FALSE){

    if(inherits(procedure, "modeling_procedure")){
        multi.procedure <- FALSE
        procedure <- listify(procedure)
    } else {
        multi.procedure <- TRUE
    }
    debug.flags <- get_debug_flag(procedure)
    if(is.logical(resample)){
        multi.fold <- FALSE
        resample <- data.frame(resample)
        names(resample) <- attr(resample[[1]], "fold.name")
    } else {
        multi.fold <- TRUE
    }
    make.na <- is.na(y) & !Reduce("&", lapply(resample, is.na))
    if(any(make.na)){
        log_message(indent(.verbose, 0), "%i observations will be excluded from the modeling due to missing values.", sum(make.na))
        resample[make.na,] <- NA
    }

    .save <- lapply(c(fit="fit", prediction="prediction", importance="importance", tuning="tuning"), function(lab){
        if(!is_blank(.save[[lab]]) && .save[[lab]]) TRUE else FALSE
    })

#------------------------------------------------------------------------------o
#   Make sure all plug-ins exist before we start crunching

    do.tuning <- !sapply(procedure, is_tuned)
    missing.fun <- unlist(Map(function(p, to.be.tuned) c(
        if(!is.function(p$fit_fun))
            sprintf("fit.%s", p$method)
        else NULL,
        if(!is.function(p$predict_fun) && (.save$prediction || to.be.tuned))
            sprintf("predict.%s", p$method)
        else NULL,
        if(!is.function(p$importance_fun) && .save$importance)
            sprintf("importance.%s", p$method)
        else NULL
    ), procedure, do.tuning))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))

    for(i in seq_along(procedure)){
        if(is.null(procedure[[i]]$error_fun)){
            procedure[[i]]$error_fun <- if(class(y) == "factor"){
                error_rate
            } else if(inherits(y, "Surv")){
                neg_harrell_c
            } else if(is.numeric(y)){
                mse
            } else {
                stop("Unknown type of response vector, cannot guess performance measure. Please set `error_fun` manually.")
            }
        }
    }
    procedure <- set_debug_flag(procedure, debug.flags)

#------------------------------------------------------------------------------o
#   Set up parallelization, error handling and checkpointing

    if(.Platform$OS.type == "windows" && .cores > 1){
        notify_once(id = "windows_parallelization",
                    "Parallelization is not yet implemented for windows systems. Please set it up manually as described in `?batch_model`. Running sequentially.",
                    fun = warning)
        .cores <- 1
    }
    if(.cores > 1){
        requireNamespace("parallel")
        Map_fun <- function(f, ...)
            parallel::mcmapply(f, ..., SIMPLIFY=FALSE,
                               mc.silent=.verbose, mc.cores=.cores)
    } else {
        Map_fun <- Map
    }
    if(.return_error){
        modeling_fun <- function(f, ...){
            Map_fun(function(...){
                tryCatch(f(...), error = function(err){
                    log_message(indent(.verbose, 1), "An error occurred, skipping fold.")
                    err
                })
            }, ...)
        }
    } else {
        modeling_fun <- Map_fun
    }
    if(!is.null(.checkpoint_dir)){
        if(!file.exists(.checkpoint_dir))
            dir.create(.checkpoint_dir)
        if(file.access(.checkpoint_dir, 6) != 0)
            stop("You do not have read and write permissions to the checkpoint directory.")
        checkpoint.files <- sprintf("%s/%s.Rdata",
            .checkpoint_dir, gsub("\\W+", "-", names(resample)))
    } else {
        checkpoint.files <- list(NULL)
    }

#------------------------------------------------------------------------------o
#   Build and test models

    counter <- 0
    res <- structure(class="modeling_result", .Data=modeling_fun(function(fold, fold.name, checkpoint.file){

        # Setup run time estimation
        if(.cores == 1 && is.null(checkpoint.file)){
            counter <<- counter + 1
            if(counter == 1) t1 <- Sys.time()
        }

        # Print status message
        if(inherits(resample, "crossvalidation")){
            log_message(indent(.verbose, 0), sub("^fold(\\prediction+):(\\prediction+)$", "Replicate \\1, fold \\2:", fold.name),
                      appendLF=FALSE)
        } else if(inherits(resample, "holdout")){
            log_message(indent(.verbose, 0), sub("^fold(\\prediction+)$", "Fold \\1:", fold.name),
                      appendLF=FALSE)
        } else {
            log_message(indent(.verbose, 0), fold.name, appendLF=FALSE)
        }

        # Check for checkpoint files
        if(!is.null(checkpoint.file) && file.exists(checkpoint.file)){
            if(indent(.verbose, 0)) cat(" Already completed.\n")
            en <- new.env()
            load(checkpoint.file, envir=en)
            return(en$res)
        } else if(indent(.verbose, 0)) cat("\n")

        # Disable further messages if run in parallel
        if(.cores > 1) .verbose <- -1

        # Do the work
        if(any(do.tuning)){
            tune.subset <- subresample(fold, y)
            fold.procedure <- tune(procedure, x, y, resample=tune.subset,
                pre_process=pre_process, .save=NULL, .verbose=indent(.verbose, 1))
        } else {
            fold.procedure <- procedure
        }
        log_message(indent(.verbose, 1), "Extracting fitting and testing datasets.")
        sets <- pre_process(x, y, fold)
        log_message(indent(.verbose, 1), "Fitting models and making predictions.")
        res <- lapply(fold.procedure, function(p){
            if(.verbose < 0){
                # TODO: This block should be replaced with a call to the fit function
                capture.output(suppressMessages({
                    model <- do.call(function(...)
                        p$fit_fun(sets$fit$x, sets$fit$y, ...), p$param)
                }))
            } else {
                model <- do.call(function(...)
                    p$fit_fun(sets$fit$x, sets$fit$y, ...), p$param)
            }
            prediction <- predict(p, model, sets$test$x, .verbose=indent(.verbose, 1))
            structure(c(
                if(.save$fit) list(fit = model) else NULL,
                list(error = p$error_fun(sets$test$y, prediction)),
                if(.save$prediction) list(prediction = prediction) else NULL,
                if(.save$importance) list(importance = fixvimp(importance(p, model, .verbose=indent(.verbose, 1)), sets$features)) else NULL,
                if(.save$tuning && is_tunable(p))
                    list(param=p$param, tuning = p$tuning) else NULL
            ), class="model")
        })

        # Estimate run time
        if(.cores == 1 && is.null(checkpoint.file)){
            if(indent(.verbose, 0) && counter == 1 && is.data.frame(resample) && ncol(resample) > 1){
                # Fold result size
                os <- object.size(res)
                os.i <- trunc(log(os)/log(1024))
                if(os.i == 0){
                    log_message(indent(.verbose, 0), "Result size is %i B.", os, time=FALSE)
                } else {
                    log_message(indent(.verbose, 0), "Result size is %.2f %s.",
                        exp(log(os) - os.i * log(1024)), c("KiB", "MiB", "GiB", "TiB", "PiB", "EiB")[os.i],
                        time=FALSE)
                }
                # Estimated completion time
                t2 <- t1 + difftime(Sys.time(), t1, units="sec")*ncol(resample)
                fmt <- if(difftime(t2, t1, units="days") < 1){
                    "%H:%M"
                } else if(difftime(t2, t1, units="days") < 2 &&
                          as.integer(strftime(t2, "%prediction")) -
                          as.integer(strftime(t1, "%prediction")) == 1){
                    "%H:%M tomorrow"
                } else if(difftime(t2, t1, units="days") < 365){
                    "%H:%M, %b %prediction"
                } else {
                    "%H:%M, %b %prediction, %Y"
                }
                log_message(indent(.verbose, 0),
                          "Estimated completion time %sis %s.",
                          if("tune" %in% sapply(sys.calls(), function(x) as.character(x)[1]))
                              "of tuning " else "",
                          strftime(t2, fmt), time=FALSE)
            }
        }

        # Save checkpoint file
        if(!multi.procedure) res <- res[[1]]
        if(!is.null(checkpoint.file)){
            save(res, file=checkpoint.file)
        }

        res
    }, resample, names(resample), checkpoint.files))
    if(multi.fold) res else res[[1]]
}


#' Extractor function for modeling result
#' 
#' As opposed to the standard extractor function, this will keep the class.
#' 
#' @param x modeling result object, as produced by \code{\link{batch_model}}.
#' @param ... Sent to \code{\link{`[`}}.
#' @noRd
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
`[.modeling_result` <- function(x, ...){
    structure(unclass(x)[...], class="modeling_result")
}


#' Fit a model
#'
#' Fits a model according to a modeling procedure. If the procedure contains
#' untuned variables they it will automatically be tuned prior to fitting. Note
#' however that the tuning statistics will not be return.
#' 
#' @param procedure Modeling procedure, or list of modeling procedures, as
#'   produced by \code{\link{modeling_procedure}}.
#' @param x Dataset, observations as rows and descriptors as columns.
#' @param y Response vector.
#' @param ... Sent to \code{\link{tune}}, in case tuning is required,
#'   which will pass them on to \code{\link{batch_model}}.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A list of fitted models.
#' @examples
#' procedure <- modeling_procedure("lda")
#' mod <- fit(procedure, x=iris[-5], y=iris$Species)
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{evaluate}}, \code{\link{tune}},
#'   \code{\link[=predict.modeling_procedure]{predict}}, \code{\link{importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
fit <- function(procedure, x, y, ..., .verbose=TRUE){
    if(inherits(procedure, "modeling_procedure")){
        multi.procedure <- FALSE
        procedure <- listify(procedure)
    } else {
        multi.procedure <- TRUE
    }
    missing.fun <- unlist(lapply(procedure, function(p)
        if(!is.function(p$fit_fun)) sprintf("fit.%s", p$method) else NULL))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))
    need.tuning <- !sapply(procedure, is_tuned)
    if(missing(.verbose)) .verbose <- any(need.tuning)
    log_message(indent(.verbose, 0), "Model fitting:", length(procedure))
    if(any(need.tuning)){
        procedure[need.tuning] <- tune(procedure[need.tuning], x, y, ...,
            .verbose=indent(.verbose, 1))
        log_message(indent(.verbose, 1), "Fitting final models.")
    } else {
        log_message(indent(.verbose, 1),
            if(any(sapply(procedure, is_tunable))){
                if(length(procedure) == 1){
                    "Process already tuned."
                } else {
                    paste(if(length(procedure) == 2) "Both" else "All",
                          "processes already tuned.")
                }
            } else {
                "No tuning needed."
            },
            time=FALSE)
    }
    res <- lapply(procedure, function(p){
        do.call(function(...) p$fit_fun(x, y, ...), p$param)
    })
    if(multi.procedure) res else res[[1]]
}


#' Tune parameters of modeling procedures
#'
#' These functions are rarely needed to be called manually as they are
#' automatically called by \code{\link{fit}} and \code{\link{evaluate}}
#' when needed.
#' 
#' @param procedure Modeling procedure, or list of modeling procedures, as
#'   produced by \code{\link{modeling_procedure}}.
#' @param ... Sent to \code{\link{batch_model}}.
#' @param .retune Whether to retune already tuned processes.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A tuned modeling procedures or a list of such.
#' @examples
#' procedure <- modeling_procedure("randomForest", param=list(mtry=1:4))
#' tuned.procedure <- tune(procedure, x=iris[-5], y=iris$Species)
#' mod <- fit(tuned.procedure, x=iris[-5], y=iris$Species)
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{evaluate}}, \code{\link{fit}},
#'   \code{\link[=predict.modeling_procedure]{predict}}, \code{\link{importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
tune <- function(procedure, ..., .retune=TRUE, .verbose=FALSE){
    log_message(indent(.verbose, 0), "Parameter tuning:")
    if(inherits(procedure, "modeling_procedure")){
        multi.procedure <- FALSE
        procedure <- listify(procedure)
    } else {
        multi.procedure <- TRUE
    }
    debug.flags <- get_debug_flag(procedure)
    do.tuning <- sapply(procedure, is_tunable)
    discard.tuning <- do.tuning & sapply(procedure, is_tuned)
    if(any(discard.tuning)){
        log_message(indent(.verbose, 1), "Discarding previous tuning of %i procedures.",
            sum(discard.tuning))
        for(i in which(discard.tuning))
            procedure[[i]]$tuning <- NULL
    }
    tune.procedure <- unlist(lapply(procedure[do.tuning], function(p){
        lapply(p$tuning$param, function(pp){
            p$param <- pp
            p["tuning"] <- list(NULL)
            p
        })
    }), recursive=FALSE)
    tune.procedure <- set_debug_flag(tune.procedure, rep(debug.flags,
        each=sapply(procedure, function(p) length(p$tuning$param) )))
    tuning <- batch_model(tune.procedure, ..., .verbose=indent(.verbose, 1))
    procedure.id <- rep(which(do.tuning),
                   sapply(procedure[do.tuning], function(p) length(p$tuning$param)))
    for(i in which(do.tuning)){
        procedure[[i]]$tuning$error <-
            sapply(tuning, function(tun) unlist(subtree(tun, procedure.id == i, "error")))
        rownames(procedure[[i]]$tuning$error) <- NULL
        mean.err <- apply(procedure[[i]]$tuning$error, 1, mean)
        best.param <- which(mean.err == min(mean.err))
        if(length(best.param) > 1) best.param <- sample(best.param, 1)
        procedure[[i]]$param <- procedure[[i]]$tuning$param[[best.param]]
        procedure[[i]]$tuning$result <- lapply(tuning, "[", procedure.id == i)
    }
    procedure <- set_debug_flag(procedure, debug.flags)
    if(multi.procedure) procedure else procedure[[1]]
}
#' @rdname tune
#' @return Logical indicating if the procedure(s) are tuned.
#' @export
is_tuned <- function(procedure)
    !is_tunable(procedure) || !is.null(procedure$param)
#' @rdname tune
#' @return Logical indicating if the has tunable parameters.
#' @export
is_tunable <- function(procedure)
    !is.null(procedure$tuning)
#' @rdname tune
#' @return A list of untuned modeling procedures.
#' @export
detune <- function(procedure){
    debug.flags <- sapply(procedure, function(p) is.function(p) && isdebugged(p))
    procedure$tuning <- NULL
    lapply(procedure[debug.flags], debug)
    procedure
}


#' Performance estimation of modeling procedures
#' 
#' This function performs the important task of evaluating the performance of
#' a modeling procedure with resampling, including tuning and pre-processing
#' to not bias the results by information leakage.
#' 
#' @param procedure Modeling procedure, or list of modeling procedures, as
#'   produced by \code{\link{modeling_procedure}}.
#' @param x Dataset, observations as rows and descriptors as columns.
#' @param y Response vector.
#' @param ... Sent to \code{\link{tune}} and \code{\link{batch_model}}.
#' @param .save See \code{\link{batch_model}}.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A list of fitted models.
#' @examples
#' procedure <- modeling_procedure("lda")
#' cv <- resample("crossvalidation", y=iris$Species, nfold=5, nreplicate=3)
#' perf <- evaluate(procedure, x=iris[-5], y=iris$Species, resample=cv)
#' err <- subtree(perf, TRUE, "error")
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{fit}}, \code{\link{tune}}
#'   \code{\link[=predict.modeling_procedure]{predict}}, \code{\link{importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
evaluate <- function(procedure, x, y, ...,
    .save=list(fit=FALSE, prediction=TRUE, importance=FALSE, tuning=TRUE),
    .verbose=TRUE){

    reset_notification()
    log_message(indent(.verbose, 0), "Evaluating modeling performance:")
    if(inherits(procedure, "modeling_procedure")){
        multi.procedure <- FALSE
        procedure <- listify(procedure)
    } else {
        multi.procedure <- TRUE
    }
    do.tuning <- sapply(procedure, is_tunable)
    discard.tuning <- do.tuning & sapply(procedure, is_tuned)
    if(any(discard.tuning)){
        log_message(indent(.verbose, 1),
            "Discarding previous tuning of %i modeling procedures.",
            sum(discard.tuning))
        for(i in which(discard.tuning))
            procedure[[i]]$tuning <- NULL
    }

    return(batch_model(if(multi.procedure) procedure else procedure[[1]], x, y, ...,
           .save=.save, .verbose=indent(.verbose, 1)))
}


#' Predict the response of unknown observations
#'
#' @param object Modeling procedure.
#' @param model Fitted model.
#' @param x Data set with observations whose response is to be predicted.
#' @param ... Sent to the procedure's prediction function.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return See the documentation of procedure's method.
#' @examples
#' procedure <- modeling_procedure("lda")
#' mod <- fit(procedure, x=iris[-5], y=iris$Species)
#' prediction <- predict(procedure, mod, iris[-5])
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{evaluate}},\code{\link{fit}}, \code{\link{tune}},
#'   \code{\link{importance}}
#' @export
predict.modeling_procedure <- function(object, model, x, ..., .verbose=TRUE){
    if(.verbose < 0){
        capture.output(suppressMessages(
            res <- object$predict_fun(object = model, x = x, ...)
        ))
        res
    } else {
        object$predict_fun(object = model, x = x, ...)
    }
}

#' Preserve debugging flags when manipulating lists
#'
#' \emph{In R version >= 3.1.0 these functions are no longer needed due to
#' changed behavior of the debugging facilities.}
#'
#' These functions are designed to copy and reset debugging flags when
#' manipulating modeling procedures or lists of such. See the code of
#' \code{\link{tune}}, e.g. by calling \code{page(tune)} for an example
#' of how to work with them.
#'
#' By default, when performing an operation on a list, in whole or in part, the
#' whole list will be overwritten by a new list containing the results of the
#' operation. This will discard debugging flags set by \code{\link{debug}},
#' which is not desirable when working with lists containing functions.
#'
#' @param procedure A modeling procedure to be wrapped in a one-element-list.
#' @param procedure.list A list of modeling procedures.
#' @param debug.flags A list of debugging flags, as produced by
#'   \code{\link{get_debug_flag}}
#' @return \code{list(x)} with debug flags maintained.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
listify <- function(procedure){
    debug.flags <- sapply(procedure, function(p) is.function(p) && isdebugged(p))
    procedure <- list(procedure)
    lapply(procedure[[1]][debug.flags], debug)
    procedure
}
# @rdname listify
#' @noRd
get_debug_flag <- function(procedure.list){
    lapply(procedure.list, sapply, function(p) is.function(p) && isdebugged(p))
}
# @rdname listify
#' @noRd
set_debug_flag <- function(procedure.list, debug.flags){
    if(as.integer(R.Version()$major) < 3 || as.numeric(R.Version()$minor) < 1){
        for(i in seq_along(debug.flags)){
            for(j in seq_along(debug.flags[[i]])){
                if(debug.flags[[i]][j]) debug(procedure.list[[i]][[j]])
            }
        }
    }
    procedure.list
}

