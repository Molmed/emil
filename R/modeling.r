#' Setup a modeling procedure
#'
#' A modeling procedure is an object containing all information necessary to
#' carry out and evaluate the performance of a predictive modeling task with
#' \code{\link{fit}}, \code{\link{tune}}, or \code{\link{evaluate}}.
#' To use an out-of-the box algorithm with default values, only the
#' \code{method} argument needs to be set. See \code{\link{emil}} for a
#' list of available methods. To deviate from the defaults, e.g. by tuning
#' parameters or using a custom function for model fitting, set the appropriate
#' parameters as described below.
#' For a guide on how to implement a custom method see the documentaion page
#' \code{\link{extension}}.
#' 
#' @param method The name of the modeling method. Only needed to identify
#'   plug-in functions, i.e. if you supply them yourself there is no need to
#'   set \code{method}.
#' @param parameter A list of model parameters. These will be fed to the fitting
#'   function after the dataset (\code{x} and \code{y} parameters). To tune a
#'   parameter, supply the candidate values in a vector or list.
#' 
#'   When tuning more than one parameter, all combinations of parameter values
#'   will be tested, if the elements of \code{parameter} are named. To manually
#'   specify which parameter value combinations to try, leave the the elements
#'   unnamed (see example 3 and 4).
#'   
#'   Parameters that should have vectors or lists as values, e.g. \code{trControl}
#'   when using
#'   \code{\link{fit_caret}} to train pkg{caret} models, must be wrapped in an
#'   additional list. That is, to set a parameter value to a list, but not tune it,
#'   make it a list of length 1 containing the list to be used (see example 6).
#' @param fit_fun The function to be used for model fitting.
#' @param predict_fun The function to be used for model prediction.
#' @param importance_fun The function to be used for calculating or extracting 
#'   feature importances. See \code{\link{get_importance}} for details.
#' @param error_fun Performance measure used to evaluate procedures
#'   and to tune parameters. See \code{\link{error_fun}} for details.
#' @return An object of class \code{modeling_procedure}.
#' @example examples/modeling_procedure.r
#' @seealso \code{\link{emil}}, \code{\link{evaluate}},
#'   \code{\link{fit}}, \code{\link{tune}},
#'   \code{\link[=predict.model]{predict}}, \code{\link{get_importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
modeling_procedure <- function(method, parameter=list(), error_fun=NULL, fit_fun, predict_fun, importance_fun){
    if(any(sapply(list(NA, FALSE), identical, parameter))){
        warning("`parameter` must be supplied as a list. Assuming you really want `list()` i.e. to not set any parameters.")
        parameter <- list()
    }
    if(!is.null(names(parameter))){
        parameter <- apply(do.call(expand.grid, lapply(parameter, seq_along)), 1, function(i){
            Map("[[", parameter, i)
        })
    }
    procedure <- structure(class = "modeling_procedure", .Data = list(
        method = if(missing(method)) NULL else method,
        parameter = if(length(parameter) == 0) list() else if(length(parameter) == 1) parameter[[1]] else NULL,
        tuning = if(length(parameter) < 2) NULL else list(parameter = parameter, error = NULL),
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
#' @method print modeling_procedure
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
   feature importance function:  %s
   individual error function:    %s
   
   number of parameter sets to tune over: %i
   tuned: %s\n",
        if(is.null(x$method)) "Custom" else paste0("`", x$method, "`"),
        yn(x$fit_fun), yn(x$predict_fun), yn(x$importance_fun), yn(x$error_fun),
        if(is.null(x$tuning$parameter)) 1 else length(x$tuning$parameter),
        if(is_tunable(x)) if(is_tuned(x)) "yes" else "no" else "not needed"
    ))
}

#' Coerce to modeling procedure
#' 
#' @param x modeling procedure.
#' @param ... Ignored (kept for S3 consistency).
#' @return Modeling procedure
#' @examples
#' as.modeling_procedure("lda")
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
as.modeling_procedure <- function(x, ...){
    UseMethod("as.modeling_procedure")
}
#' @method as.modeling_procedure default
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
#' @noRd
as.modeling_procedure.default <- function(x, ...){
    modeling_procedure(method = x, ...)
}
#' @method as.modeling_procedure character
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
#' @noRd
as.modeling_procedure.character <- function(x, ..., simplify=TRUE){
    if(length(x) == 1 && simplify){
        modeling_procedure(method = x)
    } else {
        if(is.null(names(x))) names(x) <- x
        lapply(x, modeling_procedure)
    }
}

#' Evaluate a modeling procedure
#' 
#' This function performs the important task of evaluating the performance of
#' a modeling procedure with resampling, including tuning and pre-processing
#' to not bias the results by information leakage.
#'
#' @param procedure Modeling procedure, or list of modeling procedures, as
#'   produced by \code{\link{modeling_procedure}}.
#' @param x Dataset, observations as rows and descriptors as columns.
#' @param y Response vector.
#' @param resample The test subsets used for parameter tuning. Leave blank to
#'   randomly generate a resampling scheme of the same kind as is used by
#'   \code{\link{evaluate}} to assess the performance of the whole
#'   modeling_procedure.
#' @param pre_process Function that performs pre-processing and splits dataset
#'   into fitting and test subsets.
#' @param .save What parts of the modeling results to return to the user. If
#'   \code{importance} is \code{FALSE} varible importance calculation will be
#'   skipped.
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
#' @param .verbose Whether to print an activity log.
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
#'       \item{\code{importance}}{Feature importance scores.}
#'       \item{\code{tune}}{Results from the parameter tuning. See
#'           \code{\link{tune}} for details.}
#'   }
#' @example examples/evaluate.r
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
evaluate <- function(procedure, x, y,
    resample=emil::resample("crossvalidation", y, nfold=2, nreplicate=2), pre_process=pre_split,
    .save=c(model=TRUE, prediction=TRUE, error=TRUE, importance=FALSE),
    .cores=1, .checkpoint_dir=NULL, .return_error=.cores > 1,
    .verbose=getOption("emil_verbose", TRUE)){

    log_message(.verbose, "Evaluating modeling performance...")

    if(is.list(procedure) && all(sapply(procedure, inherits, "modeling_procedure"))){
        multi.procedure <- TRUE
    } else if(inherits(procedure, "modeling_procedure")){
        procedure <- multify(procedure)
        multi.procedure <- FALSE
    } else {
        procedure <- as.modeling_procedure(procedure, simplify=FALSE)
        multi.procedure <- length(procedure) > 1
    }
    if(inherits(resample, "fold")){
        multi.fold <- FALSE
        resample <- data.frame(resample)
        names(resample) <- attr(resample[[1]], "fold.name")
    } else {
        multi.fold <- TRUE
    }
    make.na <- is.na(y) & !Reduce("&", lapply(resample, is.na))
    if(any(make.na)){
        log_message(indent(.verbose, 1), "%i observations will be excluded from the modeling due to missing values.", sum(make.na))
        resample[make.na,] <- NA
    }

    .save.default <- c(model=FALSE, prediction=TRUE, error=TRUE, importance=FALSE)
    for(s in names(.save)){
        .save.default[pmatch(s, names(.save.default))] <- .save[s]
    }
    .save <- .save.default
    rm(.save.default)

#------------------------------------------------------------------------------o
#   Make sure all plug-ins exist before we start crunching

    do.tuning <- !sapply(procedure, is_tuned)
    missing.fun <- unlist(Map(function(p, to.be.tuned) c(
        if(!is.function(p$fit_fun))
            sprintf("fit_%s", p$method)
        else NULL,
        if(!is.function(p$predict_fun) && (.save["prediction"] || to.be.tuned))
            sprintf("predict_%s", p$method)
        else NULL,
        if(!is.function(p$importance_fun) && .save["importance"])
            sprintf("importance_%s", p$method)
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
                rmse
            } else {
                stop("Unknown type of response vector, cannot guess performance measure. Please set `error_fun` manually.")
            }
        }
    }

#------------------------------------------------------------------------------o
#   Set up parallelization, error handling and checkpointing

    if(.Platform$OS.type == "windows" && .cores > 1){
        notify_once(id = "windows_parallelization",
                    "Parallelization is not yet implemented for windows systems. Please set it up manually as described in `?evaluate`. Running sequentially.",
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
                    log_message(indent(.verbose, 2), "An error occurred, skipping fold.")
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
        fold.message <- if(inherits(fold, "crossvalidation")){
            sub("^fold(\\d+).(\\d+)$", "Replicate \\1, fold \\2", fold.name)
        } else if(inherits(resample, "holdout")){
            sub("^fold(\\d+)$", "Fold \\1", fold.name)
        } else {
            fold.name
        }

        # Check for checkpoint files
        if(!is.null(checkpoint.file) && file.exists(checkpoint.file)){
            fold.message <- paste(fold.message, "already completed.")
            en <- new.env()
            load(checkpoint.file, envir=en)
            return(en$res)
        }
        log_message(indent(.verbose, 1), fold.message)

        # Disable further messages if run in parallel
        if(.cores > 1) .verbose <- 0

        # Do the work
        if(any(do.tuning)){
            # Tune before extracting the training and testing sets (below) to
            # not produce any unnecessary in-memory copies of the dataset.
            tune.subset <- subresample(fold, y = if(is.character(y)) x[[y]] else y)
            fold.procedure <- tune(procedure, x, y, resample=tune.subset,
                pre_process=pre_process, .save=NULL, .verbose=indent(.verbose, 2))
        } else {
            fold.procedure <- procedure
        }
        log_message(indent(.verbose, 2), "Extracting fitting and testing datasets.")
        if(is.function(pre_process)){
            sets <- pre_process(x, y, fold)
        } else if(is.list(pre_process)){
            sets <- pre_process[[1]](x, y, fold)
            if(length(pre_process) > 1) for(i in 2:length(pre_process)){
                sets <- pre_process[[i]](sets)
            }
            sets
        } else {
            stop("Invalid pre-processing.")
        }

        res <- lapply(seq_along(fold.procedure), function(i){
            # Rather than doing all models at once we do one at a time in case
            # they require a lot of memory. The [[-workaround is to keep the
            # procedure names printing in the correct way.
            model <- fit(fold.procedure[i], x=sets$fit$x, y=sets$fit$y,
                         .verbose=indent(.verbose, 2))[[1]]
            prediction <- predict(model, sets$test$x, .verbose=indent(.verbose, 1))
            list(error = if(.save["error"]){
                    fold.procedure[[i]]$error_fun(sets$test$y, prediction$prediction)
                 } else NULL,
                 model = if(.save["model"]) model else NULL,
                 prediction = if(.save["prediction"]) prediction else NULL,
                 importance = if(.save["importance"]) get_importance(model)
             )
        })
        names(res) <- names(fold.procedure)

        # Estimate run time
        if(.cores == 1 && is.null(checkpoint.file)){
            if(.verbose && counter == 1 && is.data.frame(resample) && ncol(resample) > 1){
                # Fold result size
                os <- object.size(res)
                os.i <- trunc(log(os)/log(1024))
                if(os.i == 0){
                    log_message(indent(.verbose, 1), "Result size is %i B.", os, time=FALSE)
                } else {
                    log_message(indent(.verbose, 1), "Result size is %.2f %s.",
                        exp(log(os) - os.i * log(1024)), c("KiB", "MiB", "GiB", "TiB", "PiB", "EiB")[os.i],
                        time=FALSE)
                }
                # Estimated completion time
                t2 <- t1 + difftime(Sys.time(), t1, units="sec")*ncol(resample)
                fmt <- if(difftime(t2, t1, units="days") < 1){
                    "%H:%M"
                } else if(difftime(t2, t1, units="days") < 2 &&
                          # FIXME!!!
                          as.integer(strftime(t2, "%prediction")) -
                          as.integer(strftime(t1, "%prediction")) == 1){
                    "%H:%M tomorrow"
                } else if(difftime(t2, t1, units="days") < 365){
                    "%H:%M, %b %prediction"
                } else {
                    "%H:%M, %b %prediction, %Y"
                }
                log_message(indent(.verbose, 1),
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
#' @param x modeling result object, as produced by \code{\link{evaluate}}.
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
#' untuned parameters they will automatically be tuned prior to fitting.
#' 
#' @param procedure Modeling procedure, or list of modeling procedures, as
#'   produced by \code{\link{modeling_procedure}}.
#' @param x Dataset, observations as rows and descriptors as columns.
#' @param y Response vector.
#' @param ... Sent to \code{\link{tune}}, in case tuning is required,
#'   which will pass them on to \code{\link{evaluate}}.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A list of fitted models.
#' @examples
#' mod <- fit("lda", x=iris[-5], y=iris$Species)
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{evaluate}}, \code{\link{tune}},
#'   \code{\link[=predict.model]{predict}}, \code{\link{get_importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
fit <- function(procedure, x, y, ..., .verbose=getOption("emil_verbose", FALSE)){
    log_message(.verbose, "Fitting models...")
    if(is.list(procedure) && all(sapply(procedure, inherits, "modeling_procedure"))){
        multi.procedure <- TRUE
    } else if(inherits(procedure, "modeling_procedure")){
        procedure <- multify(procedure)
        multi.procedure <- FALSE
    } else {
        procedure <- as.modeling_procedure(procedure, simplify=FALSE)
        multi.procedure <- length(procedure) > 1
    }

    missing.fun <- unlist(lapply(procedure, function(p)
        if(!is.function(p$fit_fun)) sprintf("fit_%s", p$method) else NULL))
    if(!is.null(missing.fun))
        stop(sprintf("Plug-in%s function %s not found.",
            if(length(missing.fun) > 1) "s" else "",
            paste("`", missing.fun, "`", sep="", collapse=", ")))

    need.tuning <- !sapply(procedure, is_tuned)
    if(any(need.tuning)){
        procedure[need.tuning] <- tune(procedure[need.tuning], x=x, y=y, ...,
            .verbose=indent(.verbose, 1))
    }
    res <- Map(function(p, mn){
        log_message(indent(.verbose, 1), "Fitting %s", mn)
        if(".verbose" %in% names(formals(p$fit_fun)))
            p$parameter$.verbose <- indent(.verbose, 1)
        model <- do.call(function(...) p$fit_fun(x=x, y=y, ...), p$parameter)
        structure(list(fit = model, procedure = p), class="model")
    }, procedure, names(procedure))
    # Restore debug flags (for some reason Map removes them)
    for(i in seq_along(procedure))
        debug_flag(res[[i]]$procedure) <- debug_flag(procedure[[i]])
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
#' @param ... Sent to \code{\link{evaluate}}.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   suppress all messages.
#' @return A tuned modeling procedures or a list of such.
#' @examples
#' procedure <- modeling_procedure("randomForest", parameter=list(mtry=1:4))
#' tuned.procedure <- tune(procedure, x=iris[-5], y=iris$Species)
#' mod <- fit(tuned.procedure, x=iris[-5], y=iris$Species)
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{evaluate}}, \code{\link{fit}},
#'   \code{\link[=predict.model]{predict}}, \code{\link{get_importance}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
tune <- function(procedure, ..., .verbose=getOption("emil_verbose", FALSE)){
    log_message(.verbose, "Tuning parameters...")
    if(is.list(procedure) && all(sapply(procedure, inherits, "modeling_procedure"))){
        multi.procedure <- TRUE
    } else if(inherits(procedure, "modeling_procedure")){
        procedure <- multify(procedure)
        multi.procedure <- FALSE
    } else {
        procedure <- as.modeling_procedure(procedure, simplify=FALSE)
        multi.procedure <- length(procedure) > 1
    }

    do.tuning <- sapply(procedure, is_tunable)
    discard.tuning <- do.tuning & sapply(procedure, is_tuned)
    for(i in which(discard.tuning))
        procedure[[i]]$tuning <- NULL
    tune.procedure <- lapply(procedure[do.tuning], function(p){
        lapply(p$tuning$parameter, function(pp){
            p$parameter <- pp
            p["tuning"] <- list(NULL)
            p
        })
    })
    tune.name <- rep(names(tune.procedure), sapply(tune.procedure, length))
    tune.procedure <- unlist(tune.procedure, recursive=FALSE)
    names(tune.procedure) <- ave(tune.name, tune.name, FUN=function(x){
        if(length(x) == 1) x else sprintf("%s (%i)", x, seq_along(x))
    })

    tuning <- evaluate(tune.procedure, ..., .verbose=indent(.verbose, 1))
    procedure.id <- rep(which(do.tuning),
                   sapply(procedure[do.tuning], function(p) length(p$tuning$parameter)))
    for(i in which(do.tuning)){
        procedure[[i]]$tuning$error <-
            sapply(tuning, function(tun) unlist(subtree(tun, procedure.id == i, "error")))
        rownames(procedure[[i]]$tuning$error) <- NULL
        mean.err <- apply(procedure[[i]]$tuning$error, 1, mean)
        best.parameter <- which(mean.err == min(mean.err))
        if(length(best.parameter) > 1) best.parameter <- sample(best.parameter, 1)
        procedure[[i]]$parameter <- procedure[[i]]$tuning$parameter[[best.parameter]]
        procedure[[i]]$tuning$result <- lapply(tuning, "[", procedure.id == i)
    }
    if(multi.procedure) procedure else procedure[[1]]
}
#' @rdname tune
#' @return Logical indicating if the procedure(s) are tuned.
#' @export
is_tuned <- function(procedure)
    !is_tunable(procedure) || !is.null(procedure$parameter)
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


#' Predict the response of unknown observations
#'
#' @method predict model
#' @param object Fitted model.
#' @param x Data set with observations whose response is to be predicted.
#' @param ... Sent to the procedure's prediction function.
#' @param .verbose Whether to print an activity log.
#' @return See the documentation of procedure's method.
#' @examples
#' mod <- fit("lda", x=iris[-5], y=iris$Species)
#' prediction <- predict(mod, iris[-5])
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{modeling_procedure}},
#'   \code{\link{evaluate}},\code{\link{fit}}, \code{\link{tune}},
#'   \code{\link{get_importance}}
#' @export
predict.model <- function(object, x, ..., .verbose=FALSE){
    if(".verbose" %in% names(formals(object$procedure$predict_fun))){
        p <- object$procedure$predict_fun(object = object$fit, x = x, ..., .verbose=.verbose)
    } else {
        p <- object$procedure$predict_fun(object = object$fit, x = x, ...)
    }
    structure(p, class="prediction")
}

#' Get debug flags of an object
#'
#' Normally you don't need this function when working with modeling procedures,
#' but in some special cases the flags are lost and need to be restored.
#'
#' @param x A complex object contianing functions.
#' @return A list of the same structure as \code{x} containing logicals
#'  indicating if each object is a function under debug.
#' @examples
#' m1 <- modeling_procedure("randomForest")
#' m2 <- modeling_procedure("pamr")
#' debug(m1$fit_fun)
#' isdebugged(m2$fit_fun)
#' debug_flag(m2) <- debug_flag(m1)
#' isdebugged(m2$fit_fun)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
debug_flag <- function(x){
    if(inherits(x, "modeling_procedure")){
        sapply(x, function(p) is.function(p) && isdebugged(p))
    } else if(is.list(x)){
        lapply(x, debug_flag)
    } else {
        stop("Invalid procedure.")
    }
}
#' @param value A list of logicals to be used to set debug flags.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
`debug_flag<-` <- function(x, value){
    if(inherits(x, "modeling_procedure") && is.logical(value)){
        for(f in names(which(value))) debug(x[[f]])
    } else if(is.list(x) && is.list(value)){
        for(i in seq_along(x)) debug_flag(x[[i]]) <- value[[i]]
    } else {
        stop("Object and debug flags doesn't match.")
    }
    x
}

#' Wrap a modeling procedure in a named list
#' 
#' The functions \code{\link{fit}}, \code{\link{tune}}, and
#' \code{\link{evaluate}} all work on lists of procedures rather than individual
#' procedures. This functions wraps a single procedure in a list with correct 
#' naming for pretty output.
#' 
#' @param procedure Modeling procedure, as returned by
#'   \code{\link{modeling_procedure}}.
#' @return A named list of modeling procedures.
#' @noRd
#' @author Christofer \enc{Bäcklin}{Backlin}
multify <- function(procedure){
    stopifnot(inherits(procedure, "modeling_procedure"))
    procedure <- list(procedure)
    names(procedure) <- if(!is.null(procedure[[1]]$method)){
        procedure[[1]]$method
    } else {
        "model"
    }
    procedure
}

