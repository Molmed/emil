##' Get a desired plug-in function
##'
##' @param fun.name Plug-in name, e.g. "design.nsc".
##' @param location The location to search for the function at. Can be specified
##'   as:
##'   \describe{
##'     \item{\code{NA}}{First look in the global environment, then the regular
##'       package search path.}
##'     \item{\code{"package:x"}}{Only look among the functions exported from
##'       package \emph{x}.}
##'     \item{\code{"namespace:x"}}{Only look in the namespace of package
##'       \emph{x}}
##'   }
##' @return A function.
##' @author Christofer \enc{Bäcklin}{Backlin}
get.plugin <- function(fun.name, location){
    if(grepl("^namespace:", location)){
        # The user has specified what namespace to get the function from
        getFromNamespace(fun.name, sub("^namespace:", "", location))
    } else if(grepl("^package:", location)){
        # The user has specified what package to get the function from
        getExportedValue(sub("^package:", "", location), fun.name)
    } else if(fun.name %in% ls(envir=globalenv())){
        # The function has been defined (or overlayered) in the global environment
        get(fun.name, envir=globalenv())
    } else {
        # Follow the standard search path
        get(fun.name)
    }
}


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
##'   sets parameters (e.g. \code{list(rf=list(mtry=5000))}).
##' 
##'   To tune a parameter specify its possible values as in a list, e.g.
##'   \code{list(rf=list(mtry=list(500, 1000, 2000), ntree=list(5000)))}.
##' 
##'   If more than one parameter is to be tuned all combinations of parameter
##'   values will be tested. If only specifc combinations should be tested they
##'   should be supplied as individual lists on this form:
##'   \code{list(rf=list(list(mtry=500, ntree=100), list(mtry=2000, ntree=50)))}
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
##' @param save.fit Whether to include the designed models in the returned
##'   results. Note that the size of these depend on the model type and could
##'   potentially be very large.
##' @param save.predict Whether to make and return predictions. If
##'   \code{FALSE} only fitted models are returned.
##' @param save.vimp Whether to calculate and return variable importance
##'   scores.
##' @param .verbose Whether to print status messages.
##' @examples
##' x <- sweep(matrix(rnorm(60*10), 60), 1, rep(c(0,.8), each=30))
##' y <- gl(2,30)
##' pred <- batch.predict(x, y, "lda", resample.crossval(y, nfold=3, nrep=3))
##'
##' \dontrun{
##' models <- list(lda=list(pi=list(c(.25, .75), c(.5, .5))), lda=list())
##' pred <- batch.predict(x, y, models, resample.crossval(y, nfold=3, nrep=3))}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
batch.predict <- function(x, y, models, test.subset, error.fun, pre.trans=pre.split,
                   save.fit = FALSE, save.predict = TRUE, save.vimp = FALSE,
                   .verbose=FALSE){


#---------------------------------------------------------------[ Initial test ]
#   Taking care of the most obvious user misstakes.

    if(nrow(x) != length(y))
        stop("x and y does not match.")
    if(!missing(test.subset) && nrow(x) != nrow(test.subset))
        stop("x and test.subset does not match.")
    na.idx <- Reduce("|", lapply(test.subset, function(idx) is.na(y) & !is.na(idx)))
    if(any(na.idx)){
        warning("Response vector `y` contains NA values that are not NA in `test.subset`. These will not be used for neither design nor test.")
        test.subset[na.idx,] <- NA
    }
    if(!save.fit && !save.predict && !save.vimp)
        stop("Current call would not return any results.")

    msg <- if(.verbose) trace.msg else function(...) invisible()
    msg(1, "Initializing analysis.")


#------------------------------------------[ Set up model definiton and tuning ]

    if(is.character(models))
        models <- structure(vector("list", length(models)), names=models)
    out <- list(models = lapply(models, function(m){
        if(is.null(m)){
            list(list())
        } else if(is.null(names(m))){
            # Every element correspond to a complete parameter setup
            m
        } else {
            # Every element correspond to one parameter, return all combinations
            apply(do.call(expand.grid, lapply(m, seq_along)), 1, function(i){
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


#-------------------------------------------------------------[ Fetch plug-ins ]

    types <- unique(names(out$models))
    locations <- ifelse(grepl(":::", types),
        sprintf("namespace:%s", sub(":.*", "", types)),
        ifelse(grepl("::", types),
            sprintf("package:%s", sub(":.*", "", types)),
            NA))
    names(locations) <- types
    suppressWarnings(
        missing.packages <- !sapply(na.omit(sub(".*:", "", locations)),
                                    require, character.only=TRUE))
    if(any(missing.packages))
        stop(sprintf("Could not load package%s %s",
            if(sum(missing.packages) > 1) "s" else "",
            paste("`", names(missing.packages), "`", sep="", collapse=", ")))
    fun.names <- lapply(types, function(type)
        sprintf("%s.%s",
            c("design", 
                if(save.predict || do.tuning[type]) "predict" else NULL,
                if(save.vimp) "vimp" else NULL),
            sub(".*:", "", type)))
    plugin <- mapply(function(fun, loc)
        sapply(fun, function(x) tryCatch(get.plugin(x, loc), error=function(...) NULL)),
        fun.names, locations, SIMPLIFY=FALSE)
    names(plugin) <- types
    missing.plugins <- unlist(lapply(plugin, lapply, is.null))
    if(any(missing.plugins))
        stop(sprintf("Could not find plug-in%s %s",
            if(sum(missing.plugins) > 1) "s" else "",
            paste("`", unlist(fun.names)[missing.plugins], "`", sep="", collapse=", ")))

    # Output a summary of what plug-ins will be used
    for(i in seq_along(plugin)){
        msg(2, "Plug-ins for model `%s`:", types[i], time = FALSE)
        msg(3, "Using `%s` from %s",
            fun.names[[i]],
            sapply(plugin[[i]], function(x) capture.output(environment(x))),
            time = FALSE)
    }

    # This elaborate construction allows us to call an arbitrary design function
    # with a dataset and a list of parameters without combining them in a single
    # list (i.e. `do.call(fun, c(list(data), params))`), which would result in
    # an unnecessary copy of the dataset in the memory.
    #
    # @param fun Function to call.
    # @param x Dataset.
    # @param y Response vector.
    # @param param Model parameters.
    # @param type Model name. Only used for setting the class.
    do.design.call <- function(fun, x, y, param, type){
        fit <- do.call(function(...) fun(x=x, y=y, ...), param)
        structure(fit, class=c(sub(".*:", "", type), "classifier", class(fit)))
    }


#----------------------------------------------------------[ Perform modelling ]

    counter <- 0
    out$cv <- lapply(test.subset, function(fold){
        counter <<- counter + 1
        if(inherits(test.subset, "crossval")){
            msg(1, sub("^fold(\\d):(\\d)$", "Replicate \\1, fold \\2", colnames(test.subset)[counter]))
        } else if(inherits(test.subset, "holdout")){
            msg(1, sub("^fold(\\d)$", "Fold \\2", colnames(test.subset)[counter]))
        } else {
            msg(1, colnames(test.subset)[counter])
        }
        if(any(do.tuning)){
            msg(2, "Tuning %s", paste(sprintf("%s(%i)", names(do.tuning[do.tuning]),
                sapply(out$models[do.tuning], length)), collapse=", "))
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
                sets <- pre.trans(x, y, tuning.fold)
                lapply(which(do.tuning), function(i){
                    sapply(out$models[[i]], function(my.param){
                        error.fun(
                            y[na.fill(tuning.fold, FALSE)],
                            plugin[[names(models)[i]]]$predict(
                                do.design.call(
                                    plugin[[names(out$models)[i]]]$design,
                                    sets$design,
                                    y[na.fill(!tuning.fold, FALSE)],
                                    my.param,
                                    names(out$models)[i]),
                                sets$test))
                    })
                })
            })
            tuning.err <- lapply(seq_along(do.tuning), function(i)
                if(do.tuning[i]) ssubtree(tuning, T, i) else NULL)
            names(tuning.err) <- names(do.tuning)
            my.param <- mapply(function(p, e){
                if(is.blank(e)){
                    p[[1]]
                } else {
                    e <- apply(e, 1, mean)
                    p[[order(e, runif(length(e)))[1]]]
                }
            }, out$models, tuning.err, SIMPLIFY=FALSE)
        } else {
            my.param <- lapply(out$models, "[[", 1)
        }
        msg(2, "Extracting and preprocessing design and test sets")
        sets <- pre.trans(x, y, fold)
        res <- lapply(seq_along(out$models), function(i){
            msg(2, "Fitting and evaluating %s", names(out$models)[i])
            fit <- do.design.call(
                plugin[[names(models)[i]]]$design,
                sets$design,
                y[na.fill(!fold, FALSE)],
                my.param[[i]],
                names(out$models)[i])
            pred <- plugin[[names(models)[i]]]$predict(fit, sets$test)
            err <- error.fun(y[na.fill(fold, FALSE)], pred)
            if(save.vimp){
                if("features" %in% names(sets)){
                    temp.vimp <- plugin[[names(models)[i]]]$vimp(fit)
                    temp.map <- cumsum(sets$features)
                    temp.map[!sets$features] <- NA
                    if(is.data.frame(temp.vimp) || is.matrix(temp.vimp)){
                        my.vimp <- list(vimp=temp.vimp[temp.map,,drop=FALSE])
                    } else if(is.vector(temp.vimp)){
                        my.vimp <- list(vimp=temp.vimp[temp.map])
                    }
                    rm(temp.vimp, temp.map)
                } else {
                    my.vimp <- list(vimp=plugin[[names(models)[i]]]$vimp(fit))
                }
            } else my.vimp <- NULL

            c(pred,
              list(error=err),
              if(save.fit) list(fit=fit) else NULL,
              my.vimp,
              if(do.tuning[i]) list(param=my.param[[i]], tuning.err=tuning.err[[i]]) else NULL)
        })
        names(res) <- names(out$models)
        if(counter == 1 && .verbose){
            os <- object.size(res)
            os.i <- trunc(log(os)/log(1024))
            msg(3, "Fold result size is %.2f %s",
                exp(log(os) - os.i * log(1024)), c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB")[os.i + 1],
                time=FALSE)
        }
        res
    })
    msg(1, "Done")
    out
}


##' Design a single classifier
##' 
##' This is a general wrapper that fits a prediction model and adds the classes
##' required for it to be recognized by the package's other functions.
##' 
##' @param type Classifier type, e.g. "lda" for linear discriminant.
##' @param x Dataset.
##' @param y Response vector.
##' @param ... Sent to type the specific wrapper, e.g. \code{\link{design.lda}}.
##' @param .verbose Whether to print a status message on what plug-in was
##'   used.
##' @return A fitted model.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
design <- function(type, x, y, ..., .verbose=FALSE){
    location <- if(grepl(":::", type)){
        sprintf("namespace:%s", sub(":.*", "", type))
    } else if(grepl("::", type)){
        sprintf("package:%s", sub(":.*", "", type))
    } else {
        NA
    }
    fun <- get.plugin(fun.name, location)
    if(.verbose) cat(sprintf("Using `%s%` from %s\n",
        fun.name, capture.output(environment(fun))))
    fit <- fun(x=x, y=y, ...)
    structure(fit, class=c(sub(".*:", "", type), "classifier", class(fit)))
}

