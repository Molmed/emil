##' Setup a design procedure
##'
##' A design procedure is an object containing all information necessary to
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
##'   parameter, supply the candidate values in a list.
##' 
##'   When tuning more than one parameter, all combinations of parameter values
##'   will be tested, if the elements of \code{param} are named. To manually
##'   specify which parameter value combinations to try, leave the the elements
##'   unnamed (see example 3 and 4).
##'   
##'   Parameters that should have lists as values, e.g. \code{trControl} when using
##'   \code{\link{design.caret}} to train caret models, must be wrapped in an
##'   additional list. That is, to set a parameter value to a list, but not tune it,
##'   make it a list of length 1 containing the list to be used (see example 6).
##' @param design The function to be used for model fitting.
##' @param predict The function to be used for model prediction.
##' @param vimp The function to be used for calculating or extracting variable
##'   importance scores.
##' @param test.subset The test subsets used for parameter tuning. Leave blank to
##'   randomly generate a resampling scheme of the same kind as is used by
##'   \code{\link{batch.predict}} to assess the performance of the whole design
##'   procedure.
##' @param error.fun Performance measure. Only set this if you wish to carry out
##'   modelling with multiple design procedures with different performance measures
##'   in the same call to \code{\link{batch.predict}}. An example of such a
##'   situation is if you wish to design both regression and classification models.
##' @return A list of functions and attributes that define a design procedure.
##' @examples
##' # 1: Design linear discriminants without tuning any parameter,
##' # since it has none
##' design.procedure("lda")
##' 
##' # 2: Tune random forest's `mtry` parameter, with 3 possible values
##' design.procedure("rf", list(mtry = list(100, 250, 1000)))
##' 
##' # 3: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # with 3 values each, testing all 9 possible combinations
##' design.procedure("rf", list(mtry = list(100, 250, 1000),
##'                             maxnodes = list(5, 10, 25)))
##' 
##' # 4: Tune random forest's `mtry` and `maxnodes` parameters simultaneously,
##' # but only test 3 manually specified combinations of the two
##' design.procedure("rf", list(list(mtry = 100, maxnodes = 5),
##'                             list(mtry = 250, maxnodes = 10),
##'                             list(mtry = 1000, maxnodes = 25)))
##' 
##' # 5: Tune elastic net's `alpha` and `lambda` parameters. Since elastic net's
##' # design function can tune `lambda` internally in a more efficient way
##' # than the general framework is able to do, only tune `alpha` and pass all
##' # `lambda` values as a single argument.
##' design.procedure("glmnet", list(alpha = as.list(seq(0, 1, length.out=6)),
##'                                 lambda = seq(0, 5, length.out=30)))
##' 
##' # 6: Train elastic nets using the caret package's model fitting framework
##' design.procedure("caret", list(method = "glmnet",
##'     trControl = list(trainControl(verboseIter = TRUE, classProbs = TRUE))))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
design.procedure <- function(method, param, design, predict, vimp, test.subset=NULL, error.fun=NULL){
    if(!is.list(param))
        stop("Model parameters must be supplied as a list.")

    structure(class = "design.procedure", .Data = list(
        method = method,
        param =
            if(is.null(names(param))){
                param
            } else {
                apply(do.call(expand.grid, lapply(param, seq_along)), 1, function(i){
                    mapply("[[", param, i, SIMPLIFY=FALSE)
                })
            },
        design =
            if(missing(design)){
                tryCatch(get(sprintf("design.%s", method)), error=function(err) err)
            } else design,
        predict =
            if(missing(predict)){
                tryCatch(get(sprintf("predict.%s", method)), error=function(err) err)
            } else predict,
        vimp =
            if(missing(vimp)){
                tryCatch(get(sprintf("vimp.%s", method)), error=function(err) err)
            } else vimp,
        test.subset = test.subset,
        error.fun = NULL
    ))
}

