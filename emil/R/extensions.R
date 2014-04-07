##' Extending the emil framework with user-defined methods
##' 
##' This page describes how to implement custom methods compatible with the
##' functions of the emil package, most notably \code{\link{fit}},
##' \code{\link{tune}}, and \code{\link{evaluate.modeling}}. Pre-processing and
##' resampling is not covered here, but in the entries \code{\link{pre.process}}
##' and \code{\link{resample}}.
##' 
##' \bold{Fitting models}
##' 
##' To write and use custom model fitting functions with the emil framework,
##' it must take the the following inputs
##' 
##'     \code{function(x, y, ...)}
##' 
##' \describe{
##'     \item{\code{x}}{The descriptors (or variables) of the observations you
##'         want to train the model on. This is typically a matrix or data frame
##'         where each row correspons to an observation. In case it is more
##'         natural to chraracterize your observations some other way, maybe as
##'         character vectors of varying length for some document classification
##'         method,  \code{x} can be of any form you like as long as the fitting
##'         function knows how to handle it. In that case you will also need
##'         supply you own pre-processing function
##'         (see \code{\link{pre.process}} that can extract training and test
##'         sets from the entire data set.
##'     
##'         See the functions \code{\link{pre.pamr}} and \code{\link{fit.pamr}}
##'         for an example of a function that does not take its descriptors in
##'         the default way.
##'     }
##'     \item{\code{y}}{A repsonse vector. This is a the outcome you want to
##'         model, e.g. the variable of interest in a regression, class label in
##'         a classification problem, or anything else that a fitted model will
##'         produce when given data to make predictions from.}
##'     \item{\code{...}}{Model parameters. These will all be tunable with the
##'         \code{\link{tune}} and \code{\link{evaluate.modeling}} functions.}
##' }
##' 
##' The function must return everything necessary to make future predictions,
##' but it can take any form you like. In the simplest case it is just a number
##' of fitted parameter values, like in a least squares regression, but it could
##' also be some big and complex structure holding an ensemble of multiple
##' submodels.
##' 
##' \bold{Making predictions}
##' 
##' Once a model is fitted it can be used to make predictions with a prediction
##' function, defined as such
##' 
##'     \code{function(object, x, ...)}
##' 
##' \describe{
##'     \item{\code{object}}{A fitted model produced by the model fitting
##'         function described above.}
##'     \item{\code{x}}{Descriptors of observations to make predictions on.}
##'     \item{\code{...}}{Parameters to the prediction functions. These are
##'         ignored by \code{\link{tune}} and \code{\link{evaluate.modeling}},
##'         but could be convenient if the user wants to work with it manually.}
##' }
##' 
##' The output of the prediction function must be an object that can be compared
##' to the true response, by an error function (see below). It is typically a
##' list with elements named \code{"pred"} for "predictions" or "risk" for
##' estimated risks. It can also be on an arbitrary form as long as a compatible
##' error function is used.
##' 
##' \bold{Calculating performance}
##' 
##' To quantify the performance of a model an error function is used. It takes
##' two arguments and produce a number, the lower the better.
##' 
##'     \code{function(true, pred)}
##' 
##' \describe{
##'     \item{\code{true}}{A vector of true responses.}
##'     \item{\code{pred}}{Prediction returned from the predction function.}
##' }
##' 
##' In most cases the true response and the predctions are of the same type,
##' e.g. true and fitted values in a regression or class labels in a 
##' classification problem, but it is not a requirement. An example of different
##' types could be if the prediction function produce class probabilities for all
##' classes rather than one label, or the risks that the observations will
##' experience the event of interest, to be compared to the actual outcome that
##' it did occur or has not yet occured at a specific time point. See
##' \code{\link{neg.harrell.C}} for an exmple of the latter, and
##' \code{\link{error.fun}} for an overview of the error functions provided in
##' the package.
##' 
##' \bold{Calculating variable importance scores}
##' 
##' Estimating the importance of each descriptor (or variable) can often be as
##' important as making predictions. Functions for calculating or extracting
##' variable imporance scores from fitted models should be defined as follows:
##' 
##'     \code{function(object, ...)}
##' 
##' \describe{
##'     \item{\code{object}}{A fitted model produced by the model fitting
##'         function described above.}
##'     \item{\code{...}}{Parameters to the prediction functions. These are
##'         ignored by \code{\link{tune}} and \code{\link{evaluate.modeling}},
##'         but could be convenient if the user wants to work with it manually.}
##' }
##' 
##' The function should return a vector of length p or a p-by-c data frame where
##' p is the number of descriptors in the data set and c is the number of
##' classes.
##' 
##' @seealso error.fun, pre.process, resample
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name emil.extensions
{}

