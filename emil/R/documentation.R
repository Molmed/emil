##' Introduction to the emil package
##'
##' The emil package implements a framework for working with predictive modeling
##' problems without information leakage. For an overview of its functionality
##' please read the original publication (\enc{Bäcklin}{Backlin} 2014).
##' 
##' \section{Central topics and functions}{
##'     
##'     \subsection{Setting up modeling problems}{
##'         \describe{
##'             \item{\code{\link{resample}}}{Functions for generating and implementing
##'                 resampling schemes.}
##'             \item{\code{\link{pre.process}}}{Data pre-processing functions.}
##'             \item{\code{\link{modeling.procedure}}}{Manages algorithms used for
##'                 fitting models, making predictions, and extracting variable
##'                 importance scores.}
##'             \item{\code{\link{error.fun}}}{Performance estimation functions
##'                 used to tune variables and evaluate performance of modeling
##'                 procedures.}
##'         }
##'     }
##' 
##'     \subsection{Solving modeling problems}{
##'         \describe{
##'             \item{\code{\link{fit}}}{Fit a model (according to a procedure).}
##'             \item{\code{\link{tune}}}{Tune parameters of a procedure.}
##'             \item{\code{\link{evaluate.modeling}}}{Evaluate the performance of a
##'                 procedure using resampling.}
##'             \item{\code{\link{batch.model}}}{A semi-internal function that carries
##'                 out most of the work. Parameters passed along from the above
##'                 functions to this function are documented here.}
##'         }
##'     }
##' 
##'     \subsection{Managing the results of modeling problems}{
##'         \describe{
##'             \item{\code{\link{subtree}}}{Extracts results from the output of
##'                 \code{\link{evaluate.modeling}}. It is essentially a recursive
##'                 version of \code{\link{lapply}} and \code{\link{sapply}}.}
##'             \item{\code{\link{subframe}}}{Extracts results and organize them per
##'                 observation. Useful when comparing predcitions and properties of 
##'                 individual observations accross folds of a resampling scheme.}
##'         }
##'         No functions for plotting is included in the current version of the package,
##'         except for \code{\link{image.reample}}.
##'     }
##' }
##' 
##' \section{Methods included in the package}{
##' 
##'     \subsection{Resampling methods}{
##'         See \code{\link{resample}} for information on usage and
##'         implementation of custom methods.
##'         \describe{
##'             \item{\code{\link{resample.holdout}}}{Repeated holdout.}
##'             \item{\code{\link{resample.crossval}}}{Cross validation.}
##'         }
##'     }
##' 
##'     \subsection{Data pre-processing methods}{
##'         See \code{\link{pre.process}} for information on usage and
##'         implementation of custom methods.
##'         The imputation functions can also be used outside of the resampling
##'         scheme, see \code{\link{impute}}.
##'         \describe{
##'             \item{\code{\link{pre.split}}}{Only split, no transformation.}
##'             \item{\code{\link{pre.center}}}{Center data to have mean 0 of each
##'                 feature.}
##'             \item{\code{\link{pre.scale}}}{Center and scale data to have mean 0
##'                 and standard deviation 1. }
##'             \item{\code{\link{pre.impute.median}}}{Impute missing values with
##'                 feature medians.}
##'             \item{\code{\link{pre.impute.knn}}}{Impute missing values with k-NN,
##'                 see \code{\link{pre.impute.knn}} for details on how to set
##'                 parameters.}
##'         }
##'     }
##' 
##'     \subsection{Modeling methods}{
##'         See \code{\link{modeling.procedure}} for information on usage and
##'         \code{\link{emil.extensions}} for information on implementation of
##'         custom methods.
##'         \describe{
##'             \item{\code{\link[=emil.fit.cforest]{cforest}}}{Conditional inference forest.}
##'             \item{\code{\link[=emil.fit.glmnet]{glmnet}}}{Elastic net.}
##'             \item{\code{\link[=emil.fit.lda]{lda}}}{Linear discriminant.}
##'             \item{\code{\link[=emil.fit.lm]{lm}}}{Linear model.}
##'             \item{\code{\link[=emil.fit.pamr]{pamr}}}{Nearest shrunken centroids.}
##'             \item{\code{\link[=emil.fit.qda]{qda}}}{Quadratic discriminant.}
##'             \item{\code{\link[=emil.fit.randomForest]{randomForest}}}{Random forest.}
##'         }
##'     }
##'     
##'     \subsection{Performance estimation methods}{
##'         See \code{\link{error.fun}} for information on usage and
##'         implementation of custom methods.
##' 
##'         For classification problems:
##'         \describe{
##'           \item{\code{error.rate}}{Fraction of predictions that were incorrect.}
##'           \item{\code{weighted.error.rate}}{See its own documentation.}
##'           \item{\code{neg.auc}}{Negative area under ROC curve.}
##'           \item{\code{\link{neg.gmpa}}}{Negative geometric mean of class-specific
##'             prediction accuracy. Good for problems with imbalanced class sizes.}
##'         }
##'         For regression problems:
##'         \describe{
##'             \item{\code{mse}}{Mean square error.}
##'             \item{\code{rmse}}{Root mean square error.}
##'         }
##'         For survival analysis problem:
##'         \describe{
##'             \item{\code{neg.harrell.C}}{Negative Harrell's concordance index.}
##'         }
##'     }
##' 
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name emil
{}


##' Extending the emil framework with user-defined methods
##' 
##' This page describes how to implement custom methods compatible with the
##' functions of the emil framework, most notably \code{\link{fit}},
##' \code{\link{tune}}, and \code{\link{evaluate.modeling}}. Pre-processing and
##' resampling is not covered here, but in the entries \code{\link{pre.process}}
##' and \code{\link{resample}}.
##' 
##' \section{Fitting models}{
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
##'         See the functions \code{\link{pre.pamr}} and \code{\link{emil.fit.pamr}}
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
##' }
##' 
##' \section{Making predictions}{
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
##' }
##' 
##' \section{Calculating performance}{
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
##' }
##' 
##' \section{Calculating variable importance scores}{
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
##' }
##' 
##' @seealso error.fun, pre.process, resample
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name emil.extensions
{}

