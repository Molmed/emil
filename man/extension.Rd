\name{extension}
\alias{extension}
\title{Extending the emil framework with user-defined methods}
\description{
    This page describes how to implement custom methods
    compatible with the functions of the emil framework, most
    notably \code{\link{fit}}, \code{\link{tune}}, and
    \code{\link{evaluate}}. Pre-processing and
    resampling is not covered here, but in the entries
    \code{\link{pre_process}} and \code{\link{resample}}.
}
\section{Fitting models}{

    To write and use custom model fitting functions with the
    emil framework, it must take the the following inputs.
    Optional

    \code{function(x, y, p1, p2, p3, ..., .verbose)}
    \describe{
        \item{\code{x}}{
            The features (or variables)
            of the observations you want to train the model on. This is
            typically a matrix or data frame where each row corresponds
            to an observation. In case it is more natural to
            characterize your observations some other way, maybe as
            character vectors of varying length for some document
            classification method, \code{x} can be of any form you like
            as long as the fitting function knows how to handle it. In
            that case you will also need supply you own pre-processing
            function (see \code{\link{pre_process}} that can extract
            training and test sets from the entire data set.

            See the functions \code{\link{pre_pamr}} and
            \code{\link{fit_pamr}} for an example of a function
            that does not take its data in the default way.
        }
        \item{\code{y}}{
            A response vector. This is the outcome
            you want to model, e.g. the feature of interest in a
            regression, class label in a classification problem, or
            anything else that a fitted model will produce when given
            data to make predictions from.
        }
        \item{\code{p1}, \code{p2}, \code{p3}, \code{...}}{
            (Optional) Method-specific model parameters.
            These will all be tunable with the
            \code{\link{tune}} and \code{\link{evaluate}}
            functions.
            Note that you can give them any name you want,
            the names used here are just an example.
        }
        \item{\code{.verbose}}{
            (Optional) Indentation level of log messages.
            Feed this to \code{\link{log_message}}.
        }
    }

    The function must return everything necessary to make
    future predictions, but it can take any form you like. In
    the simplest case it is just a number of fitted parameter
    values, like in a least squares regression, but it could
    also be some big and complex structure holding an ensemble
    of multiple sub-models.
}
\section{Making predictions}{

    Once a model is fitted it can be used to make predictions
    with a prediction function, defined as such

    \code{function(object, x, ...)}
    \describe{
        \item{\code{object}}{
            A fitted model produced by the model fitting function described above.
        }
        \item{\code{x}}{
            Observations to make predictions on (describing features only).
        }
        \item{\code{...}}{
            Parameters to the prediction functions. These are ignored by
            \code{\link{tune}} and \code{\link{evaluate}}, but
            could be convenient if the user wants to work with it manually.
        }
    }

    The output of the prediction function must be an object
    that can be compared to the true response, by an error
    function (see below). It is typically a list with elements
    named \code{"pred"} for "predictions" or "risk" for
    estimated risks. It can also be on an arbitrary form as
    long as a compatible error function is used.
}
\section{Calculating feature importance scores}{

    Estimating the importance of each feature (or variable)
    can often be as important as making predictions. Functions
    for calculating or extracting feature importance scores
    from fitted models should be defined as follows:

    \code{function(object, ...)}
    \describe{
        \item{\code{object}}{
            A fitted model produced by the model fitting function described above.
        }
        \item{\code{...}}{
            Parameters to the prediction functions.
            These are ignored by \code{\link{tune}} and
            \code{\link{evaluate}}, but could be convenient if
            the user wants to work with it manually.
        }
    }

    The function should return a vector of length p or a p-by-c
    data frame where p is the number of features in the data
    set and c is the number of classes.
}
\section{Calculating performance}{
    See \code{\link{error_fun}}.
}
\section{Resampling schemes}{
    See \code{\link{resample}}.
}
\section{Pre-processing functions}{
    See \code{\link{pre_process}}.
}
\section{Code style guidelines}{
    Names of functions, arguments and variables should be written in underscore
    separated lower case, singular form, unabbreviated, and American English.
    Users are encouraged to also follow use this style when wrining extensions.
    However, the guidelines may be violated in cases where they break the
    consistency with an incorporated well established package, see for example
    \code{\link{fit_randomForest}} which according to the guidelines should be
    \code{fit_randomforest} or \code{fit_random_forest}.

    A few exceptions to the rule against abbreviations exists, namely
    \sQuote{fun} and \sQuote{function} and \sQuote{dir} for \sQuote{directory}.
    These are only used for arguments to indicate the type of value that is
    accepted.
}
\author{Christofer \enc{Bäcklin}{Backlin}}
\seealso{\code{\link{emil}}, \code{\link{error_fun}},
    \code{\link{pre_process}}, \code{\link{resample}}}

