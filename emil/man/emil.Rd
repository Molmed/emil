\name{emil}
\alias{emil}
\title{Introduction to the emil package}
\description{
The emil package implements a framework for working with
predictive modeling problems without information leakage.
For an overview of its functionality please read the
original publication (\enc{Bäcklin}{Backlin} 2014).
}

\section{Central topics and functions}{
    \subsection{Setting up modeling problems}{
        \describe{
            \item{\code{\link{resample}}}{Functions for generating and
                implementing resampling schemes.}
            \item{\code{\link{pre.process}}}{Data pre-processing
                functions.}
            \item{\code{\link{modeling.procedure}}}{Manages
                algorithms used for fitting models, making predictions, and
                extracting variable importance scores.}
            \item{\code{\link{error.fun}}}{Performance estimation
                functions used to tune variables and evaluate performance
                of modeling procedures.}
        }
    }
    \subsection{Solving modeling problems}{
        \describe{
            \item{\code{\link{fit}}}{Fit a model (according to a procedure).}
            \item{\code{\link{tune}}}{Tune parameters of a procedure.}
            \item{\code{\link{evaluate.modeling}}}{Evaluate
                the performance of a procedure using resampling.}
            \item{\code{\link{batch.model}}}{A semi-internal function
                that carries out most of the work. Parameters passed along
                from the above functions to this function are documented
                here.}
        } 
    }
    \subsection{Managing the results of modeling problems}{
        \describe{
            \item{\code{\link{subtree}}}{Extracts results
                from the output of \code{\link{evaluate.modeling}}. It is
                essentially a recursive version of \code{\link{lapply}} and
                \code{\link{sapply}}.}
            \item{\code{\link{subframe}}}{Extracts results and organize
                them per observation. Useful when comparing predcitions and
                properties of individual observations accross folds of a
                resampling scheme.}
        }
        No functions for plotting is included in the current version of the
        package, except for \code{\link{image.reample}}.
    }
}

\section{Methods included in the package}{
    \subsection{Resampling methods}{
        See \code{\link{resample}} for information on usage and implementation
        of custom methods.
        \describe{
            \item{\code{\link{resample.holdout}}}{Repeated holdout.}
            \item{\code{\link{resample.crossval}}}{Cross validation.}
        }
    }
    \subsection{Data pre-processing methods}{
        See \code{\link{pre.process}} for information on usage and
        implementation of custom methods.  The imputation functions
        can also be used outside of the resampling scheme, see
        \code{\link{impute}}.
        \describe{
            \item{\code{\link{pre.split}}}{Only split, no transformation.}
            \item{\code{\link{pre.center}}}{Center data to have mean 0 of each
                feature.}
            \item{\code{\link{pre.scale}}}{Center and scale data to
                have mean 0 and standard deviation 1.}
            \item{\code{\link{pre.impute.median}}}{Impute missing
                values with feature medians.}
            \item{\code{\link{pre.impute.knn}}}{Impute missing values
                with k-NN, see \code{\link{pre.impute.knn}} for details on
                how to set parameters.}
        }
    }
    \subsection{Modeling methods}{
        See \code{\link{modeling.procedure}} for information on usage
        and \code{\link{emil.extensions}} for information on
        implementation of custom methods.
        \describe{
            \item{\code{\link[=emil.fit.cforest]{cforest}}}{Conditional inference
                forest.}
            \item{\code{\link[=emil.fit.glmnet]{glmnet}}}{Elastic net.}
            \item{\code{\link[=emil.fit.lda]{lda}}}{Linear discriminant.}
            \item{\code{\link[=emil.fit.lm]{lm}}}{Linear model.}
            \item{\code{\link[=emil.fit.pamr]{pamr}}}{Nearest shrunken
                centroids.}
            \item{\code{\link[=emil.fit.qda]{qda}}}{Quadratic discriminant.}
            \item{\code{\link[=emil.fit.randomForest]{randomForest}}}{Random
                forest.}
        }
    }
    \subsection{Performance estimation methods}{
        See \code{\link{error.fun}} for information on usage and implementation
        of custom methods.

        For classification problems:
            \describe{
                \item{\code{error.rate}}{Fraction of predictions that were
                    incorrect.}
            \item{\code{weighted.error.rate}}{See its own documentation.}
            \item{\code{neg.auc}}{Negative area under ROC curve.}
            \item{\code{\link{neg.gmpa}}}{Negative geometric mean of class-specific
                prediction accuracy. Good for problems with imbalanced class sizes.}
        }
        For regression problems:
        \describe{
            \item{\code{mse}}{Mean square error.}
            \item{\code{rmse}}{Root mean square error.}
        }
        For survival analysis problem:
        \describe{
            \item{\code{neg.harrell.C}}{Negative Harrell's concordance index.}
        }
    }
}
\author{Christofer \enc{Bäcklin}{Backlin}}

