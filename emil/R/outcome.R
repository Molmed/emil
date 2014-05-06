##' Create a vector of outcomes

##'
##' Heavily modeled after the \code{\link{Surv}} class in the `survival` package.
##' Objects of this class are internally stored as data frames but should be
##' thought of as vectors and can be treated as such through out.
##' 
##' @param time Time points at which an event occurred.
##' @param event The type of event that occurred. \code{NA} codes for no event.
##' @param levels Which levels of event to keep. Defaults to all unique
##'   values of \code{event}.
##' @param censor What values of event should be considered censoring.
##'   Defaults to \code{NA} and all values in \code{event} not present in
##'   \code{levels}.
##' @return A vector of outcomes.
##' @examples
##' outcome(runif(15), sample(c(NA, "Mechanical failure", "Out of fuel"), 15, TRUE))
##' @seealso \code{\link{factor.events}}, \code{\link{integer.events}},
##'   \code{\link{plot.outcome}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
outcome <- function(time, event, levels, censor){
    if(missing(censor)){
        if(missing(levels)){
            censor <- NA
        } else {
            censor <- unique(c(NA, setdiff(as.character(unique(event)), levels)))
        }
    }
    if(missing(levels)) levels <- setdiff(
        if(is.factor(event)) base::levels(event) else unique(event),
        censor)
    time[!event %in% c(levels, censor)] <- NA
    event[is.na(time)] <- NA
    x <- data.frame(time=time, event=factor(event, levels=levels))
    class(x) <- "outcome"
    return(x)
}

##' Convert outcome vector to data frame
##' 
##' @param x Outcome vector.
##' @param ... Ignored, kept for S3 consistency.
##' @return The data frame underlying the outcome vector.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.data.frame.outcome <- function(x, ...){
    class(x) <- "data.frame"
    return(x)
}


##' Convert outcome vector to matrix
##'
##' @param x Outcome vector.
##' @param ... Ignored, kept for S3 consistency.
##' @return The data frame underlying the outcome vector converted to a matrix.
##'   Event types are converted to integers.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.matrix.outcome <- function(x, ...){
    x$event <- as.integer(x$event)
    x$event[is.na(x$event)] <- 0
    as.matrix(as.data.frame(x))
}


##' Convert object to outcome vector
##'
##' Converts objects to class \code{\link{outcome}}.
##' 
##' @param x Generic object.
##' @param ... Sent to class methods.
##' @return A vector of class \code{\link{outcome}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.outcome <- function(x, ...) UseMethod("as.outcome")


##' Convert Surv vector to outcome vector
##'
##' @param x Surv vector.
##' @param ... Ignored, kept for S3 consistency.
##' @return A vector of class \code{\link{outcome}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.outcome.Surv <- function(x, ...){
    x[is.na(x[,2]),1] <- NA
    outcome(x[,1], c(NA, "event")[1+x[,2]])
}


##' Convert object to Surv vector
##'
##' Converts objects to class \code{\link{Surv}}.
##' 
##' @param x Generic object.
##' @param ... Sent to class methods.
##' @return A vector of class \code{\link{Surv}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.Surv <- function(x, ...) UseMethod("as.Surv")

##' Trivial function
##'
##' Defined in case \code{\link{as.Surv}} is called on a \code{\link{Surv}}
##' object. 
##'
##' @param x Object.
##' @param ... Ignored.
##' @return A vector of class \code{\link{Surv}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.Surv.Surv <- function(x, ...) identity(x)


##' Convert outcome vector to Surv vector
##'
##' @param x Outcome vector.
##' @param main Surv only supports one event type. This argument controls
##'   which type that will be kept, all others are considered censoring.
##' @param censor A vector of event types to consider censoring.
##' @param ... Ignored, kept for S3 consistency.
##' @return A vector of class \code{\link{Surv}}. All event types other than
##'   the main event are considered censoring.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.Surv.outcome <- function(x, main=1, censor=NA, ...){
    if(is.numeric(main)) main <- levels(x$event)[main]
    if(is.numeric(censor)) censor <- levels(x$event)[censor]
    new.event <- c(NA, 0:1)[1+(!is.na(x$time) & x$event %in% c(main, censor)) + (x$event %in% main)]
    x <- as.matrix(x)
    x[is.na(new.event),1] <- NA
    x[,2] <- new.event
    colnames(x)[2] <- "status"
    attr(x, "type") <- "right"
    class(x) <- "Surv"
    return(x)
}

##' Convert outcome vector to character vector
##'
##' @param x Outcome vector.
##' @param ... Ignored, kept for S3 consistency.
##' @return A character vector.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.character.outcome <- function(x, ...) {
    if (is.R()) class(x) <- NULL
    else        oldClass(x) <- NULL

    ifelse(is.na(x$time), rep("NA", length(x)),
           ifelse(is.na(x$event), sprintf("?@>%.1f", x$time),
                                  sprintf("%s@%.1f", x$event, x$time)))
}


##' Get events on factor form
##'
##' By default all events are returned regardless of when they occurred. By
##' setting \code{time} only the events that has occurred up until then will be
##' returned, and cases with shorter follow-up times and no event will be marked
##' as censored.
##'
##' @param x Outcome vector.
##' @param time Time point to evaluate at.
##' @param censor.label What to label the absence of an event with.
##' @param keep Event types to keep, defaults to all.
##' @return A factor of events.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
factor.events <- function(x, time, censor.label="no event", keep){
    events <- as.character(x$event)
    if(missing(keep)){
        keep <- c(censor.label, levels(x$event))
    } else {
        keep <- c(censor.label, keep)
    }
    # If a time has been specified clear all events occurring after that time
    if(!missing(time)) events[!is.na(x$event) & x$time > time] <- NA
    # Replace "no event" with the no event level
    events[is.na(events)] <- censor.label
    # Keep the missing observations missing
    events[is.na(x)] <- NA
    if(!missing(time)) events[is.na(x$event) & x$time < time] <- NA
    return(factor(events, levels=keep))
}


##' Return events in integer form
##' 
##' Basically calls \code{\link{factor.events}} and converts to integer. No
##' event is coded with 0 and the other event types with > 0.
##'
##' @param x Outcome vector.
##' @param ... Sent to \code{\link{factor.events}}.
##' @return Integer vector.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
integer.events <- function(x, ...){
    return(as.integer(factor.events(x, ...))-1)
    #ifelse(is.na(x$time), NA, na.fill(as.integer(x$event), 0))
}


##' Print outcome vector
##'
##' @param x Outcome vector.
##' @param quote Logical, whether to print quotation marks.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing, only prints the vector to stdout.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
print.outcome <- function(x, quote=FALSE, ...) {
    print(as.character(x), quote=quote, ...)
}


##' Extract
##' 
##' @param x Outcome vector.
##' @param i Index.
##' @param j Column index, if given \code{x} is treated as a data frame.
##' @param drop See \code{\link{Extract}}
##' @return A subset of \code{x}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname extract.outcome
##' @export
`[.outcome` <- function(x, i, j, drop=FALSE) {
    if (missing(j)) {
        if (is.R()) {
            ctemp <- class(x)
            class(x) <- 'data.frame'
            x <- x[i,, drop=FALSE]
            class(x) <- ctemp
        } else {
            ctemp <- oldClass(x)
            oldClass(x) <- NULL
            x <- x[i,, drop=FALSE]
            oldClass(x) <- ctemp
        }
        return(x)
    } else { # return a matrix or vector
    	if (is.R()) class(x) <- 'data.frame'
        else oldClass(x) <- NULL
	    NextMethod("[")
	}
}


##' Check for missing values
##' 
##' @param x Outcome vector.
##' @return A logical vector with \code{TRUE} where an outcome is missing.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
is.na.outcome <- function(x) {
    is.na(x$time)
}


##' Test if object is of class outcome
##' 
##' @param x Object.
##' @return \code{TRUE} if \code{x} is of class \code{outcome}. \code{FALSE}
##'   otherwise.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
is.outcome <- function(x) inherits(x, 'outcome')


##' Dimension of an outcome vector
##' 
##' @param x Outcome vector.
##' @return A vector with number of observations and number of variables, of
##'   which there is always two, time and event.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
dim.outcome <- function(x) dim(as.data.frame(x))

##' Length of an outcome vector
##'
##' @param x Outcome vector.
##' @return Length.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
length.outcome <- nrow


##' Plot outcome vector
##' 
##' @param x outcome vector.
##' @param y Y-values.
##' @param segments Whether to draw horizontal segments.
##' @param flip Flip the plot to show time on y.
##' @param legendpos Position of legend, see \code{\link{legend}}. Set to NA or
##'   NULL to suppress legend.
##' @param ... Sent to \code{\link{plot}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
plot.outcome <- function(x, y, segments=TRUE, flip=FALSE, legendpos="topright", ...){
    if(missing(y)) y <- 1:length(x)

    if(flip){
        plot(y, x$time, type="n", ...)
        if(segments) segments(y, 0, y, x$time, col=integer.events(x)+1)
        points(y, x$time, pch=20, col=integer.events(x)+1)
    } else {
        plot(x$time, y, type="n", ...)
        if(segments) segments(0, y, x$time, y, col=integer.events(x)+1)
        points(x$time, y, pch=20, col=integer.events(x)+1)
    }
    if(!is.blank(legendpos)){
        legend(legendpos, c("No event", levels(x$event)), pch=20,
               col=0:length(levels(x$event))+1)
    }
}

