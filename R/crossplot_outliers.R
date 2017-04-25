## c:/Dropbox/Rpackages/crossplotr/R/crossplot_outliers.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2017-04-25

##To get the data corresponding to outliers in certain variables


#' To get the index of the outliers for a vector x
#'
#' This function will return a logical index where outliers are set to
#' \code{TRUE}. The user can imput either imput the number of desired
#' outliers, \code{num.outliers}, or the percentile corresponding to
#' the outliers \code{percentile.outliers}, but not both
#'
#' @param x a numeric vector
#' @param num.outliers the number of outliers. Defaults to \code{NULL}
#' @param percentile.outliers the percentile used to measure
#'     outliers. Defaults to \code{NULL}
#' @return a logical index where outliers are set to \code{TRUE}
get_outlier_logicals <- function(x, num.outliers = NULL, percentile.outliers = NULL) {

    if (!is.numeric(x)) stop("Error: x must be numeric")

    if (!is.null(num.outliers) & !is.null(percentile.outliers))
        stop("Error: get_outlier_logical() only uses either num.outliers or percentile.outliers")

    ##Sort x
    x.sorted <- sort(x)
    ##the length of x
    nn <- length(x)

    if (!is.null(num.outliers)) {
        ##Use the number of outliers
        outliers <- c(x.sorted[1:num.outliers], x.sorted[(nn - num.outliers + 1):nn])
        which(x %in% outliers)
    } else if (!is.null(percentile.outliers)) {
        ##use the percentile to get the outliers

        ##Make sure percentile.outliers is between 0 and 1
        if (percentile.outliers < 0 | percentile.outliers > 1 )
            stop("Error: percentile.outliers must be between 0 and 1")

        ##The number of outliers to be used if the percentile is set
        num.percentile.outliers <- round(nn * percentile.outliers)
        outliers <- c(x.sorted[1:num.percentile.outliers],
                      x.sorted[(nn - num.percentile.outliers):nn])

    } else {
        stop("Error: get_outlier_logical() requires either num.outliers or percentile.outliers to be set")
    }

    ##Return the logicals for the outliers
    return(x %in% outliers)

}


#' To get the outlier data form a ggplot
#'
#' Based of plot variables from the user, \code{crossplot_outliers}
#' returns the plot data corresponding to the outliers. The user can
#' imput either imput the number of desired outliers,
#' \code{num.outliers}, or the percentile corresponding to the
#' outliers \code{percentile.outliers}, but not both
#'
#' @param p a crossplot \code{ggplot2} plot. Can be generated from
#'     \code{crossplot}
#' @param num.outliers the number of outliers. Defaults to \code{NULL}
#' @param percentile.outliers the percentile used to measure
#'     outliers. Defaults to \code{NULL}
#' @param plot.vars the plot variables for which the outliers will be
#'     calculated. These variables are from \code{ggplot2} mappings
#'     and can include \code{x}, \code{y}, \code{size}. The default is
#'     \code{c("x", "y")}
#' @return a \code{data.frame} with the plot data for the outliers
#' @examples
#' p <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point(aes(size = wt))
#' ##Get the outliers -- defaults to outliers for just "x" and "y"
#' crossplot_outliers(p, num.outliers = 2)
#' crossplot_outliers(p, percentile.outliers = .05)
#' ##Specifying the plot variables
#' crossplot_outliers(p, num.outliers = 2, plot.vars = c("x", "y", "size"))
#' crossplot_outliers(p, percentile.outliers = .05, plot.vars = c("x", "y", "size"))
#' @export
crossplot_outliers <- function(p, num.outliers = NULL, percentile.outliers = NULL,
                               plot.vars = c("x", "y")) {

    if (!is.null(num.outliers) & !is.null(percentile.outliers))
        stop("Error: crossplot_outliers() only uses either num.outliers or percentile.outliers")

    ##The plot data
    all.data <- p$data

    ##Get a named character vector with all of the mappings
    mappings <- p$mapping %>% as.character
    ##The other layers -- just looking for size here
    layer.mappings <- lapply(p$layers, function(x) x$mapping %>% as.character)
    layer.mappings <- do.call("c", layer.mappings)
    ##All mappings
    mappings <- c(mappings, layer.mappings)
    ##Keep only the variables requested by the used
    mappings <- mappings[names(mappings) %in% plot.vars] %>% as.character

    if (!is.null(num.outliers)) {
        f_get_outliers <- function(x) get_outlier_logicals(x, num.outliers)
    } else if (!is.null(percentile.outliers)) {
        f_get_outliers <- function(x) get_outlier_logicals(x, percentile.outliers)
    } else {
        stop("Error: crossplot_outliers() requires either num.outliers or percentile.outliers to be set")
    }

    ##Get just the data that we need for the outlier indices
    outliers.logicals <- all.data[, mappings]
    ##apply the f_get_outliers function
    outliers.logicals[] <- lapply(outliers.logicals, f_get_outliers)
    ##sum across rows and get a logical
    outliers.logicals <- rowSums(outliers.logicals) %>% as.logical

    ##Return the data
    all.data.outliers <- all.data[outliers.logicals, ]

    return(all.data.outliers)

}





