## c:/Dropbox/Rpackages/crossplotr/R/utils.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-26

##utility functions for crossplotr

#' Mean of a variable using the \code{lm} function
#'
#' return is the output from \code{lmtest::coeftest} which includes
#' the mean, the standard error, the t-stat and p-value
#'
#' @param df \code{data.frame} of data
#' @param var a string with the variable name
#' @param weights.var a string with variable to be used as
#'     weights. Defaults to \code{NULL}
#' @return an object from \code{lmtest::coeftest} with the output from
#'     the regression in a 1x4 matrix including the mean (the
#'     estimate), the standard error, the t-value, and the p-value
reg_mean <- function(df, var, weights.var = NULL) {

    if (!is.character(var)) {
        stop("var in reg_mean() needs to be a string.")
    }

    if (!is.null(weights.var)) {
        if (!is.character(weights.var)) {
            stop("weights.var in reg_mean() needs to be a string.")
        }
        ##Use weights
        mod.out <- lm(as.formula(paste0(var, " ~ 1")),
                      data = df,
                      weights = df[[weights.var]])
    } else {
        mod.out <- lm(as.formula(paste0(var, " ~ 1")),
                      data = df)
    }

    mod.out <- lmtest::coeftest(mod.out)
    return(mod.out)

}


#' Check that the data does not have and NA values
#'
#' Return a dataframe with no NA values for the variables of interest
#'
#'
#' @param data the data set
#' @param env the environment. Defaults to \code{parent.frame}
#' @return a data frame with no missing values for the variables of
#'     interest
data_no_na_values <- function(data, env = parent.frame()) {

    vars <- c(env$x.var, env$y.var)
    if (!is.null(env$size.var) && !is.na(env$size.var))
        vars <- c(vars, env$size.var)
    if (!is.null(env$control.vars) && !any(is.na(env$control.vars)))
        vars <- c(vars, env$control.vars)

    return(data[complete.cases(data[, vars]), ])
}

