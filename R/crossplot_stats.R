## c:/Dropbox/Rpackages/crossplotr/R/crossplot_stats.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-26


#' Basic Statistics for regression varaibles and plotting
#'
#' This function will plot either an unweighted or weighted
#' regression line and print summary statistics. Log regressions
#' are also permitted
#'
#'
#' @param p a crossplot \code{ggplot2} plot. Can be generated from
#'     \code{crossplot}
#' @param log.reg logical variable that indicates if the regression
#'     should be run in logs. Defaults to \code{FALSE}
#' @param weighted logical variable that indicates if the the
#'     regression will be weighted by the \code{size} variable from
#'     the plot. Defaults to \code{FALSE}
#' @param sprintf.format the string format for the stats in
#'     \code{sprintf}.
#' @param xlabel a string with the name for the x-variable. Defaults
#'     to \code{NULL}. If set to \code{NULL}, the variable name from
#'     the x-axis will be used
#' @param ylabel a string with the name for the y-variable. Defaults
#'     to \code{NULL}. If set to \code{NULL}, the variable name from
#'     the y-axis will be used
#' @param reg.label.se logical. If set to \code{TRUE}, the regression
#'     standard errors will be printed
#' @param mean.label.se logical. If set to \code{TRUE}, the standard
#'     errors for the means will be printed.
#' @return a list of two \code{data.frame}s with the crossplot
#'     stats. The first element of the list, \code{plot.stats} holds a
#'     \code{data.frame} of the stats in numeric format. The second
#'     element of the list \code{plot.stats.pretty} holds a
#'     \code{data.frame} of the stats in a pretty stringr format using
#'     \code{sprintf}. The \code{data.frame} will include the
#'     following variables: \code{"slope"}, \code{"slope.se"},
#'     \code{"intercept"}, \code{"intercept.se"}, \code{"r.squared"},
#'     \code{"adj.r.squared"}, \code{"mean.x"}, \code{"mean.y"}
#'     \code{"var.x"}, \code{"var.y"}, \code{"sd.x"},
#'     \code{"sd.y"}. The standard errors for the slope and the
#'     intercept will be printed in parentheses next to the
#'     coefficient in the labels. For the labels of the means,
#'     variances, or standard deviations, \code{xlabel} and
#'     \code{ylabel} will be used for the labels.  Standard errors
#'     will be heteroskedasticity-robust white standard errors
#' @export
crossplot_stats <- function(p, log.reg = FALSE, weighted = FALSE,
                            sprintf.format = "%.2f",
                            xlabel = NULL, ylabel = NULL,
                            reg.label.se = TRUE, mean.label.se = FALSE) {



    ##Get a named character vector with all of the mappings
    mappings <- p$mapping %>% as.character
    ##The other layers -- just looking for size here
    layer.mappings <- lapply(p$layers, function(x) x$mapping %>% as.character)
    layer.mappings <- do.call("c", layer.mappings)
    mappings <- c(mappings, layer.mappings)

    ##If weighted, there must be a size variable pre-defined
    if (weighted & !any(grepl("size", names(mappings)))) {
        stop("For weighted estimation, a size variable needs to be defined in the ggplot")
    }

    ##Define the x and y variables
    x.var <- mappings["x"]
    y.var <- mappings["y"]
    if (weighted) size.var <- mappings["size"]


    ##If xlable and ylabel are undefined, use x.var and y.var
    if (is.null(xlabel)) xlabel = x.var
    if (is.null(ylabel)) ylabel = y.var

    ##The data
    temp.data <- p$data %>%
        ##remove observations where x or y are missing
        dplyr::filter_(lazyeval::interp(~!is.na(x), x = as.name(x.var))) %>%
        dplyr::filter_(lazyeval::interp(~!is.na(y), y = as.name(y.var)))


    ##The regresison formula
    if (log.reg) {
        formula.model <- paste0("log(", y.var, ") ~ log(", x.var, ")") %>%
            as.formula(.)
    } else {
        formula.model <- paste0(y.var, " ~ ", x.var) %>%
            as.formula(.)
    }

    ##The plot statistics
    if (weighted) {
        ##use weighted values
        mod <- lm(formula.model, temp.data, weights = temp.data[[size.var]])
        mod.mean.x <- reg_mean(temp.data, x.var, size.var)
        mod.mean.y <- reg_mean(temp.data, y.var, size.var)
        var.x <- Hmisc::wtd.var(temp.data[[x.var]], temp.data[[size.var]])
        var.y <- Hmisc::wtd.var(temp.data[[y.var]], temp.data[[size.var]])
    } else {
        mod <- lm(formula.model, temp.data)
        mod.mean.x <- reg_mean(temp.data, x.var)
        mod.mean.y <- reg_mean(temp.data, y.var)
        var.x <- var(temp.data[[x.var]])
        var.y <- var(temp.data[[y.var]])
    }
    ##the standard deviations of the variables
    mean.x <- mod.mean.x[1, 1]
    se.mean.x <- mod.mean.x[1, 2]
    mean.y <- mod.mean.y[1, 1]
    se.mean.y <- mod.mean.y[1, 2]
    sd.x <- sqrt(var.x)
    sd.y <- sqrt(var.y)


    ##the white standard erorrs
    mod.coeftest <- lmtest::coeftest(mod, vcov = sandwich::sandwich) %>%
        broom::tidy(.)
    ##the model summary stats
    mod.glance <- mod %>% broom::glance(.)

    intercept <- mod.coeftest$estimate[1]
    intercept.se <- mod.coeftest$std.error[1]
    slope <- mod.coeftest$estimate[2]
    slope.se <- mod.coeftest$std.error[2]

    ##the summary stats to be printed
    stats.out <- data.frame(intercept = intercept, intercept.se = intercept.se,
                            slope = slope, slope.se = slope.se,
                            r.squared = mod.glance$r.squared,
                            adj.r.squared = mod.glance$adj.r.squared,
                            mean.x = mean.x, mean.y = mean.y,
                            var.x = var.x, var.y = var.y,
                            sd.x = sd.x, sd.y = sd.y,
                            se.mean.x = se.mean.x,
                            se.mean.y = se.mean.y,
                            stringsAsFactors = FALSE)
    ##Use sprintf to format
    stats.out.pretty <- stats.out
    stats.out.pretty[] <- lapply(stats.out.pretty, function(x) sprintf(sprintf.format, x))
    ##Add parentheses around the standard errors
    stats.out.pretty <- stats.out.pretty %>%
        dplyr::mutate(intercept.se = paste0("(", intercept.se, ")")) %>%
        dplyr::mutate(slope.se = paste0("(", slope.se, ")")) %>%
        dplyr::mutate(se.mean.x = paste0("(", se.mean.x, ")")) %>%
        dplyr::mutate(se.mean.y = paste0("(", se.mean.y, ")"))

    ## -- Now get the labels -- ##
    stats.out.labels <- stats.out.pretty
    ##if reg.label.se is true, add the standard error to the slope and intercept
    if (reg.label.se) {
        stats.out.labels$intercept <- paste(stats.out.labels$intercept,
                                            stats.out.labels$intercept.se)
        stats.out.labels$slope <- paste(stats.out.labels$slope,
                                        stats.out.labels$slope.se)
    }

    ##if mean.label.se is TRUE, print the mean standard errors
    if (mean.label.se) {
        stats.out.labels$mean.x <- paste(stats.out.labels$mean.x,
                                         stats.out.labels$se.mean.x)
        stats.out.labels$mean.y <- paste(stats.out.labels$mean.y,
                                         stats.out.labels$se.mean.y)
    }


    stats.out.labels$intercept.se <- NULL
    stats.out.labels$slope.se <- NULL
    stats.out.labels$se.mean.x <- NULL
    stats.out.labels$se.mean.y <- NULL
    ##Now add the labels to all of the variable
    stats.out.labels$intercept = paste0("Intercept = ", stats.out.labels$intercept)
    stats.out.labels$slope = paste0("Slope = ", stats.out.labels$slope)
    stats.out.labels$r.squared = paste0("R-Squared = ", stats.out.labels$r.squared)
    stats.out.labels$adj.r.squared = paste0("Adj R-Squared = ",
                                            stats.out.labels$adj.r.squared)
    stats.out.labels$mean.x = paste0("Mean(", xlabel, ") = ",
                                     stats.out.labels$mean.x)
    stats.out.labels$mean.y = paste0("Mean(", ylabel, ") = ",
                                     stats.out.labels$mean.y)
    stats.out.labels$var.x = paste0("Var(", xlabel, ") = ",
                                     stats.out.labels$var.x)
    stats.out.labels$var.y = paste0("Var(", ylabel, ") = ",
                                    stats.out.labels$var.y)
    stats.out.labels$sd.x = paste0("Sd(", xlabel, ") = ",
                                     stats.out.labels$sd.x)
    stats.out.labels$sd.y = paste0("Sd(", ylabel, ") = ",
                                   stats.out.labels$sd.y)

    ##Return
    return(list(plot.stats = stats.out, plot.stats.pretty = stats.out.pretty,
                plot.stats.labels = stats.out.labels))

}
