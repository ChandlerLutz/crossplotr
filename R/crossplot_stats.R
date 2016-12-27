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
#' @param p the plot
#' @param log.reg logical variable that indicates if the regression
#' should be run in logs. Defaults to \code{FALSE}
#' @param weighted logical variable that indicates if the the regression
#' will be weighted by the \code{size} variable from the plot. Defaults
#' to \code{FALSE}
#' @param sprintf.format the string format for the stats in \code{sprintf}.
#' @return a \code{data.frame} with the crossplot stats. The \code{data.frame}
#' will include the following variables:
#' \code{"slope"}, \code{"slope.se"}, \code{"intercept"}, \code{"intercept.se"},
#' \code{"r.squared"}, \code{"adj.r.squared"}, \code{"mean.x"}, \code{"mean.y"}
#' \code{"var.x"}, \code{"var.y"}, \code{"sd.x"}, \code{"sd.y"}. The default values
#' are \code{c("slope", "slope.se", "r.squared")}. The standard errors for the slope
#' and the intercept will be printed in parentheses next to the coefficient. If
#' the means, variances, or standard deviations are requested by the user,
#' \code{xlabel} and \code{ylabel} (see below) will be used for the labels.
#' Standard errors will be heteroskedasticity-robust white standard errors
#' @export
crossplot_stats <- function(p, log.reg = FALSE, weighted = FALSE,
                            sprintf.format = "%.2f") {



    ##Get a named character vector with all of the mappings
    mappings <- p$mapping %>% as.character
    ##The other layers -- just looking for size here
    layer.mappings <- lapply(p$layers, function(x) x$mapping %>% as.character)
    layer.mappings <- do.call("c", layer.mappings)
    mappings <- c(mappings, layer.mappings)

    ##If weighted, there must be a size variable pre-defined
    if (weighted & any(grepl("size", names(mappings)))) {
        stop("For weighted estimation, a size variable needs to be defined in the ggplot")
    }

    ##Define the x and y variables
    x.var <- mappings["x"]
    y.var <- mappings["y"]
    if (weighted) size.var <- mappings["size"]

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
        mean.x <- Hmisc::wtd.mean(temp.data[[x.var]], temp.data[[size.var]])
        mean.y <- Hmisc::wtd.mean(temp.data[[y.var]], temp.data[[size.var]])
        var.x <- Hmisc::wtd.var(temp.data[[x.var]], temp.data[[size.var]])
        var.y <- Hmisc::wtd.var(temp.data[[y.var]], temp.data[[size.var]])
    } else {
        mod <- lm(formula.model, temp.data)
        mean.x <- mean(temp.data[[x.var]])
        mean.y <- mean(temp.data[[y.var]])
        var.x <- var(temp.data[[x.var]])
        var.y <- var(temp.data[[y.var]])
    }
    ##the standard deviations of the variables
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
                            stringsAsFactors = FALSE)
    ##Use sprintf to format
    stats.out[] <- lapply(., function(x) sprintf(sprintf.format, x))
    ##Add parentheses around the standard errors
    stats.out <- stats.out %>%
        dplyr::mutate(intercept.se <- paste0("(", intercept.se, ")")) %>%
        dplyr::mutate(slope.se <- paste0("(", slope.se, ")"))

    return(stats.out)

}
