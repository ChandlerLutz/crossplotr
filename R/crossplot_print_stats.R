## c:/Dropbox/Rpackages/crossplotr/R/crossplot_print_stats.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-26

#' A convience function to print the plot stats and a regression line
#'
#' @param p a crossplot \code{ggplot2} plot. Can be generated from
#'     \code{crossplot}
#' @param text.pos the text position to be passed to
#'     \code{cowplot::draw_label()} as a numeric vector of length 2
#'     with \code{x} and \code{y}.
#' @param stats character vector with the stats that will be printed
#'     (in order). Possible statistics include \code{"slope"},
#'     \code{"intercept"}, \code{"r.squared"}, \code{"adj.r.squared"},
#'     \code{"mean.x"}, \code{"mean.y"} \code{"var.x"},
#'     \code{"var.y"}, \code{"sd.x"}, \code{"sd.y"}. Defualt is
#'     \code{c("slope", "r.squared")}
#' @param reg.color a string with the \code{ggplot2} color for the
#'     regression line
#' @param reg.linetype a string with the \code{ggplot2} linetype for
#'     the regression line
#' @param log.reg logical. If set to \code{TRUE}, the regression will
#'     be estimated in logs. Defualt is \code{FALSE}

#' @param weighted logical. If set to \code{TRUE}, the regression and
#'     all statistics will be weighted using the \code{size} variable
#'     form the \code{ggplot2} plot. Default is \code{FALSE}
#' @param sprintf.format character string with the \code{sprintf}
#'     format.
#' @param xlabel the label for the x-axis variable
#' @param ylabel the label for the y-axis variable
#' @param reg.label.se logical. If set to \code{TRUE}, the regression
#'     standard errors will be printed. Default is \code{TRUE}
#' @param mean.label.se logical If set to \code{TRUE}, the standard
#'     errors for the means will be printed. Default is \code{FALSE}
#' @param ... Other arguments to be passed to
#'     \code{cowplot::draw_label}. See \code{?cowplot::draw_label} for
#'     options and more details.
#' @return a \code{ggplot2} plot with the regression line and label
#'     added
#' @examples
#' data(mtcars)
#' p <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
#'                shapes.var = "cyl")
#' ##Print weighted regression line and weighted stats
#' crossplot_print_stats(p, text.pos = c(200, 25), weighted = TRUE)
#' ##Print unweighted regression line and unweighted stats
#' crossplot_print_stats(p, text.pos = c(200, 25), weighted = FALSE)
#'
#' ##passing other arguments to cowplot::draw_label.
#' ##See ?cowplot::draw_label for more details
#' crossplot_print_stats(p, text.pos = c(200, 25), weighted = FALSE,
#'                       fontface = "bold")
#'
#' ## -- Controlling for qsec weighted by wt -- ##
#'
#' ##First see the results using a normal regression
#' mod <- lm(mpg ~ hp + qsec, mtcars, weights = wt)
#' library(sandwich); library(lmtest)
#' coeftest(mod, vcov = sandwich)
#' ##Now in a plot
#' p2 <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
#'                 shapes.var = "cyl", control.vars = "qsec")
#' crossplot_print_stats(p2, text.pos = c(0, 7), weighted = TRUE)
#' @export
crossplot_print_stats <- function(p,
                                  ##Aesthetics for the plot
                                  text.pos,
                                  stats = c("slope", "r.squared"),
                                  reg.color = "black",
                                  reg.linetype = "solid",
                                  ##Arguments that will be
                                  ##passed to crossplot_stats
                                  log.reg = FALSE, weighted = FALSE,
                                  sprintf.format = "%.2f",
                                  xlabel = NULL, ylabel = NULL,
                                  reg.label.se = TRUE,
                                  mean.label.se = FALSE, ...) {

    ##get the stats
    plot.stats <- crossplot_stats(p, log.reg = log.reg, weighted = weighted,
                                  sprintf.format = sprintf.format,
                                  xlabel = xlabel, ylabel = ylabel,
                                  reg.label.se = reg.label.se,
                                  mean.label.se = mean.label.se)


    print(plot.stats$stats.out$intercept)
    ##Add the regression line
    p <- p + geom_abline(slope = plot.stats$plot.stats$slope,
                         intercept = plot.stats$plot.stats$intercept,
                         color = reg.color, linetype = reg.linetype)

    ##get the pretty plots
    plot.stats.labels <- plot.stats$plot.stats.labels
    ##Only use the variables in stats as requested by the user
    plot.stats.labels <- plot.stats.labels[stats]

    plot.stats.labels <- paste(plot.stats.labels, collapse = "\n")

    ##Add the text to the plot. Set hjust and vjust to zero
    ##To make it left aligned
    p.out <- p + draw_label(label = plot.stats.labels, x = text.pos[1], y = text.pos[2],
                            hjust = 0, vjust = 0, ...)

    return(p.out)
}




