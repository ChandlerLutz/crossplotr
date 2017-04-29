## c:/Dropbox/Rpackages/crossplotr/R/crossplot_print_stats.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-26

#' A convience function to print the plot stats and a regression line
#'
#' @param p a crossplot \code{ggplot2} plot. Can be generated from
#'     \code{crossplot}
#' @param stats character vector with the stats that will be printed
#'     (in order). Possible statistics include \code{"slope"},
#'     \code{"intercept"}, \code{"r.squared"}, \code{"adj.r.squared"},
#'     \code{"mean.x"}, \code{"mean.y"} \code{"var.x"},
#'     \code{"var.y"}, \code{"sd.x"}, \code{"sd.y"}. Defualt is
#'     \code{c("slope", "r.squared")}
#' @param text.pos the text position to be passed to
#'     \code{cowplot::draw_label()}
#' @param sprintf.format character string with the \code{sprintf}
#'     format
#' @param xlabel the label for the x-axis variable
#' @param ylabel the label for the y-axis variable
#' @param log.reg logical. If set to \code{TRUE}, the regression will
#'     be estimated in logs
#' @param weighted logical. If set to \code{TRUE}, the regression and
#'     all statistics will be weighted using the \code{size} variable
#'     form the \code{ggplot2} plot
#' @param reg.label.se logical. If set to \code{TRUE}, the regression
#'     standard errors will be printed
#' @param reg.color a string with the \code{ggplot2} color for the
#'     regression line
#' @param reg.linetype a string with the \code{ggplot2} linetype for
#'     the regression line
#' @param mean.label.se logical If set to \code{TRUE}, the standard
#'     errors for the means will be printed.
#' @return a \code{ggplot2} plot with the regression line and label
#'     added
#' @export
crossplot_print_stats <- function(p, stats = c("slope", "r.squared"),
                                  text.pos,
                                  sprintf.format = "%.2f",
                                  xlabel = NULL, ylabel = NULL,
                                  log.reg = FALSE, weighted = FALSE,
                                  reg.label.se = TRUE,
                                  reg.color = "black", reg.linetype = "solid",
                                  mean.label.se = FALSE
                                  ) {


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
                            hjust = 0, vjust = 0)

    return(p.out)
}




