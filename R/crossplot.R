## c:/Dropbox/Rpackages/crossplotr/R/plot_basic.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-26


#' Create a basic cross-sectional plot using standard evaluation
#' of the variable names
#'
#' @param data the data to be plotted
#' @param x.var string with the x axis variable
#' @param y.var string with the y axis variable
#' @param size.var string with the size (weighting) variable. Defaults
#'     to \code{NULL}
#' @param shapes.var string with the shapes (different shapes for
#'     different) points variable. Defaults to \code{NULL}
#' @param label.var string with the names of the points that can be
#'     used with \code{geom_text} or \code{geom_text_repel}
#' @param control.vars a character vector with the names of variables
#'     that will be used as controls via the Frisch–Waugh–Lovell
#'     theorem. The result will be a plot (an AV Plot) and statistics
#'     where the variables in \code{control.vars} are partialed
#'     out. These variables must be present in the original
#'     dataset. Note: if \code{control.vars} is set, an attribute will
#'     be added to the returned plot and will be named
#'     \code{control.vars}
#' @param title string with the the title for plot
#' @param xlabel string with the xlabel. Defualts to \code{NULL}
#' @param ylabel string with the ylabel
#' @param shapes numeric vector with the shapes of the points. For
#'     shape options, see
#'     \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}
#' @param colors character vector with the colors for different
#'     points. Defaults to \code{c("blue", "red", "green")}
#' @param points.alpha \code{numeric(1)} indicating the alpha to be
#'     applied to the points. Default value is \code{1}. Setting
#'     \code{points.alpha = 0.7} is helpful when plotting labels with
#'     the points
#' @return a \code{ggplot2} plot with the cross-sectional output
#' @examples
#' data(mtcars)
#' crossplot(mtcars, x.var = "mpg", y.var = "hp", size.var = "wt",
#'           shapes.var = "cyl")
#' ##Control for (partial out) qsec
#' p <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
#'               shapes.var = "cyl", control.vars = "qsec")
#' print(p)
#' p$control.vars
#' @export
crossplot <- function(data, x.var, y.var, size.var = NULL, shapes.var = NULL,
                      label.var = NULL, control.vars = NULL,
                      title = NULL, xlabel = NULL,
                      ylabel = NULL, shapes = c(1, 2, 0, 5, 6),
                      colors = c("blue", "red", "green"), points.alpha = 1) {

    ##Make sure data is a dataframe
    if (!is.data.frame(data)) stop("Error: data is not a data frame")
    data <- as.data.frame(data)

    ##Make sure x.var, y.var, size.var, shapes.var, label.var, and control.vars
    ##are character strings
    if (!is.character(x.var) || !is.character(y.var))
        stop("Error: x.var and y.var need to be character strings")
    if (!is.null(size.var) && !is.character(size.var))
        stop("Error: size.var needs to be a character string")
    if (!is.null(shapes.var) && !is.character(shapes.var))
        stop("Error: shapes.var needs to be a character string")
    if (!is.null(label.var) && !is.character(label.var))
        stop("Error: shapes.var needs to be a character string")
    if (!is.null(control.vars) && !is.character(control.vars))
        stop("Error: control.vars needs to be a character vector")

    ##if xlabel or ylabel are null, use the x and y variable names
    if (is.null(xlabel)) xlabel <- x.var
    if (is.null(ylabel)) ylabel <- y.var

    ##Only complete cases for the variables of interest -- see utils.R
    data <- data_no_na_values(data)

    ##If necessary, update x and y using control.vars and the
    ##Frisch–Waugh–Lovell thm
    if (!is.null(control.vars)) {
        if (!(control.vars %in% names(data)))
            stop("Error: each member of control.vars needs to be in data")

        if (is.null(size.var)) {
            ##Not weighted
            data[[x.var]] <- residuals(lm(data[, x.var] ~ data[, control.vars]))
            data[[y.var]] <- residuals(lm(data[, y.var] ~ data[, control.vars]))
        } else {
            ##Weighted
            data[[x.var]] <- residuals(lm(data[, x.var] ~ data[, control.vars],
                                          weights = data[, size.var]))
            data[[y.var]] <- residuals(lm(data[, y.var] ~ data[, control.vars],
                                          weights = data[, size.var]))
        }

    }



    ##Update shapes.var and factor ordering to make sure the bottom-positioned
    ##legend has space
    if (!is.null(shapes.var)) {
        if (is.factor(data[[shapes.var]])) {
            ##factor -- retain the levels
            fact.levels <- levels(data[[shapes.var]]) %>% as.character
        } else {
            fact.levels <- data[[shapes.var]] %>% unique %>% as.character
        }
        fact.levels <- stringr::str_trim(fact.levels)
        data[[shapes.var]] <- factor(paste0(stringr::str_trim(data[[shapes.var]]), "  "),
                                           levels = paste0(fact.levels, "  "))
    }


    ##Do the basic plot
    if (is.null(label.var)) {
        p <- ggplot(data, aes_string(x.var, y.var))
    } else {
        p <- ggplot(data, aes_string(x.var, y.var, label = label.var))
    }

    ##Depending on shapes.var is set
    if (!is.null(shapes.var)) {
        p <- p + geom_point(aes_string(size = size.var,
                                       color = shapes.var,
                                       shape = shapes.var),
                            alpha = points.alpha)
    } else if (is.null(shapes.var)) {
        p <- p + geom_point(aes_string(size = size.var), shape = 1, alpha = points.alpha)
    }

    ##The aestetics of the plot
    p <- p +
        background_grid(minor='none') +
        ##theme_bw() +
        ##The colors -- black and red
        scale_colour_manual(values = colors)  +
        ##The size of the shapes -- range from 2 to 20 and remove legend
        scale_size_continuous(range = c(2, 20), guide = "none") +
        ##For shapes across the metros
        scale_shape_manual(values = shapes) +
        labs(x = xlabel, y = ylabel)

    ##If necessary, add the title
    if (!is.null(title)) {
        p <- p + ggtitle(title)
    }

    p <- p +
        ##remove legend title and add other aesthetics to the legend
        theme(legend.text = element_text(size = 16),
              legend.title = element_blank(),
              ##legend.background = element_rect(fill="gray90",
              ##                                 size=.5,linetype="dotted"),
              legend.background = element_rect(color="black",
                                              size=.5,linetype="solid"),
              panel.grid.minor = element_blank()) +
        ##make symbols in legend bigger. see
        ##http://stackoverflow.com/a/20416049/1317443
        guides(colour = guide_legend(override.aes = list(size=6)))

    ##Add control.vars as an attribute to p
    if (!is.null(control.vars)) {
        p$control.vars <- control.vars
    } else {
        p$control.vars <- NA
    }

    return(p)
}
