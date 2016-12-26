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
#' @param size.var string with the size (weighting) variable. Defaults to
#' \code{NULL}
#' @param shapes.var string with the shapes (different shapes for different)
#' points variable. Defaults to \code{NULL}
#' @param label.var string with the names of the points that can be used
#' with \code{geom_text} or \code{geom_text_repel}
#' @param title.out string with the the title for plot
#' @param xlabel string with the xlabel. Defualts to \code{NULL}
#' @param ylabel string with the ylabel
#' @param shapes numeric vector with the shapes of the points. For shape
#' options, see \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}
#' @param colors character vector with the colors for different points. Defaults
#' to \code{c("blue", "red", "green")}
#' @examples
#' data(mtcars)
#' crossplot_basic(mtcars, x.var = "mpg", y.var = "hp", shapes.var = "cyl")
#' @export
crossplot_basic <- function(data, x.var, y.var, size.var = NULL, shapes.var = NULL,
                       label.var = NULL,
                       title.out = NULL, xlabel = NULL,
                       ylabel = NULL, shapes = c(1, 2, 0, 5, 6),
                       colors = c("blue", "red", "green")) {

    ##if xlabel or ylabel are null, use the x and y variable names
    if (is.null(xlabel)) xlabel <- x.var
    if (is.null(ylabel)) ylabel <- y.var


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
                            alpha = 0.5)
    } else if (is.null(shapes.var)) {
        p <- p + geom_point(aes_string(size = size.var), shape = 1, alpha = 0.4)
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
    if (!is.null(title.out)) {
        p <- p + ggtitle(title.out)
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
    return(p)
}
