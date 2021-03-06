% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossplot.R
\name{crossplot}
\alias{crossplot}
\title{Create a basic cross-sectional plot using standard evaluation
of the variable names}
\usage{
crossplot(data, x.var, y.var, size.var = NULL, shapes.var = NULL,
  label.var = NULL, control.vars = NULL, title = NULL, xlabel = NULL,
  ylabel = NULL, shapes = c(1, 2, 0, 5, 6), colors = c("blue", "red",
  "darkgreen"), points.alpha = 1)
}
\arguments{
\item{data}{the data to be plotted}

\item{x.var}{string with the x axis variable}

\item{y.var}{string with the y axis variable}

\item{size.var}{string with the size (weighting) variable. Defaults
to \code{NULL}}

\item{shapes.var}{string with the shapes (different shapes for
different) points variable. Defaults to \code{NULL}}

\item{label.var}{string with the names of the points that can be
used with \code{geom_text} or \code{geom_text_repel}}

\item{control.vars}{a character vector with the names of variables
that will be used as controls via the Frisch–Waugh–Lovell
theorem. The result will be a plot (an AV Plot) and statistics
where the variables in \code{control.vars} are partialed
out. These variables must be present in the original
dataset. Note: if \code{control.vars} is set, an attribute will
be added to the returned plot and will be named
\code{control.vars}}

\item{title}{string with the the title for plot}

\item{xlabel}{string with the xlabel. Defualts to \code{NULL}}

\item{ylabel}{string with the ylabel}

\item{shapes}{numeric vector with the shapes of the points. For
shape options, see
\url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}}

\item{colors}{character vector with the colors for different
points. Defaults to \code{c("blue", "red", "green")}}

\item{points.alpha}{\code{numeric(1)} indicating the alpha to be
applied to the points. Default value is \code{1}. Setting
\code{points.alpha = 0.7} is helpful when plotting labels with
the points}
}
\value{
a \code{ggplot2} plot with the cross-sectional output
}
\description{
Create a basic cross-sectional plot using standard evaluation
of the variable names
}
\examples{
data(mtcars)
crossplot(mtcars, x.var = "mpg", y.var = "hp", size.var = "wt",
          shapes.var = "cyl")
##Control for (partial out) qsec
p <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
              shapes.var = "cyl", control.vars = "qsec")
print(p)
p$control.vars
}
