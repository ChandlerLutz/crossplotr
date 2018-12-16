## c:/Dropbox/Rpackages/crossplotr/tests/testthat/test_mtcars.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-16


data(mtcars)

context("mtcars data")

test_that("crossplot example works", {
  data(mtcars)
  crossplot(mtcars, x.var = "mpg", y.var = "hp", size.var = "wt",
            shapes.var = "cyl")
  ##Control for (partial out) qsec
  p <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
                 shapes.var = "cyl", control.vars = "qsec")
  expect_true("ggplot" %in% class(p))
})

test_that("crossplot_stats works", {
  data(mtcars)
  p <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
                 shapes.var = "cyl", control.vars = "qsec")
  expect_true("ggplot" %in% class(p))
  plot.stats <- crossplot_stats(p, weighted = TRUE)
  expect_true("data.frame" %in% class(plot.stats[[1]]))

})



test_that("crossplot_print_stats works", {
  data(mtcars)
  p <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
                 shapes.var = "cyl")
  ##Print weighted regression line and weighted stats
  crossplot_print_stats(p, text.pos = c(200, 25), weighted = TRUE)
  ##Print unweighted regression line and unweighted stats
  crossplot_print_stats(p, text.pos = c(200, 25), weighted = FALSE)

  ##passing other arguments to cowplot::draw_label.
  ##See ?cowplot::draw_label for more details
  p <- crossplot_print_stats(p, text.pos = c(200, 25), weighted = FALSE,
                             fontface = "bold")
  expect_true("ggplot" %in% class(p))

  ## -- Controlling for qsec weighted by wt -- ##

  ##First see the results using a normal regression
  mod <- lm(mpg ~ hp + qsec, mtcars, weights = wt)
  library(sandwich); library(lmtest)
  coeftest(mod, vcov = sandwich)
  ##Now in a plot
  p2 <- crossplot(mtcars, x.var = "hp", y.var = "mpg", size.var = "wt",
                  shapes.var = "cyl", control.vars = "qsec")
  crossplot_print_stats(p2, text.pos = c(0, 7), weighted = TRUE)
  expect_true("ggplot" %in% class(p2))
})

