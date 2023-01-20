## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center")

## ----message = FALSE----------------------------------------------------------
# Load deeptime
library(deeptime)
# Load ggplot for making plots
# It has some example data too
library(ggplot2)

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(disp, wt)) +
  geom_point() +
  coord_trans_flip(x = "sqrt", y = "log10") +
  theme_classic()

## -----------------------------------------------------------------------------
# make transformer
library(ggforce)
trans <- linear_trans(shear(50, 0))

# set up data to be plotted
square <- data.frame(disp = c(min(mtcars$disp), min(mtcars$disp),
                              max(mtcars$disp), max(mtcars$disp)),
                     wt = c(min(mtcars$wt), max(mtcars$wt),
                              max(mtcars$wt), min(mtcars$wt)))

# plot data normally
library(ggplot2)
ggplot(mtcars, aes(disp, wt)) +
  geom_polygon(data = square, fill = NA, color = "black") +
  geom_point(color = 'black') +
  coord_cartesian() +
  theme_classic()
# plot data with transformation
ggplot(mtcars, aes(disp, wt)) +
  geom_polygon(data = square, fill = NA, color = "black") +
  geom_point(color = 'black') +
  coord_trans_xy(trans = trans, expand = TRUE) +
  theme_classic()

