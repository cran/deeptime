## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center")

## ----message = FALSE----------------------------------------------------------
# Load deeptime
library(deeptime)
# Load other packages
library(ggplot2)
library(dplyr)
# Load dispRity for example data
library(dispRity)
data(demo_data)
# Load paleotree for example data
library(phytools)
data(mammal.tree)
data(mammal.data)

## -----------------------------------------------------------------------------
# make transformer
library(ggforce)
trans <- linear_trans(shear(.75, 0))

# prepare data to be plotted
crinoids <- as.data.frame(demo_data$wright$matrix[[1]][, 1:2])
crinoids$time <- "before extinction"
crinoids$time[demo_data$wright$subsets$after$elements] <- "after extinction"

# a box to outline the trait space
square <- data.frame(V1 = c(-.6, -.6, .6, .6), V2 = c(-.4, .4, .4, -.4))

ggplot() +
  geom_segment(
    data = data.frame(
      x = -.6, y = seq(-.4, .4, .2),
      xend = .6, yend = seq(-0.4, .4, .2)
    ),
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = "dashed", color = "grey"
  ) +
  geom_segment(
    data = data.frame(
      x = seq(-.6, .6, .2), y = -.4,
      xend = seq(-.6, .6, .2), yend = .4
    ),
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = "dashed", color = "grey"
  ) +
  geom_polygon(data = square, aes(x = V1, y = V2), fill = NA, color = "black") +
  geom_point(data = crinoids, aes(x = V1, y = V2), color = "black") +
  coord_trans_xy(trans = trans, expand = FALSE) +
  labs(x = "PCO1", y = "PCO2") +
  theme_classic() +
  facet_wrap(~time, ncol = 1, strip.position = "right") +
  theme(panel.spacing = unit(1, "lines"), panel.background = element_blank())

## ----fig.height = 4-----------------------------------------------------------
crinoids$time <- factor(crinoids$time)
disparity_through_time(time ~ V2 * V1,
  data = crinoids, groups = time, aspect = c(1.5, .6),
  xlim = c(-.6, .6), ylim = c(-.5, .5),
  col.regions = "lightyellow", col.point = c("red", "blue"),
  par.settings = list(
    axis.line = list(col = "transparent"),
    layout.heights =
      list(
        top.padding = -20, main.key.padding = 0,
        key.axis.padding = 0, axis.xlab.padding = 0,
        xlab.key.padding = 0, key.sub.padding = 0,
        bottom.padding = -20
      ),
    layout.widths =
      list(
        left.padding = -10, key.ylab.padding = 0,
        ylab.axis.padding = 0, axis.key.padding = 0,
        right.padding = 0
      )
  )
)

## ----message = FALSE, warning = FALSE-----------------------------------------
mammal.data$label <- rownames(mammal.data)

ggplot(mammal.data, aes(x = bodyMass, y = homeRange, label = label)) +
  geom_phylomorpho(mammal.tree) +
  theme_classic()

