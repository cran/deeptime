## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center",
                      out.width = "100%")

## ----message = FALSE, warning = FALSE-----------------------------------------
# Load deeptime
library(deeptime)
# Load other packages
library(ggplot2)
library(dplyr)
# Load ggtree
library(ggtree)
# Load phytools for some example data
library(phytools)
data(mammal.tree)
# Load paleotree for some example fossil data
library(paleotree)
data(RaiaCopesRule)

## -----------------------------------------------------------------------------
p <- ggtree(mammal.tree) +
  coord_geo(xlim = c(-75, 0), ylim = c(-2, Ntip(mammal.tree)),
            neg = TRUE, abbrv = FALSE) +
  scale_x_continuous(breaks = seq(-80, 0, 20), labels = abs(seq(-80, 0, 20))) +
  theme_tree2()
revts(p)

## -----------------------------------------------------------------------------
ggtree(ceratopsianTreeRaia,
       position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
  coord_geo(
    xlim = c(-163.5, -66), ylim = c(-2, Ntip(ceratopsianTreeRaia)),
    pos = list("bottom", "bottom"), skip = c("Paleocene", "Middle Jurassic"),
    dat = list("epochs", "periods"), abbrv = FALSE,
    size = list(4, 5), neg = TRUE, center_end_labels = TRUE
  ) +
  scale_x_continuous(breaks = -rev(epochs$max_age),
                     labels = rev(epochs$max_age)) +
  theme_tree2() +
  theme(plot.margin = margin(7, 11, 7, 11))

## ----eval = FALSE-------------------------------------------------------------
#  revts(ggtree(mammal.tree)) +
#    coord_geo_radial(dat = "stages") +
#    scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20)),
#                       expand = expansion(mult = c(0.05, 0))) +
#    scale_y_continuous(guide = NULL, expand = expansion(mult = c(0.01, 0.01))) +
#    theme_classic()

## ----eval = FALSE-------------------------------------------------------------
#  revts(ggtree(mammal.tree)) +
#    coord_geo_radial(
#        dat = list("stages", "periods"), alpha = .5, lty = "dashed",
#        prop = list(0.66, .34), start = 2 * pi, end = 1.75 * pi, direction = 1,
#    ) +
#    scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20)),
#                       expand = expansion(mult = c(0.05, 0))) +
#    scale_y_continuous(guide = NULL, expand = expansion(mult = c(0.01, 0.01))) +
#    theme_classic()

## ----eval = FALSE-------------------------------------------------------------
#  revts(ggtree(mammal.tree)) +
#    coord_radial(theta = "y", start = -0.5 * pi, end = 1.25 * pi) +
#    scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20)),
#                       expand = expansion(mult = c(0.05, 0)),
#                       guide = guide_geo("epochs", neg = TRUE, rot = -90,
#                                         size = "auto",
#                                         height = unit(1, "line"))) +
#    scale_y_continuous(guide = NULL, expand = expansion(mult = c(0.01, 0.01))) +
#    theme_classic()

## ----eval = FALSE-------------------------------------------------------------
#  revts(ggtree(mammal.tree)) +
#    coord_radial(theta = "y", start = -0.5 * pi, end = 1.25 * pi) +
#    scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20)),
#                       expand = expansion(mult = c(0.05, 0)),
#                       guide = guide_axis_stack(guide_geo("epochs", neg = TRUE,
#                                                          rot = -90, size = "auto",
#                                                          height = unit(1, "line")),
#                                                guide_axis(),
#                                                spacing = unit(0, "line"))) +
#    scale_y_continuous(guide = NULL, expand = expansion(mult = c(0.01, 0.01))) +
#    theme_classic()

## ----eval = FALSE-------------------------------------------------------------
#  revts(ggtree(mammal.tree)) +
#    coord_geo_radial(dat = "stages", fill = c("grey90", "grey95"), end = 1.49 * pi) +
#    scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20)),
#                       expand = expansion(mult = c(0.05, 0))) +
#    scale_y_continuous(guide = NULL, expand = expansion(mult = c(0.01, 0.05))) +
#    theme_classic() +
#    guides(r = guide_axis_stack(guide_geo("epochs", neg = TRUE,
#                                          rot = -90, size = "auto",
#                                          height = unit(1, "line")),
#                                guide_axis(),
#                                spacing = unit(0, "line"))) +
#    theme(axis.text.y = element_text(color = "black"))

