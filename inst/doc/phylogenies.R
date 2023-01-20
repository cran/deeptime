## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center")

## ----message = FALSE----------------------------------------------------------
# Load deeptime
library(deeptime)
# Load tidyverse (includes ggplot2)
library(tidyverse)
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
  coord_geo(xlim = c(-75, 0), ylim = c(-2, Ntip(mammal.tree)), neg = TRUE, abbrv = FALSE) +
  scale_x_continuous(breaks=seq(-80, 0, 20), labels=abs(seq(-80, 0, 20))) +
  theme_tree2()
revts(p)

## -----------------------------------------------------------------------------
ggtree(ceratopsianTreeRaia, position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
  coord_geo(xlim = c(-163.5, -66), ylim = c(-2, Ntip(ceratopsianTreeRaia)),
            pos = list("bottom", "bottom"), skip = c("Paleocene", "Middle Jurassic"),
            dat = list("epochs", "periods"), abbrv = FALSE,
            size = list(4, 5), neg = TRUE, center_end_labels = TRUE) +
  scale_x_continuous(breaks = -rev(epochs$max_age), labels = rev(epochs$max_age)) +
  theme_tree2() +
  theme(plot.margin = margin(7, 11, 7, 11))

## -----------------------------------------------------------------------------
revts(ggtree(mammal.tree)) +
  coord_geo_polar(dat = "stages") +
  scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20)))

## ----message = FALSE----------------------------------------------------------
revts(ggtree(mammal.tree)) +
  coord_geo_polar(dat = list("stages", "periods"), alpha = .5,
                  prop = list(0.66, .34), start = pi/4, lty = "dashed") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(breaks = seq(-60, 0, 20), labels = abs(seq(-60, 0, 20))) +
  theme(axis.text.r = element_text(size = 3.5, hjust = .25, vjust = .5))

