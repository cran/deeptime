## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center")

## ----setup, message = FALSE---------------------------------------------------
library(deeptime)
# Load ggplot2
library(ggplot2)
# Load ggtree
library(ggtree)
# Load paleotree for some example data
library(paleotree)
data(RaiaCopesRule)

## -----------------------------------------------------------------------------
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  theme_classic(base_size = 20)
ggarrange2(p1, debug = TRUE)

## -----------------------------------------------------------------------------
p1 <- ggplot(ammoniteTraitsRaia) +
  geom_point(aes(x = Log_D, y = FD)) +
  labs(x = "Body size", y = "Suture complexity") +
  theme_classic()

p2 <- ggplot(ammoniteTraitsRaia) +
  geom_point(aes(x = Log_D, y = log_dur)) +
  labs(x = "Body size", y = "Stratigraphic duration (myr)") +
  theme_classic()
gg1 <- ggarrange2(p1, p2, widths = c(2, 1), draw = FALSE)

## -----------------------------------------------------------------------------
p3 <- ggtree(ammoniteTreeRaia, position = position_nudge(x = -ammoniteTreeRaia$root.time)) +
  coord_geo(
    xlim = c(-415, -66), ylim = c(-2, Ntip(ammoniteTreeRaia)), pos = "bottom",
    size = 4, abbrv = FALSE, neg = TRUE
  ) +
  scale_x_continuous(breaks = seq(-425, -50, 25), labels = -seq(-425, -50, 25)) +
  theme_tree2() +
  theme(plot.margin = margin(7, 11, 7, 11))
ggarrange2(gg1, p3, nrow = 2, heights = c(1, 2))

## -----------------------------------------------------------------------------
ggarrange2(p1, p2, p3,
  layout = matrix(c(1, 2, 0, 3), nrow = 2, byrow = TRUE),
  widths = c(1, 3)
)

