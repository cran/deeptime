## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, out.width = "100%", dev.args = list(png = list(type = "cairo-png")),
                      fig.width = 7, fig.height = 6, fig.align = "center")

## ----echo = FALSE-------------------------------------------------------------
#library(devtools)
#install_github("palaeoverse/rmacrostrat")

## ----message = FALSE----------------------------------------------------------
# Load deeptime
library(deeptime)
# Load rmacrostrat to get data
library(rmacrostrat)
# Load packages for data visualization
library(ggplot2)
library(ggrepel)

## -----------------------------------------------------------------------------
# Using the column ID, retrieve the units in the San Juan Basin column
san_juan_units <- get_units(column_id = 489, interval_name = "Cretaceous")
# See the column names and number of rows
colnames(san_juan_units)
nrow(san_juan_units)

## ----fig.height = 8-----------------------------------------------------------
# Specify x_min and x_max in dataframe
san_juan_units$x_min <- 0
san_juan_units$x_max <- 1
# Tweak values for overlapping units
san_juan_units$x_max[10] <- 0.5
san_juan_units$x_min[11] <- 0.5

# Add midpoint age for plotting
san_juan_units$m_age <- (san_juan_units$b_age + san_juan_units$t_age) / 2

ggplot(san_juan_units, aes(ymin = b_age, ymax = t_age,
                           xmin = x_min, xmax = x_max)) +
  # Plot units, colored by rock type
  geom_rect(fill = san_juan_units$color, color = "black") +
  # Add text labels
  geom_text_repel(aes(x = x_max, y = m_age, label = unit_name),
                  size = 3.5, hjust = 0, force = 2,
                  min.segment.length = 0, direction = "y",
                  nudge_x = rep_len(x = c(2, 3), length.out = 17)) +
  # Reverse direction of y-axis
  scale_y_reverse(limits = c(145, 66), n.breaks = 10, name = "Time (Ma)") +
  # Theming
  theme_classic() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # Add geological time scale
  coord_geo(pos = "left", dat = list("stages"), rot = 90)

## -----------------------------------------------------------------------------
# Get lithology definitions
liths <- def_lithologies()
head(liths)

## -----------------------------------------------------------------------------
# Count lithologies for each unit
sapply(san_juan_units$lith, nrow)
# Inspect one with multiple rows
san_juan_units$lith[16]

## -----------------------------------------------------------------------------
# Get the primary lithology for each unit
san_juan_units$lith_prim <- sapply(san_juan_units$lith, function(df) {
  df$name[which.max(df$prop)]
})
table(san_juan_units$lith_prim)

## -----------------------------------------------------------------------------
# Assign pattern code
san_juan_units$pattern <- factor(liths$fill[match(san_juan_units$lith_prim, liths$name)])
table(san_juan_units$pattern, exclude = NULL)

## ----fig.height = 8-----------------------------------------------------------
# Plot with pattern fills
ggplot(san_juan_units, aes(ymin = b_age, ymax = t_age,
                           xmin = x_min, xmax = x_max)) +
  # Plot units, patterned by rock type
  geom_rect(aes(fill = pattern), color = "black") +
  scale_fill_geopattern() +
  # Add text labels
  geom_text_repel(aes(x = x_max, y = m_age, label = unit_name),
                  size = 3.5, hjust = 0, force = 2,
                  min.segment.length = 0, direction = "y",
                  nudge_x = rep_len(x = c(2, 3), length.out = 17)) +
  # Reverse direction of y-axis
  scale_y_reverse(limits = c(145, 66), n.breaks = 10, name = "Time (Ma)") +
  # Theming
  theme_classic() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # Add geological time scale
  coord_geo(pos = "left", dat = list("stages"), rot = 90)

## ----fig.height = 8-----------------------------------------------------------
# Plot using ggpattern
library(ggpattern)
ggplot(san_juan_units, aes(ymin = b_age, ymax = t_age,
                           xmin = x_min, xmax = x_max)) +
  # Plot units, patterned by rock type
  geom_rect_pattern(aes(pattern_type = pattern), pattern = "geo",
                    pattern_scale = 4) +
  scale_pattern_type_identity() +
  # Add text labels
  geom_text_repel(aes(x = x_max, y = m_age, label = unit_name),
                  size = 3.5, hjust = 0, force = 2,
                  min.segment.length = 0, direction = "y",
                  nudge_x = rep_len(x = c(2, 3), length.out = 17)) +
  # Reverse direction of y-axis
  scale_y_reverse(limits = c(145, 66), n.breaks = 10, name = "Time (Ma)") +
  # Theming
  theme_classic() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # Add geological time scale
  coord_geo(pos = "left", dat = list("stages"), rot = 90)

## ----fig.height = 8-----------------------------------------------------------
ggplot(san_juan_units, aes(ymin = b_age, ymax = t_age,
                           xmin = x_min, xmax = x_max)) +
  # Plot units, patterned by rock type
  geom_rect_pattern(aes(pattern_type = pattern), pattern = "geo",
                    pattern_color = "black", pattern_fill = "white",
                    fill = "white", pattern_scale = 4) +
  scale_pattern_type_identity() +
  # Add text labels
  geom_text_repel(aes(x = x_max, y = m_age, label = unit_name),
                  size = 3.5, hjust = 0, force = 2,
                  min.segment.length = 0, direction = "y",
                  nudge_x = rep_len(x = c(2, 3), length.out = 17)) +
  # Reverse direction of y-axis
  scale_y_reverse(limits = c(145, 66), n.breaks = 10, name = "Time (Ma)") +
  # Theming
  theme_classic() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # Add geological time scale
  coord_geo(pos = "left", dat = list("stages"), rot = 90)

## ----fig.height = 8-----------------------------------------------------------
ggplot(san_juan_units, aes(ymin = b_age, ymax = t_age,
                           xmin = x_min, xmax = x_max)) +
  # Plot units, patterned by rock type
  geom_rect_pattern(aes(pattern_type = pattern, fill = color,
                        pattern_fill = color), pattern = "geo",
                    pattern_color = "black", pattern_scale = 4) +
  scale_pattern_type_identity() +
  # Use the raw color values
  scale_fill_identity() +
  scale_pattern_fill_identity() +
  # Add text labels
  geom_text_repel(aes(x = x_max, y = m_age, label = unit_name),
                  size = 3.5, hjust = 0, force = 2,
                  min.segment.length = 0, direction = "y",
                  nudge_x = rep_len(x = c(2, 3), length.out = 17)) +
  # Reverse direction of y-axis
  scale_y_reverse(limits = c(145, 66), n.breaks = 10, name = "Time (Ma)") +
  # Theming
  theme_classic() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # Add geological time scale
  coord_geo(pos = "left", dat = list("stages"), rot = 90)

