## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center")

## ----message = FALSE----------------------------------------------------------
# Load deeptime
library(deeptime)
# Load other packages
library(ggplot2)
library(dplyr)
# Load divDyn for coral data
library(divDyn)
data(corals)
# Load gsloid for oxygen isotope data
library(gsloid)

## ----message = FALSE----------------------------------------------------------
# this is not a proper diversity curve but it gets the point across
coral_div <- corals %>%
  filter(stage != "") %>%
  group_by(period, stage) %>%
  summarise(n = n()) %>%
  mutate(stage_age = (stages$max_age[match(stage, stages$name)] +
    stages$min_age[match(stage, stages$name)]) / 2)

## -----------------------------------------------------------------------------
ggplot(coral_div) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(xlim = c(250, 0), ylim = c(0, 1700)) +
  theme_classic()

## -----------------------------------------------------------------------------
ggplot(lisiecki2005) +
  geom_line(aes(x = d18O, y = Time / 1000), orientation = "y") +
  scale_y_reverse("Time (Ma)") +
  scale_x_reverse() +
  coord_geo(
    dat = "Geomagnetic Polarity Chron", xlim = c(6, 2), ylim = c(6, 0),
    pos = "left", rot = 90
  ) +
  theme_classic()

## -----------------------------------------------------------------------------
# uses the oxygen isotope data from above
ggplot(lisiecki2005) +
  geom_line(aes(x = d18O, y = Time / 1000), orientation = "y") +
  scale_y_reverse("Time (Ma)") +
  scale_x_reverse() +
  coord_geo(
    dat = list("Geomagnetic Polarity Chron", "Planktic foraminiferal Primary Biozones"),
    xlim = c(6, 2), ylim = c(5.5, 0), pos = list("l", "r"),
    rot = 90, skip = "PL4", size = list(5, 4)
  ) +
  theme_classic()

## -----------------------------------------------------------------------------
# uses the coral diversity data from above
ggplot(coral_div) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(
    dat = list("periods", "eras"), xlim = c(250, 0), ylim = c(0, 1700),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE)
  ) +
  theme_classic()

## ----message = FALSE----------------------------------------------------------
# uses the coral occurrence data from above
coral_div_diet <- corals %>%
  filter(stage != "") %>%
  group_by(diet, stage) %>%
  summarise(n = n()) %>%
  mutate(stage_age = (stages$max_age[match(stage, stages$name)] +
    stages$min_age[match(stage, stages$name)]) / 2)

ggplot(coral_div_diet) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(xlim = c(250, 0)) +
  theme_classic() +
  facet_wrap(~diet, nrow = 3)

## ----message = FALSE----------------------------------------------------------
ggplot(coral_div_diet) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(xlim = c(250, 0)) +
  theme_classic() +
  facet_wrap(~diet, nrow = 3, scales = "free_x")

## -----------------------------------------------------------------------------
ggplot(coral_div) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(
    dat = "periods", xlim = c(250, 0), ylim = c(0, 1700),
    abbrv = FALSE, size = "auto", fittext_args = list(size = 20)
  ) +
  theme_classic()

## ----message = FALSE----------------------------------------------------------
# use the coral occurrence data from above
coral_div_dis <- corals %>%
  filter(period != "") %>%
  group_by(diet, period) %>%
  summarise(n = n()) %>%
  mutate(period_age = (periods$max_age[match(period, periods$name)] +
    periods$min_age[match(period, periods$name)]) / 2) %>%
  arrange(-period_age)

ggplot(coral_div_dis) +
  geom_col(aes(x = period, y = n, fill = diet)) +
  scale_x_discrete("Period",
    limits = unique(coral_div_dis$period), labels = NULL,
    expand = expansion(add = .5)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d("Diet") +
  ylab("Coral Genera") +
  coord_geo(expand = TRUE, skip = NULL, abbrv = TRUE) +
  theme_classic() +
  theme(axis.ticks.length.x = unit(0, "lines"))

## -----------------------------------------------------------------------------
eras_custom <- data.frame(
  name = c("Mesozoic", "Cenozoic"), max_age = c(0.5, 3.5),
  min_age = c(3.5, 6.5), color = c("#67C5CA", "#F2F91D")
)

ggplot(coral_div_dis) +
  geom_col(aes(x = period, y = n, fill = diet)) +
  scale_x_discrete(NULL,
    limits = unique(coral_div_dis$period), labels = NULL,
    expand = expansion(add = .5)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d("Diet") +
  ylab("Coral Genera") +
  coord_geo(
    dat = list("periods", eras_custom), pos = c("b", "b"), expand = TRUE,
    skip = NULL, abbrv = list(TRUE, FALSE), dat_is_discrete = list(FALSE, TRUE)
  ) +
  theme_classic() +
  theme(axis.ticks.length.x = unit(0, "lines"))

