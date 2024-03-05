## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width = 7, fig.height = 5, fig.align = "center")

## ----message = FALSE----------------------------------------------------------
# Load deeptime
library(deeptime)
# Load other packages
library(ggplot2)
library(dplyr)
# Load palaeoverse for tetrapod occurrence data
library(palaeoverse)
data(tetrapods)

## -----------------------------------------------------------------------------
# sort the occurrences from most common genera to least common genera
# assume the age is just the mean of the max and min
occdf <- tetrapods %>%
  filter(accepted_rank == "genus") %>%
  select(occurrence_no, accepted_name, max_ma, min_ma) %>%
  mutate(accepted_name = reorder(accepted_name, accepted_name, length)) %>%
  arrange(desc(accepted_name)) %>%
  mutate(age = (max_ma + min_ma) / 2)
# get a reasonable subset of those occurrences
occdf <- occdf[1:300, ]

# plot those occurrences
ggplot(data = occdf) +
  geom_points_range(aes(x = age, y = accepted_name)) +
  theme_classic()

## -----------------------------------------------------------------------------
ggplot(data = occdf) +
  geom_points_range(aes(x = age, y = accepted_name)) +
  scale_x_reverse() +
  coord_geo(pos = list("bottom", "bottom"), dat = list("stages", "periods"),
            abbrv = list(TRUE, FALSE), expand = TRUE, size = "auto") +
  theme_classic()

## ----message = FALSE, include = FALSE-----------------------------------------
set.seed(1234)

## -----------------------------------------------------------------------------
occdf$certainty <- factor(sample(0:1, nrow(occdf), replace = TRUE))

ggplot(data = occdf) +
  geom_points_range(aes(x = age, y = accepted_name,
                        fill = certainty, linetype = certainty), shape = 21) +
  scale_x_reverse() +
  scale_fill_manual(values = c("white", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_geo(pos = list("bottom", "bottom"), dat = list("stages", "periods"),
            abbrv = list(TRUE, FALSE), expand = TRUE, size = "auto") +
  theme_classic()

## -----------------------------------------------------------------------------
occdf$accepted_name <- reorder(occdf$accepted_name, occdf$age, max,
                               decreasing = TRUE)
ggplot(data = occdf) +
  geom_points_range(aes(x = age, y = accepted_name,
                        fill = certainty, linetype = certainty), shape = 21) +
  scale_x_reverse() +
  scale_fill_manual(values = c("white", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_geo(pos = list("bottom", "bottom"), dat = list("stages", "periods"),
            abbrv = list(TRUE, FALSE), expand = TRUE, size = "auto") +
  theme_classic()

## -----------------------------------------------------------------------------
oldest_certain <- occdf %>%
  filter(accepted_name == "Diictodon", certainty == 1) %>%
  pull(age) %>% max()

n_uncertain <- sum(occdf$accepted_name == "Diictodon" & occdf$certainty == 0)

# make the uncertain points all much older
occdf$age[occdf$accepted_name == "Diictodon" & occdf$certainty == 0] <-
  oldest_certain + runif(n_uncertain, 15, 30)

ggplot(data = occdf) +
  geom_points_range(aes(x = age, y = accepted_name,
                        fill = certainty, linetype = certainty), shape = 21) +
  scale_x_reverse() +
  scale_fill_manual(values = c("white", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_geo(pos = list("bottom", "bottom"), dat = list("stages", "periods"),
            abbrv = list(TRUE, FALSE), expand = TRUE, size = "auto") +
  theme_classic()

## -----------------------------------------------------------------------------
ggplot(data = occdf) +
  geom_points_range(aes(x = age, y = accepted_name,
                        fill = certainty, linetype = certainty), shape = 21,
                    background_line = list(linetype = "dashed")) +
  scale_x_reverse() +
  scale_fill_manual(values = c("white", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_geo(pos = list("bottom", "bottom"), dat = list("stages", "periods"),
            abbrv = list(TRUE, FALSE), expand = TRUE, size = "auto") +
  theme_classic()

## ----message = FALSE, include = FALSE-----------------------------------------
library(divDyn)
data(corals)

coral_div_dis <- corals %>%
  filter(period != "") %>%
  group_by(diet, period) %>%
  summarise(n = n()) %>%
  mutate(period_age = (periods$max_age[match(period, periods$name)] +
    periods$min_age[match(period, periods$name)]) / 2) %>%
  arrange(-period_age)

## -----------------------------------------------------------------------------
ggplot(coral_div_dis, aes(x = n, y = diet, fill = period)) +
  geom_col() +
  scale_fill_geo(periods) +
  xlab("Coral Genera") +
  ylab("Diet") +
  theme_classic()

