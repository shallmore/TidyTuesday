# TidyTuesday Challenge
# 2023 week 31
# 2023-07-31
# US State names
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-08-01
# https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# https://www.r-bloggers.com/2015/05/geojson-hexagonal-statebins-in-r/

# Load packages ----

library(ggtext)
library(tidyverse)
library(rgdal)
library(rgeos)
library(showtext)
library(patchwork)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Import fonts ----

# Roboto Condensed for plot text
font_add_google("Roboto Condensed", "Roboto Condensed")

# Bangers for plot title
font_add_google("Bangers", "Bangers")

showtext_auto()


# Import the datasets ----

states <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv')
us_hex <- readOGR("raw/us_states_hexgrid.geojson")

# Explore the dataset ----

glimpse(states)

# Data cleaning and prep ----

# Land to water area ratios
land_to_water_ratios <- states |>
  # calculate land/total and water/total area ratios
  mutate(land_area_ratio = round(land_area_km2 / total_area_km2, 3),
         water_area_ratio = round(1 - land_area_ratio, 3)) |>
  # select columns
  select(id = postal_abbreviation, state, land_area_ratio, water_area_ratio)

# Prepare data to create hex map of land to water ratio per US state
us_clean <- us_hex |>
  # transform hex data into table
  fortify(region = "iso3166_2") |>
  # transform into tibble format
  as_tibble() |>
  # select columns
  select(id, long, lat) |>
  # join land to water ratios values
  left_join(land_to_water_ratios) |>
  # remove District of Columbia
  filter(id != "DC") |>
  # add column with point order
  mutate(pt_nb = rep(1:7, times = 50), .before = long) |>
  # group by state id
  group_by(id) |>
  # calculate parameters for each state
  mutate(
    # total area for reactangle around hex
    area_rect = (long[pt_nb == 3] - long[pt_nb == 5]) * (lat[pt_nb == 1] - lat[pt_nb == 4]),
    # slope of upper hex triangle
    slope = (lat[pt_nb == 6] - lat[pt_nb == 1]) / (long[pt_nb == 6] - long[pt_nb == 1]),
    # y coordinate for horizontal border btwn land/water
    split_y = ((land_area_ratio * area_rect) / (long[pt_nb == 2] - long[pt_nb == 6]) + lat[pt_nb == 4]),
    # determine type of split : 1 if split line below upper triangle / 2 if not
    split_type = case_when(split_y <= lat[pt_nb == 2] ~ 1, TRUE ~ 2),
    # calculate x coordinates for horizontal border btwn land/water
    split_x1 = case_when(split_type == 1 ~ min(long),
                         split_type == 2 ~ long[pt_nb == 6] + ((split_y - lat[pt_nb == 6]) / slope)),
    split_x2 = case_when(split_type == 1 ~ max(long),
                         split_type == 2 ~ long[pt_nb == 1] + (long[pt_nb == 1] - split_x1))
    )

# Subset us_clean data for type 1 split
us_clean_type1 <- us_clean |>
  # filter data
  filter(split_type == 1) |>
  # create new columns to keep points needed for plot
  mutate(pt1_x = long[pt_nb == 1], pt1_y = lat[pt_nb == 1],
         pt2_x = long[pt_nb == 2], pt2_y = lat[pt_nb == 2],
         pt3_x = unique(split_x2), pt3_y = unique(split_y),
         pt4_x = unique(split_x1), pt4_y = unique(split_y),
         pt5_x = long[pt_nb == 6], pt5_y = lat[pt_nb == 6],
         pt6_x = pt1_x, pt6_y = pt1_y) |>
  # select columns
  select(id, pt1_x:pt6_y) |>
  # ungroup data
  ungroup() |>
  # pivot to long format
  pivot_longer(cols = -id, names_to = "pt", values_to = "value") |>
  # separate "pt" column
  separate(col = pt, into = c("pt_nb", "xy"), sep = "_") |>
  # remove "pt" string from pt_nb column
  mutate(pt_nb = str_remove_all(pt_nb, "pt")) |>
  # keep distinct rows
  distinct() |>
  # pivot to wide format
  pivot_wider(id_cols = id:pt_nb, names_from = "xy", values_from = "value")

# Subset us_clean data for type 2 split
us_clean_type2 <- us_clean |>
  # filter data
  filter(split_type == 2) |>
  # create new columns to keep points needed for plot
  mutate(pt1_x = long[pt_nb == 1], pt1_y = lat[pt_nb == 1],
         pt2_x = unique(split_x2), pt2_y = unique(split_y),
         pt3_x = unique(split_x1), pt3_y = unique(split_y),
         pt4_x = pt1_x, pt4_y = pt1_y) |>
  # select columns
  select(id, pt1_x:pt4_y) |>
  # ungroup data
  ungroup() |>
  # pivot to long format
  pivot_longer(cols = -id, names_to = "pt", values_to = "value") |>
  # separate "pt" column
  separate(col = pt, into = c("pt_nb", "xy"), sep = "_") |>
  # remove "pt" string from pt_nb column
  mutate(pt_nb = str_remove_all(pt_nb, "pt")) |>
  # keep distinct rows
  distinct() |>
  # pivot to wide format
  pivot_wider(id_cols = id:pt_nb, names_from = "xy", values_from = "value")

# Calculate centres of polygons to plot state labels
centres <- gCentroid(us_hex, byid = TRUE) |>
  as_tibble()

labels <- us_hex@data$iso3166_2

hex_labels <- tibble(id = labels,
                     centres) |>
  filter(id != "DC")

# Clean global environment
rm(centres, labels, land_to_water_ratios, states, us_hex)

# Create plot ----

p <- ggplot() +
  geom_polygon(data = us_clean, aes(x = long, y = lat, group = id),
               colour = NA, fill = "#48bf91") +
  geom_polygon(data = us_clean_type1, aes(x = x, y = y, group = id),
               colour = NA, fill = "#0076be") +
  geom_polygon(data = us_clean_type2, aes(x = x, y = y, group = id),
               colour = NA, fill = "#0076be") +
  geom_polygon(data = us_clean, aes(x = long, y = lat, group = id),
               colour = "white", fill = NA, linewidth = 0.5) +
  geom_text(data = hex_labels, aes(x = x, y = y, label = id),
            family = "Roboto Condensed", colour = "black", size = 16) +
  coord_map() +
  labs(title = "<span style='color:#48bf91;'>Land</span> to <span style='color:#0076be;'>water</span> surface ratios in the U.S.",
       caption = "#TidyTuesday 2023 week 31 | Data from Wikipedia | Jonathan Kitt") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = NA),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.title = element_markdown(family = "Bangers", size = 90,
                                      hjust = 0.5, colour = "white",
                                      margin = margin(t = 10)),
        plot.caption = element_text(family = "Roboto Condensed", colour = "white", size = 30,
                                    hjust = 0.5,
                                    margin = margin(t = 5, b = 5)))

# Export plot ----

ggsave("figs/tt_2023_w31_us.png", p, dpi = 320, width = 12, height = 6)
