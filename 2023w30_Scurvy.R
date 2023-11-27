# TidyTuesday Challenge
# 2023 week 30
# 2023-07-31
# Scurvy
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-07-25

# Load packages ----

library(tidyverse)
library(showtext)
library(patchwork)

# Import fonts ----

# Roboto Condensed for plot text
font_add_google("Roboto Condensed", "Roboto Condensed")

# Bangers for plot title
font_add_google("Bangers", "Bangers")

showtext_auto()

# Download the dataset ----

scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

# Explore the dataset ----

glimpse(scurvy)

# Data cleaning and prep ----

scurvy_clean <- scurvy |>
  # remove dosing regimen column
  select(-dosing_regimen_for_scurvy) |>
  # transform from wide to long format
  pivot_longer(cols = gum_rot_d6:fit_for_duty_d6,
               names_to = "item",
               values_to = "value") |>
  # separate "value" column into "score" + "grade"
  separate(col = value,
           into = c("score", "grade"),
           convert = TRUE) |>
  # remove "_d6" suffix in item column
  # replace "_" with white spaces
  # transform character strings into sentences
  mutate(item = str_remove_all(item, "_d6"),
         across(treatment:item, ~str_replace_all(., pattern = "_", replacement = " ")),
         across(c(treatment, item, grade), ~str_to_sentence(.))) |>
  # transform study_id into factor
  # arrange factor levels by order of appearance
  mutate(study_id = fct_inseq(as_factor(study_id)),
         across(treatment:item, ~fct_inorder(.)))

# Overall improvement ----

overall_improvement <- scurvy_clean |>
  # remove "Fit for duty" column
  filter(item != "Fit for duty") |>
  # calculate total score by treatment
  summarise(total = sum(score),
            .by = treatment) |>
  # calculate overall improvement score as %
  mutate(improvement_score = 100 * (24-total) / 24) |>
  # arrange data by increasing improvement score
  arrange(improvement_score)

# Create plot - text ----

# Create table with text and x/y positions for plot
p_text <- tibble(
  x = 0,
  y = c(6, 4:2, 0:-3, -5, -6),
  text = c("In 1757, the cause of scurvy was unknown ...",
           "Aboard HMS Salisbury, the ship's surgeon, James Lind,",
           "tested 6 different treatments in 12 seamen with",
           "symptomatic scurvy.",
           "After six days of therapy, he noted the severity of several",
           "symptoms, including rotting of the gums, skin sores,",
           "weakness of the knees, and lassitude, using a scale ranging",
           "from 0 (none) to 3 (severe).",
           "The figure shows overall improvement of symptoms in the",
           "treated seamen."
           )
  )

# Create p0 plot with text
p0 <- ggplot() +
  geom_text(data = p_text,
            aes(x = x, y = y, label = text),
            family = "Roboto Condensed", colour = "white", hjust = 0, size = 18) +
  xlim(0, 10) +
  ylim(-10, 10) +
  labs(title = "Curing scurvy") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#003049", color  = "#003049"),
        plot.background = element_rect(fill = "#003049", color  = "#003049"),
        plot.title = element_text(family = "Bangers", colour = "white",
                                  size = 100, hjust = 0.5, margin = margin(t = 20)))

# Create plot - overall improvement score ----

# Create data points to draw bars from 0 to 100
p1_bars <- overall_improvement |>
  # keep treatment column
  select(treatment) |>
  # add a row id column named "y"
  rowid_to_column(var = "y") |>
  # repeat each row 101 times (0 to 100)
  slice(rep(1:n(), each = 101)) |>
  # add a column with positions to draw bars (for each of the 6 treatments)
  mutate(x = rep(0:100, times = 6))

# Create data points to draw bars for improvement scores
p1_values <- overall_improvement |>
  # round improvement score %
  mutate(improvement_score = round(improvement_score)) |>
  # select treatment + improvement score columns
  select(treatment, improvement_score) |>
  # join p1_bars to get positions for bars from 0 to score
  left_join(p1_bars) |>
  # extract max score for each treatment
  mutate(max_score = max(improvement_score),
         .by = treatment) |>
  # remove rows when x > max_score
  filter(x <= max_score) |>
  # order columns
  select(y, treatment, x)

# Create p1 plot with coloured bars
p1 <- ggplot() +
  geom_segment(data = p1_bars,
               aes(x = x, xend = x,
                   y = y - 0.25, yend = y + 0.25,
                   colour = x),
               linewidth = 1, alpha = 0.4,
               show.legend = FALSE) +
  geom_segment(data = p1_values,
               aes(x = x, xend = x,
                   y = y - 0.25, yend = y + 0.25,
                   colour = x),
               linewidth = 1,
               show.legend = FALSE) +
  geom_text(data = p1_values |> distinct(y, treatment),
            aes(x = 0, y = y + 0.4, label = treatment),
            size = 15, colour = "white", hjust = 0,
            family = "Roboto Condensed") +
  scale_colour_gradient2(low = "#d62828", mid = "#f77f00", high = "#fcbf49",
                         midpoint = 50) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#003049", color  = "#003049"),
        plot.background = element_rect(fill = "#003049", color  = "#003049"))

# Assemble plots ----

p <- p0 + p1 +
  plot_annotation(
    caption = "#TidyTuesday 2023 week 30 | Data from {medicaldata} | Jonathan Kitt",
    theme = theme(
      panel.background = element_rect(fill = "#003049", color  = "#003049"),
      plot.background = element_rect(fill = "#003049", color  = "#003049"),
      plot.caption = element_text(colour = "white", hjust = 0.5, size = 30,
                                  family = "Roboto Condensed")
      )
    )

# Export plot ----

ggsave("figs/tt_2023_w30_scurvy.png", p, dpi = 320, width = 12, height = 6)
