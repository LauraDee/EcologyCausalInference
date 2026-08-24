# Different causal estimands
# by A Heiss https://www.andrewheiss.com/blog/2024/03/21/demystifying-ate-att-atu/#what-about-other-methods
library(tidyverse)
library(ggtext)
library(ggdag)
library(dagitty)
library(gt)
library(broom)
library(marginaleffects)
library(WeightIt)

# Define a nice color palette from {MoMAColors}
# https://github.com/BlakeRMills/MoMAColors
install.packages("devtools")
devtools::install_github("BlakeRMills/MoMAColors")
clrs <- MoMAColors::moma.colors("ustwo")

# Download Mulish from https://fonts.google.com/specimen/Mulish
theme_nice <- function() {
  theme_minimal(base_family = "Mulish") +
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(face = "bold")
    )
}

theme_set(theme_nice())

update_geom_defaults("text", list(family = "Mulish", fontface = "plain"))
update_geom_defaults("label", list(family = "Mulish", fontface = "plain"))
update_geom_defaults(ggdag:::GeomDagText, list(family = "Mulish", fontface = "plain"))
update_geom_defaults(ggtext::GeomRichText, list(family = "Mulish", fontface = "plain"))



basic_po <- tribble(
  ~id, ~age,    ~treated, ~outcome_1, ~outcome_0,
  1,   "Old",   1,        80,         60,
  2,   "Old",   1,        75,         70,
  3,   "Old",   1,        85,         80,
  4,   "Old",   0,        70,         60,
  5,   "Young", 1,        75,         70,
  6,   "Young", 0,        80,         80,
  7,   "Young", 0,        90,         100,
  8,   "Young", 0,        85,         80
) |>
  mutate(
    ice = outcome_1 - outcome_0,
    outcome = ifelse(treated == 1, outcome_1, outcome_0)
  )

basic_po |>
  select(
    id, age, treated, outcome_1, outcome_0, ice, outcome
  ) |>
  gt() |>
  sub_missing(missing_text = "…") |>
  fmt_number(
    columns = c(starts_with("outcome"), ice),
    decimals = 0
  ) |>

  # Column labels
  cols_label(
    id = "ID",
    age = md("$Z_i$"),
    treated = md("$X_i$"),
    outcome_0 = md("$Y^0_i$"),
    outcome_1 = md("$Y^1_i$"),
    outcome = md("$Y_i$"),
    ice = md("$Y^1_i - Y^0_i$")
  ) |>

  # Level 1 spanner labels
  tab_spanner(
    label = "Age", columns = age,
    level = 1, id = "level1_a_po"
  ) |>
  tab_spanner(
    label = "Treated", columns = treated,
    level = 1, id = "level1_b_po"
  ) |>
  tab_spanner(
    label = "Potential outcomes",
    columns = c(outcome_1, outcome_0),
    level = 1, id = "level1_c_po"
  ) |>
  tab_spanner(
    label = "ICE or \\(\\delta_i\\)", columns = ice,
    level = 1, id = "level1_d_po"
  ) |>
  tab_spanner(
    label = "Outcome", columns = outcome,
    level = 1, id = "level1_e_po"
  ) |>

  # Level 2 spanner labels
  tab_spanner(
    label = "Confounder",
    columns = age,
    level = 2, id = "level2_a_po"
  ) |>
  tab_spanner(
    label = "Treatment", columns = treated,
    level = 2, id = "level2_b_po"
  ) |>
  tab_spanner(
    label = "Unobservable",
    columns = c(outcome_1, outcome_0, ice),
    level = 2, id = "level2_c_po"
  ) |>
  tab_spanner(
    label = "Realized", columns = outcome,
    level = 2, id = "level2_d_po") |>

  # Style stuff
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_spanners(spanners = starts_with("level1")),
      cells_column_labels(columns = "id")
    )
  ) |>
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_column_spanners(spanners = starts_with("level2"))
  ) |>

  tab_style(
    style = list(
      cell_fill(color = clrs[4], alpha = 0.5)
    ),
    locations = cells_body(rows = treated == 1)
  ) |>
  tab_footnote(
    footnote = "ICE = individual causal effect",
    locations = cells_column_spanners(spanners = "level1_d_po")
  ) |>
  opt_footnote_marks(marks = "standard") |>
  opt_horizontal_padding(scale = 3) |>
  opt_table_font(font = "Jost")

tribble(
  ~ATE, ~ATT,
  "Effect of mosquito bed nets for everyone in the country", "Effect of mosquito bed nets for people who use bed nets",
  "Effect of military service for typical applicants to the military", "Effect of military service for typical soldiers",
  "Effect of a job training program on all residents in a state", "Effect of a job training program on everyone who used it",
  "Effect of a new cancer medicine on everyone in the world", "Effect of a new cancer medicine on people with cancer"
) |>
  gt() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(v_align = "top"),
    locations = cells_body()
  ) |>
  tab_footnote(
    footnote = html("Example via <a href='https://stats.stackexchange.com/a/455012/3025'>this Cross Validated response</a>"),
    locations = cells_body(columns = ATE, rows = 2)
  ) |>
  opt_footnote_marks(marks = "standard") |>
  opt_horizontal_padding(scale = 3) |>
  opt_table_font(font = "Jost")
