library(tidyverse)
library(visitalaneysluverds)
library(ggridges)
library(metill)
library(here)
library(ggthemes)
theme_set(theme_metill())

d <- vnv(date_unity = clock::date_build(1988, 5,))

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

plot_dat <- d |> 
  mutate(
    month = month(date),
    year = year(date)
  ) |> 
  mutate(
    change = c(0, diff(log(cpi))),
    change = exp(change)
  ) |> 
  mutate(
    last_year = today() - years(1),
    last_year = floor_date(last_year, "month"),
    within_last_year = 1 * (date > last_year),
    change_last_year = ifelse(within_last_year == 1, change, NA_real_),
    month_label = month(date, label = TRUE, abbr = FALSE),
    month_ordered = fct_reorder(as_factor(month), date, .fun = max) |> 
      as.numeric()
  ) |> 
  filter(
    # year >= 2000
  )

icelandic_months <- plot_dat |> 
  slice_tail(n = 12) |> 
  pull(month_label) |> 
  as.character()

min_val <- min(plot_dat$change) - 1
max_val <- max(plot_dat$change) - 1

largest_val <- pmax(abs(min_val), abs(max_val))

range_val <- largest_val * c(-1, 1)

p <- plot_dat |> 
  ggplot(aes(change, month_ordered, group = month, fill = sin((month-0.5)/12 * pi))) +
  geom_vline(
    xintercept = 1, 
    lty = 2, 
    alpha = 0.4, 
    linewidth = 0.3
  ) +
  geom_density_ridges(
    scale = 0.9,
    rel_min_height = 0.01,
    alpha = 0.7
  ) +
  geom_segment(
    aes(
      x = change_last_year, 
      xend = change_last_year,
      y = month_ordered, 
      yend = month_ordered + 0.95
    ),
    linewidth = 1.4
  ) +
  geom_rangeframe(sides = "l") +
  scale_x_tufte(
    expand = expansion(),
    labels = function(x) hlutf(x - 1),
    trans = "log10",
    breaks = tufte_breaks(plot_dat$change)
  ) +
  scale_y_continuous(
    breaks = 1:12,
    labels = icelandic_months
  ) +
  scale_fill_distiller(direction = 1) +
  theme(
    legend.position = "none",
    axis.line.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Hvernig eru nýjustu verðbólgutölur í sögulegu samhengi?",
    subtitle = "Mánaðarleg verðbólga undanfarið ár (lóðréttar línur) og söguleg dreifing (1988 - 2023)"
  )


ggsave(
  plot = p,
  filename = here("Figures", "manadarleg_verdbolga.png"),
  width = 8, height = 0.8 * 8, scale = 1.1
)
