library(tidyverse)
library(forcats)
library(themes360info)
library(ggtext)
library(ragg)
library(here)

dose_labeller <- scales::label_number_si("", accuracy = 0.1)
dose_axis_labeller <- scales::label_number_si("", accuracy = 1)

unicef <- get_unicef_totals()

totals <-
  unicef %>%
  # doing a sheeky and ordering the bar levels here rather than explicitly with
  # factor (because i want to add %s to the labels)
  # factor order is the reverse of our bar stacking order (last level ends up left/bottom of ggplot2 bars)
  summarise(
    Unknown    = sum(unknown, na.rm = TRUE),
    AVAT       = sum(avat, na.rm = TRUE),
    Donations  = sum(donations, na.rm = TRUE),
    COVAX      = sum(covax, na.rm = TRUE),
    Commercial = sum(commercial, na.rm = TRUE),
    ) %>%
  pivot_longer(everything(), names_to = "source", values_to = "count") %>%
  mutate(
    # add counts to the labels
    source = paste0(source, ": ", dose_labeller(count)),
    source = factor(source, levels = source),
    frac = count / sum(count))

# specify the palette using the (reverse of the) order above
bar_colours <-
  c(pal_360[["darkblue"]],
    pal_360[["green"]],
    pal_360[["teal"]],
    pal_360[["grey"]],
    pal_360[["lightgrey"]]) %>%
  set_names(rev(totals$source))

# okay, here's an easy graphic: breakdown of global vaccines by program
totals %>%
  mutate(x = 1) %>%
  {
    ggplot(.) +
      aes(x = x, y = count, fill = source) +
      geom_col() +
      # geom_col(position = "fill") +
      coord_flip() +
      # scale_y_continuous(labels = scales::percent) +
      scale_y_continuous(
        breaks = c(0, 2, 4, 6, 8, 10) * 1e9,
        labels = dose_axis_labeller) +
      scale_fill_manual(
        # specify these in the reverse of the factor levels above
        values = bar_colours) +
      theme_360info() +
      theme(
        legend.direction = "horizontal",
        legend.position = "top",
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        plot.caption = element_markdown(size = rel(0.9)),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(1.5)),
        panel.grid = element_blank()
      ) +
      labs(
        x = NULL, y = NULL,
        fill = NULL,
        title = toupper("Vaccine doses by source"),
        subtitle =
          toupper("Donations make up a small fraction of global vaccine doses"),
        caption = paste(
          "**SOURCE: UNICEF** COVID-19 Vaccine Market Dashboard",
          "&lt;unicef.org/supply/covid-19-vaccine-market-dashboard&gt;"))
  } %>%
  ggsave(here("out", "total-vaccines.png"), .,
    width = 1, height = 1 / 3, device = agg_png, dpi = 100, scale = 12)

