library(tidyverse)
library(forcats)
library(themes360info)
library(ragg)
library(here)

unicef <-
  read_csv(
    here("data", "unicef-vaccine-programs.csv"),
    col_names = c("country", "commercial", "donations", "covax", "avat",
      "unknown", "total"),
    skip = 1,
    col_types = "cnnnnnn",
    na = c("(Blank)"))

totals <-
  unicef %>%
  summarise(
    Commercial = sum(commercial, na.rm = TRUE),
    Donations  = sum(donations, na.rm = TRUE),
    COVAX      = sum(covax, na.rm = TRUE),
    AVAT       = sum(avat, na.rm = TRUE),
    Unknown    = sum(unknown, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "source", values_to = "count") %>%
  mutate(
    source = fct_reorder(source, count),
    frac = count / sum(count))

# okay, here's an easy graphic: breakdown of global vaccines by program
totals %>%
  mutate(x = 1) %>%
  {
    ggplot(.) +
      aes(x = x, y = count, fill = source) +
      geom_col(position = "fill") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
      theme_360info() +
      theme(
        legend.direction = "horizontal",
        legend.position = "top",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(1.5)),
        panel.grid = element_blank()
      ) +
      labs(
        x = NULL, y = NULL,
        fill = "Source",
        title = toupper("Vaccines by source"),
        subtitle = toupper("Total vaccines globally"),
        caption = paste0(
          "UNICEF COVID-19 Vaccine Market Dashboard\n",
          "<unicef.org/supply/covid-19-vaccine-market-dashboard>"))
  } %>%
  ggsave(here("out", "total-vaccines.png"), .,
    width = 6, height = 2, device = agg_png, scale = 2)

