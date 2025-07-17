library(tidyverse)
library(rsdmx)
library(scales)
library(zoo)
library(viridis)

# ---- Paramètres ----

idbank_codes <- c("010567012", "010567056")
min_date <- as.Date("1998-01-01")
max_date <- as.Date("2022-01-01")

# ---- Construction de l’URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import et transformation des données ----

figure2 <- url |>
  readSDMX() |>
  as_tibble() |>
  transmute(
    TITLE_FR = str_extract(TITLE_FR, "Paris|France métropolitaine"),
    TITLE_FR = factor(TITLE_FR, levels = c("Paris", "France métropolitaine")),
    date = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  filter(date >= min_date, date <= max_date) |>
  group_by(TITLE_FR) |>
  arrange(date) |>
  mutate(OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]) |>
  ungroup()

# ---- Graphique ----

ggplot(figure2, aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = viridis(3)[1:2],
    labels = c("Paris", "France métropolitaine")
  ) +
  scale_x_date(
    breaks = seq(1998, 2025, 2) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(
    breaks = seq(50, 7000, by = 50)
  ) +
  labs(
    x = NULL,
    y = "Indice des prix des logements anciens"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.25, 0.85),
    legend.title = element_blank()
  )

# ---- Export ----

ggsave("figure2.png", width = 7.5, height = 4.2)
ggsave("figure2.pdf", width = 7.5, height = 4.2, device = cairo_pdf)

