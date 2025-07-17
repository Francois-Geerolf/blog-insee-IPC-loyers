library(tidyverse)
library(rsdmx)
library(scales)
library(viridis)

# ---- Paramètres ----

idbank_codes <- c("001759970", "001763530")
min_date <- as.Date("1990-01-01")
max_date <- as.Date("2021-01-01")

# ---- Construction de l'URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import et transformation des données ----

figure1 <- url |>
  readSDMX() |>
  as_tibble() |>
  transmute(
    TITLE_FR = str_remove(TITLE_FR, "Ensemble des ménages"),
    TITLE_FR = str_extract(TITLE_FR, "Loyers effectifs|Ensemble"),
    TITLE_FR = factor(TITLE_FR,
                      levels = c("Loyers effectifs", "Ensemble"),
                      labels = c("Loyers effectifs", "Ensemble de l'IPC")),
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  filter(date >= min_date, date <= max_date) |>
  group_by(TITLE_FR) |>
  arrange(date) |>
  mutate(OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]) |>
  ungroup()

# ---- Graphique ----

ggplot(figure1, aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = viridis(3)[1:2]
  ) +
  scale_x_date(
    breaks = seq(1990, 2021, 5) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(
    breaks = seq(80, 200, 5),
    labels = dollar_format(accuracy = 1, prefix = "")
  ) +
  labs(
    x = NULL,
    y = "Indice des prix"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.7, 0.2),
    legend.title = element_blank()
  )

# ---- Export graphique ----

ggsave("figure1.png", width = 7.5, height = 4.2)
ggsave("figure1.pdf", width = 7.5, height = 4.2, device = cairo_pdf)

