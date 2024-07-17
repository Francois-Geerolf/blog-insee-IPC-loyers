library(tidyverse)
figure2_idbanks <- c("010567012", "010567056")

min_date <- as.Date("1998-01-01")
max_date <- as.Date("2022-01-01")

figure2 <- paste(figure2_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  transmute(TITLE_FR = str_extract(TITLE_FR, "Paris|France métropolitaine"),
            date = zoo::as.Date(zoo::as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
            OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  filter(date <= max_date, date >= min_date) %>%
  group_by(TITLE_FR) %>%
  arrange(date) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

figure2 %>%
  ggplot + geom_line(aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  theme_minimal()  +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2050, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
  scale_color_manual(values = viridis::viridis(3)[1:2]) +
  theme(legend.position = c(0.25, 0.85),
        legend.title = element_blank()) +
  xlab("") + ylab("Indice des prix des logements anciens") +
  scale_y_log10(breaks = seq(0, 7000, 50))

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375)
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)
