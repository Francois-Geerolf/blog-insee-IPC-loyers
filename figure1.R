library(tidyverse)
figure1_idbanks <- c("001759970", "001763530")

min_date <- as.Date("1990-01-01")
max_date <- as.Date("2021-01-01")

figure1 <- paste(figure1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  transmute(TITLE_FR = str_remove(TITLE_FR, "Ensemble des ménages"),
            TITLE_FR = str_extract(TITLE_FR, "Loyers effectifs|Ensemble"),
            date = as.Date(paste0(TIME_PERIOD, "-01")),
            OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  filter(date <= max_date, date >= min_date) %>%
  group_by(TITLE_FR) %>%
  arrange(date) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

figure1 %>%
  ggplot() + ylab("Indice des prix") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  scale_color_manual(values = viridis::viridis(3)[1:2]) +
  scale_x_date(breaks = seq(1920, 2100, 5) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.7, 0.2),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(0, 200, 5),
                labels = dollar_format(accuracy = 1, prefix = ""))


ggsave("figure1.png", width = 1.25*6, height = 1.25*3.375)
ggsave("figure1.pdf", width = 1.25*6, height = 1.25*3.375)
