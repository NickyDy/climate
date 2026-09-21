library(tidyverse)
library(nanoparquet)
library(httr2)
library(tidygeocoder)
library(tidytext)

coord <- tibble(city = "sofia") %>% 
  geocode(city, method = "osm")

wf <- request("https://api.open-meteo.com/v1/forecast") %>% 
  req_url_query(
    latitude = coord$lat,
    longitude = coord$long,
    daily = paste(
      c("temperature_2m_mean",
        "temperature_2m_max",
        "temperature_2m_min",
        "rain_sum",
        "snowfall_sum",
        "wind_speed_10m_max",
        "wind_direction_10m_dominant",
        "cloud_cover_mean"),
      collapse = ","),
    timezone = "auto",
    wind_speed_unit = "ms",
    forecast_days = "14",
    models = "ecmwf_ifs") %>% 
  req_perform() %>% 
  resp_body_json(., simplifyVector = T) %>% 
  pluck("daily") %>% as_tibble() %>% 
  mutate(date = ymd(time)) %>% 
  select(-time)

wf_h <- request("https://api.open-meteo.com/v1/forecast") %>% 
  req_url_query(
    latitude = coord$lat,
    longitude = coord$long,
    hourly = paste(
      c("temperature_2m",
        "precipitation",
        "precipitation_probability",
        "wind_speed_10m",
        "wind_direction_10m",
        "cloud_cover"),
      collapse = ","),
    timezone = "auto",
    wind_speed_unit = "ms",
    forecast_days = "10",
    models = "ecmwf_ifs") %>% 
  req_perform() %>% 
  resp_body_json(., simplifyVector = T) %>% 
  pluck("hourly") %>% as_tibble() %>% 
  mutate(date = ymd_hm(time)) %>% 
  select(-time)

midnights <- data.frame(
  date = as.POSIXct(unique(as.Date(wf_h$date))))

wf_h %>%
  slice(seq(1, 240, by = 3)) %>%
  pivot_longer(c(-date)) %>%
  mutate(wind_dir = value, hour = hour(date)) %>%
  mutate(wind_dir = case_when(
    name %in% c("wind_direction_10m") & between(wind_dir, 45, 135) ~ "E",
    name %in% c("wind_direction_10m") & between(wind_dir, 135, 225) ~ "S",
    name %in% c("wind_direction_10m") & between(wind_dir, 225, 315) ~ "W",
    name %in% c("wind_direction_10m") & between(wind_dir, 315, 360) ~ "N",
    name %in% c("wind_direction_10m") & between(wind_dir, 0, 45) ~ "N", .default = "")) %>%
  mutate(value = round(value, 0), value = as.character(value), unit = case_when(
    name != "wind_direction_10m" ~ value,
    .default = wind_dir),
    value = as.numeric(value)) %>%
  mutate(name = fct_recode(name, "Температура (\u00B0C)" = "temperature_2m",
                           "Вероятност за валеж (%)" = "precipitation_probability",
                           "Валеж (mm)" = "precipitation",
                           "Посока на вятъра" = "wind_direction_10m",
                           "Скорост на вятъра (m/s)" = "wind_speed_10m",
                           "Облачност (%)" = "cloud_cover"),
         name = fct_relevel(name, "Облачност (%)", "Вероятност за валеж (%)", "Валеж (mm)",
                            "Температура (\u00B0C)", "Посока на вятъра", "Скорост на вятъра (m/s)")) %>%
  ggplot(aes(date, value, fill = name)) +
  geom_vline(data = midnights, aes(xintercept = as.numeric(date)),
             linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_col(show.legend = F) +
  geom_text(aes(label = paste0( unit)), size = 4, vjust = -0.1) +
  scale_fill_manual(values = c("#0096FF", "#0096FF", "blue", "red", "darkgreen", "green")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b-%d-%a") +
  theme(text = element_text(size = 16), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  labs(x = "Дата", y = NULL) +
  facet_wrap(vars(name), ncol = 1, scale = "free_y")

wf %>% 
  #select(-daily_wind_direction_10m_dominant) %>% 
  pivot_longer(c(-date)) %>%
  mutate(wind_dir = value) %>% 
  mutate(wind_dir = case_when(
    name %in% c("wind_direction_10m_dominant") & between(wind_dir, 45, 135) ~ "E",
    name %in% c("wind_direction_10m_dominant") & between(wind_dir, 135, 225) ~ "S",
    name %in% c("wind_direction_10m_dominant") & between(wind_dir, 225, 315) ~ "W",
    name %in% c("wind_direction_10m_dominant") & between(wind_dir, 315, 360) ~ "N",
    name %in% c("wind_direction_10m_dominant") & between(wind_dir, 0, 45) ~ "N", .default = "")) %>% 
  mutate(unit = case_when(
    name == "rain_sum" ~ "mm",
    name == "snowfall_sum" ~ "cm",
    name == "wind_speed_10m_max" ~ "m/s",
    name == "cloud_cover_mean" ~ "%",
    name == "wind_direction_10m_dominant" ~ "",
    .default = "\u00B0C")) %>%
  mutate(name = fct_recode(name, "Минимална температура" = "temperature_2m_min",
                           "Средна температура" = "temperature_2m_mean",
                           "Максимална температура" = "temperature_2m_max",
                           "Сума на валежите" = "rain_sum",
                           "Височина на снежната покривка" = "snowfall_sum",
                           "Посока на вятъра" = "wind_direction_10m_dominant",
                           "Скорост на вятъра" = "wind_speed_10m_max",
                           "Средна облачност" = "cloud_cover_mean"),
         value = round(value, 0)) %>% 
  ggplot(aes(date, value, fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = paste0(value, " ", unit, wind_dir)), size = 5, vjust = -0.3) +
  scale_fill_manual(values = c("#0096FF", "blue", "#00FFFF", "red", "orange", "yellow", "darkgreen", "green")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b-%d-%a") +
  theme(text = element_text(size = 16)) +
  labs(x = "Дата", y = "Стойност") +
  facet_wrap(vars(name), ncol = 1, scale = "free_y")