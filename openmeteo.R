library(openmeteo)
library(tidyverse)
library(lubridate)
library(rOstluft.plot)

glimpse(yambol)
yambol %>% map_dfr(~ sum(is.na(.)))

mad <- weather_history(
  location = "Madrid",
  start = "1940-01-01",
  end = "2023-04-30",
  daily = c("temperature_2m_min", "temperature_2m_mean",
            "temperature_2m_max", "precipitation_sum",
            "snowfall_sum", "windspeed_10m_max",
            "winddirection_10m_dominant")) %>% 
  rename("temp_min" = "daily_temperature_2m_min", "temp_mean" = "daily_temperature_2m_mean",
         "temp_max" = "daily_temperature_2m_max", "prec_sum" = "daily_precipitation_sum",
         "snow_sum" = "daily_snowfall_sum", "wind_max" = "daily_windspeed_10m_max",
         "wind_dir" = "daily_winddirection_10m_dominant") %>% 
  mutate(year = factor(year(date)),
         month = factor(month(date)),
         day = factor(day(date)), .after = date)

mad %>% 
  filter(month == 4) %>% 
  group_by(year) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1)) %>% 
  mutate(col = m < mean(m)) %>% 
  ggplot(aes(year, m, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(m)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = m), size = 3, vjust = -0.5) +
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Топла", "Студена")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Години", y = "Средна годишна температура (ºС)", fill = NULL)

mad %>% 
  filter(month == 4) %>% 
  group_by(year) %>% 
  summarise(p = round(sum(prec_sum, na.rm = T), 0)) %>% 
  mutate(col = p > mean(p)) %>% 
  ggplot(aes(year, p, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(p)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = p), size = 3, vjust = -0.5) +
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Суха", "Дъждовна")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Години", y = "Годишно количество на валежите (mm)", fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE))

mad %>% 
  filter(month == 4) %>%
  group_by(year) %>% 
  summarise(s = round(sum(snow_sum, na.rm = T), 0)) %>% 
  mutate(col = s > mean(s)) %>% 
  ggplot(aes(year, s, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(s)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = s), size = 3, vjust = -0.5) +
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Безснежна", "Снежна")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Години", y = "Годишно количество на снега (cm)", fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE))

mad %>% 
  filter(month == 4) %>% 
  group_by(year) %>% 
  summarise(w = round(mean(wind_max, na.rm = T), 1)) %>% 
  mutate(col = w < mean(w)) %>% 
  ggplot(aes(year, w, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(w)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = w), size = 3, vjust = -0.5) +
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Ветровита", "Безветрена")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Години", y = "Средна скорост на вятъра (km/h)", fill = NULL)

mad %>% 
  filter(year %in% c(1940:2023), month %in% c(4)) %>% 
  ggradar(wind_dir, wind_max, 
          fill = "blue", color = "blue", alpha = 0.5, show.legend = FALSE,
          facet_groups = grp(year)) +
  facet_wrap(vars(year), ncol = 14)
