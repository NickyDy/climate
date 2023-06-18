library(tidyverse)
library(openmeteo)
library(lubridate)
library(rOstluft.plot)

df <- read_rds("climate/meteo.rds")

glimpse(df)
df %>% map_dfr(~ sum(is.na(.)))
df %>% count(location)

ber <- weather_history(
  location = "Berkovitsa",
  start = "1940-01-01",
  end = "2022-12-31",
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
         day = factor(day(date)), 
         location = "Berkovitsa", .after = date)
# Temperatures------------------------------
mean_temp <- df %>%
  filter(location == "Musala peak") %>%
  group_by(year) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1),	n = n())
df %>%
  filter(location == "Musala peak") %>%
  mutate(m = mean(temp_mean, na.rm = T)) %>%
  group_by(year) %>%
  mutate(col = mean(temp_mean, na.rm = T) > m) %>%
  ggplot(aes(year, temp_mean)) +
  geom_boxplot(aes(fill = col), outlier.color = NA, fatten = NULL) +
  geom_point(data = mean_temp, aes(year, m), color = "black", size = 0.5) +
  geom_text(data = mean_temp, aes(year, m, label = m), size = 2.5, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(temp_mean, na.rm = T)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средна месечна температура (ºС)", fill = "Легенда:") +
  theme(text = element_text(size = 12), legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("Студена", "Топла")) +
  scale_y_continuous(n.breaks = 10) +
  guides(fill = guide_legend(reverse = TRUE))
df %>% 
  filter(location == "Cherni peak") %>% 
  group_by(year) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1)) %>%
  mutate(col = m < mean(m)) %>% 
  ggplot(aes(year, m, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(m)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = m), size = 3, vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Топла", "Студена")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Години", y = "Средна годишна температура (ºС)", fill = NULL)

# Rain--------------------
mean_rain <- df %>% 
  #filter(location == "Yambol") %>%
  group_by(location, year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  group_by(month, year) %>% 
  summarise(m = round(mean(sum, na.rm = T), 2), n = n())
df %>% 
  #filter(location == "Yambol") %>%
  group_by(location, year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  ungroup() %>%
  mutate(su = mean(sum, na.rm = T)) %>%
  group_by(year) %>%
  mutate(col = mean(sum, na.rm = T) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_boxplot(aes(fill = col), outlier.colour = NA, fatten = NULL) +
  geom_point(data = mean_rain, aes(year, m), color = "black", size = 0.5) +
  #geom_text(data = med_rain, aes(year, m, label = m), position = position_dodge(width = 1), size = 4, vjust = -1) +
  geom_hline(aes(yintercept = mean(sum, na.rm = T)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средно годишно количество на валежите (mm)", fill = "Легенда:") +
  theme(text = element_text(size = 12), legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Суха", "Дъждовна")) +
  scale_y_continuous(n.breaks = 10) +
  guides(fill = guide_legend(reverse = TRUE))
df %>% 
  filter(location == "Lovech") %>%
  group_by(location, year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  group_by(year) %>% 
  summarise(p = round(mean(sum, na.rm = T), 0)) %>%
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
df %>% 
  filter(year == 2022, location == "Kurdzhali", prec_sum > 0) %>% 
  ggplot(aes(date, prec_sum)) +
  geom_point() +
  geom_smooth(span = 0.2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  coord_cartesian(xlim = as.Date(c("2022-01-01", "2022-12-31")))
# Snow------------------------------------
df %>% 
  filter(location == "Blagoevgrad") %>%
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

# Wind speed------------------------------------
df %>% 
  filter(location == "Varna") %>%
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

# Wind direction------------------------------------
df %>% 
  filter(location == "Varna", year %in% c(1940:2023), month %in% c(5), day %in% c(1:31)) %>% 
  ggradar(wind_dir, wind_max, 
          fill = "blue", color = "blue", alpha = 0.5, show.legend = FALSE,
          facet_groups = grp(year)) +
  facet_wrap(vars(year), ncol = 14)
