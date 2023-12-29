library(tidyverse)
library(openmeteo)
#library(rOstluft.plot)

df <- read_rds("climate/daily.rds")

glimpse(daily)
df %>% map_dfr(~ sum(is.na(.)))
df %>% count(location) %>% print(n = Inf)

min_colors <- c("<-20 \u00B0C" = "purple", "-20:-10 \u00B0C" = "blue", ">-10 \u00B0C" = "lightblue")
max_colors <- c(">35 \u00B0C" = "red", "25-35 \u00B0C" = "orange", "<25 \u00B0C" = "green")

df <- weather_history(
  location = "Mutis",
  start = "1940-01-01",
  end = Sys.Date(),
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
         day = factor(day(date)))

df <- weather_history(
  location = "Yambol",
  start = "1970-01-01",
  end = Sys.Date(),
  hourly = c("temperature_2m", "precipitation"))

colors <- c("1" = "red", "2" = "orange" , "3" = "green", "4" = "lightblue", "5" = "blue")
labels <- c("1" = "Много горещо", "2" = "Горещо" , "3" = "Умерено", "4" = "Хладно", "5" = "Много хладно")

df %>% 
  filter(month %in% c(7, 8, 9)) %>% 
  mutate(month = fct_recode(month, "Юли" = "7", "Август" = "8", "Септември" = "9")) %>% 
  group_by(year, month) %>%
  summarise(m = round(mean(temp_max, na.rm = T), 1), n = n()) %>%
  mutate(col = case_when(m > 31 ~ "1", 
                         between(m, 29, 31) ~ "2", 
                         between(m, 27, 28.9) ~ "3",
                         between(m, 25, 26.9) ~ "4", 
                         m < 25 ~ "5")) %>%
  ggplot(aes(month, m, fill = col)) +
  geom_col(show.legend = T) +
  geom_text(aes(label = paste0(round(m, 1), " \u00B0C")), size = 3.5, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 4) +
  scale_fill_manual(values = colors, labels = labels) +
  labs(x = NULL, y = "Средна максимална дневна температура (\u00B0C)", fill = "Легенда:") +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(vars(year))

df %>% 
  drop_na() %>% 
  mutate(extreme = if_else(prec_sum > 100, prec_sum, NA)) %>%
  filter(prec_sum > 30) %>% 
  ggplot(aes(date, prec_sum)) +
  geom_point(aes(date, extreme), color = "red", size = 3, show.legend = F) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_y_log10() +
  labs(x = "Години", y = "Количество на валежа > 30 mm (литра на квадратен метър) на денонощие") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ location)

df %>% 
  drop_na() %>% 
  filter(hourly_precipitation > 5) %>% 
  ggplot(aes(datetime, hourly_precipitation)) +
  geom_point(aes(alpha = 0.05), show.legend = F) +
  #scale_x_datetime(date_breaks = "5 years") +
  theme(text = element_text(size = 16)) +
  labs(x = "Години", y = "Количество на валежа > 5 mm (литра на квадратен метър) на час") +
  geom_smooth()

df %>% 
  filter(month == 10) %>% 
  mutate(month = fct_recode(month, "Октомври" = "10")) %>% 
  group_by(year, month) %>% 
  summarise(m = round(mean(temp_max, na.rm = T), 1)) %>%
  mutate(col = case_when(m >= 16 ~ "1", 
                         m <= 16 & m >= 14.4 ~ "2", 
                         m <= 14.4 & m >= 13.1 ~ "3",
                         m <= 13.1 & m >= 12.2 ~ "4", 
                         m <= 12.2 ~ "5")) %>%
  ggplot(aes(month, m, fill = col)) +
  geom_col() +
  scale_fill_manual(values = colors, labels = labels) +
  geom_text(aes(label = paste0(round(m, 1), " \u00B0C")), size = 3.5, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 4) +
  facet_wrap(vars(year))

df %>% 
  filter(month == 10, year %in% c(1993, 1997, 2012, 2019, 2023)) %>% 
  ggplot(aes(day, temp_max)) +
  geom_col(fill = "#00BFC4") +
  geom_text(aes(label = paste0(round(temp_max, 1), " \u00B0C")), size = 3.5, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 4) +
  facet_wrap(vars(year), ncol = 1)

# Extreme temperatures
mean_max <- df %>% 
  filter(month %in% c(7, 8, 9), year == 2023) %>% 
  summarise(m = mean(temp_max, na.rm = T), .by = c(month))
df %>% 
  filter(month %in% c(7, 8, 9), year %in% c(2023)) %>% 
  mutate(extreme = case_when(temp_max > 35 ~ ">35 \u00B0C", 
                             temp_max >= 25 & temp_max <= 35 ~ "25-35 \u00B0C",
                             TRUE ~ "<25 \u00B0C"),
         extreme = fct_relevel(extreme, "<25 \u00B0C", "25-35 \u00B0C", ">35 \u00B0C")) %>% 
  ggplot(aes(day, temp_max, fill = extreme, groups = month)) +
  geom_col(show.legend = T) +
  geom_text(aes(label = round(temp_max, 1)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = max_colors) +
  scale_x_discrete(breaks = c(1:31)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.3))) +
  geom_hline(data = mean_max, aes(yintercept = m), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Дни от месеца", y = "Максимална дневна температура (\u00B0C)", fill = "Легенда:",
       title = "Максимални температури през юли и август в гр. Ямбол, 2023 година") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ month, ncol = 1, dir = "v")

mean_min <- df %>% 
  filter(month %in% c(1, 2), year == 2023) %>% 
  summarise(m = mean(temp_min, na.rm = T), .by = c(month))
df %>% 
  filter(month %in% c(1, 2), year %in% c(2023)) %>% 
  mutate(extreme = case_when(temp_min < -20 ~ "<-20 \u00B0C", 
                             between(temp_min, -20, -10) ~ "-20:-10 \u00B0C",
                             TRUE ~ ">-10 \u00B0C"),
         extreme = fct_relevel(extreme, ">-10 \u00B0C", "-20:-10 \u00B0C", "<-20 \u00B0C")) %>%
  ggplot(aes(day, temp_min, fill = extreme)) +
  geom_col(show.legend = T) +
  geom_text(aes(label = round(temp_min, 1)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = min_colors) +
  scale_x_discrete(breaks = c(1:31)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.3))) +
  geom_hline(data = mean_min, aes(yintercept = m), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Дни от месеца", y = "Минимална дневна температура (\u00B0C)", fill = "Легенда:",
       title = "Минимални температури през януари и февруари в гр. Ямбол, 2023 година") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ month, ncol = 1, dir = "v")

# Top 10
df %>% 
  slice_min(order_by = temp_min, n = 10) %>% 
  unite("united", year:day, remove = F) %>% 
  ggplot(aes(united, temp_min, fill = month)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(.07, .01))) +
  geom_text(aes(label = paste0(round(temp_min, 1), " \u00B0C")), size = 4, vjust = -0.5) +
  labs(x = "Date", y = "Min temperature (\u00B0C)", fill = "Month:")
df %>% 
  slice_max(order_by = temp_max, n = 10) %>% 
  unite("united", year:day, remove = F) %>% 
  ggplot(aes(united, temp_max, fill = month)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(.01, .07))) +
  geom_text(aes(label = paste0(round(temp_max, 1), " \u00B0C")), size = 4, vjust = -0.5) +
  labs(x = "Date", y = "Max temperature (\u00B0C)", fill = "Month:")

# Temperatures------------------------------
mean_temp <- df %>%
  filter(month %in% c(11)) %>% 
  group_by(year) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1),	n = n())
df %>%
  #filter(month %in% c(6, 7, 8)) %>% 
  mutate(m = mean(temp_mean, na.rm = T)) %>%
  group_by(year) %>%
  mutate(col = mean(temp_mean, na.rm = T) > m) %>%
  ggplot(aes(year, temp_mean)) +
  geom_boxplot(aes(fill = col), outlier.color = NA, fatten = NULL) +
  geom_point(data = mean_temp, aes(year, m), color = "black", size = 0.5) +
  geom_text(data = mean_temp, aes(year, m, label = m), size = 2.5, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(temp_mean, na.rm = T)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средна температура (\u00B0C)", fill = "Легенда:") +
  theme(text = element_text(size = 12), legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("Студена", "Топла")) +
  scale_y_continuous(n.breaks = 10) +
  guides(fill = guide_legend(reverse = TRUE))
df %>% 
  filter(month %in% c(11)) %>% 
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
  labs(x = "Години", y = "Средна температура (\u00B0C)", fill = "Легенда:")

# Rain--------------------
mean_rain <- df %>% 
  #filter(month == 6) %>%
  group_by(location, year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  group_by(month, year) %>% 
  summarise(m = round(mean(sum, na.rm = T), 0), n = n())
df %>% 
  #filter(month == 6) %>%
  group_by(location, year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  ungroup() %>%
  mutate(su = mean(sum, na.rm = T)) %>%
  group_by(year) %>%
  mutate(col = mean(sum, na.rm = T) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_boxplot(aes(fill = col), outlier.colour = NA, fatten = NULL) +
  geom_point(data = mean_rain, aes(year, m), color = "black", size = 0.5) +
  geom_text(data = mean_rain, aes(year, m, label = m), size = 3, hjust = -0.5, angle = 90) +
  geom_hline(aes(yintercept = mean(sum, na.rm = T)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средно годишно количество на валежите (mm)", fill = "Легенда:") +
  theme(text = element_text(size = 12), legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Суха", "Дъждовна")) +
  scale_y_continuous(n.breaks = 10) +
  guides(fill = guide_legend(reverse = TRUE))
df %>% 
  filter(month %in% c(11)) %>%
  group_by(year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  summarise(p = round(mean(sum, na.rm = T), 0)) %>%
  mutate(col = p > mean(p)) %>%
  ggplot(aes(year, p, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(p)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = p), size = 3, vjust = -0.5) +
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Суха", "Дъждовна")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Години", y = "Количество на валежите (mm)", fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE))

df %>% 
  filter(location == "София") %>% 
  group_by(year, month) %>% 
  summarise(prec_s = sum(prec_sum, na.rm = T)) %>% 
  group_by(month) %>% 
  summarise(prec_m = mean(prec_s)) %>% 
  ggplot(aes(month, prec_m)) +
  geom_col(fill = "#00BFC4") +
  geom_text(aes(label = round(prec_m, 1)), size = 5, vjust = -0.5)
  
# Snow------------------------------------
df %>% 
  filter(month == 6) %>%
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
  filter(month == 8) %>%
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
  mutate(month = as.numeric(month), month = month.abb[month],
         month = factor(month, levels = month.abb)) %>% 
  filter(month %in% c("Aug")) %>% 
  ggradar(wind_dir, wind_max, 
          fill = "blue", color = "blue", alpha = 0.5, show.legend = FALSE,
          facet_groups = grp(year)) +
  labs(y = "Максинална скорост на вятъра (km/h)") +
  theme(text = element_text(size = 12)) +
  facet_wrap(vars(year), ncol = 15)
