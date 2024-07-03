library(tidyverse)
library(arrow)
library(openmeteo)

#library(rOstluft.plot)

daily <- read_parquet("shiny/climate/daily.parquet") %>% 
  filter(!location %in% c("Връх Мусала", "Връх Ботев", "Вреъх Черни връх")) %>% 
  mutate(decade = case_when(
    year %in% c("1940", "1941", "1942", "1943", "1944", "1945", "1946", "1947", "1948", "1949") ~ "40s",
    year %in% c("1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959") ~ "50s",
    year %in% c("1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969") ~ "60s",
    year %in% c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979") ~ "70s",
    year %in% c("1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989") ~ "80s",
    year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999") ~ "90s",
    year %in% c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023", "2024") ~ "20s")) %>%
  relocate(decade, .after = date) %>% 
  mutate(decade = fct_inorder(decade))

df %>% map_dfr(~ sum(is.na(.)))
glimpse(df)
#---------------------
df <- weather_history(
  location = "yambol",
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

df %>% 
  drop_na() %>% 
  #filter(year %in% c(1945), location == "Ямбол") %>%
  filter(month == "6", year == 2024) %>%
  pivot_longer(2:7) %>% 
  mutate(col = case_when(name %in% c("temp_max", "temp_min", "temp_mean") & value > 35 ~ "hot",
                         name %in% c("temp_max", "temp_min", "temp_mean") & value < 0 ~ "cold",
                         name == "prec_sum" & value > 0 ~ "rain",
                         name == "snow_sum" & value > 0 ~ "snowing",
                         name == "wind_max" & value > 30 ~ "windy",
                         name == "wind_max" & value < 30 ~ "notwindy",
                         .default = "normal")) %>%
  mutate(name = fct_recode(name, 
           "Максимална температура (\u00B0C)" = "temp_max",
           "Средна температура (\u00B0C)" = "temp_mean",
           "Минимална температура (\u00B0C)" = "temp_min",
           "Дъжд (mm)" = "prec_sum",
           "Сняг (cm)" = "snow_sum",
           "Максимална скорост на вятъра (km/h)" = "wind_max"),
         name = fct_relevel(name,
           "Максимална температура (\u00B0C)",
           "Средна температура (\u00B0C)",
           "Минимална температура (\u00B0C)",
           "Дъжд (mm)",
           "Сняг (cm)",
           "Максимална скорост на вятъра (km/h)")) %>%
  ggplot(aes(day, value, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(value, 1)), size = 4, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)), n.breaks = 4) +
  scale_fill_manual(values = c("hot" = "red", "normal" = "orange", 
                               "cold" = "lightblue", "rain" = "blue",
                               "windy" = "green", "snowing" = "#00FFFF", "notwindy" = "darkgreen")) +
  #geom_hline(data = m_mont, aes(yintercept = m), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Дни", y = NULL) + 
  theme(text = element_text(size = 16)) +
  facet_wrap(vars(name), ncol = 1, dir = "v")

df %>% 
  filter(month %in% c(6)) %>% 
  summarise(m = round(mean(temp_max, na.rm = T), 1), n = n(), .by = c(year, month)) %>%
  mutate(mm = mean(m, na.rm = T), col = case_when(
    m > mm + 2 ~ "1",
    m > mm ~ "2",
    m < mm - 2 ~ "4",
    m <= mm ~ "3")) %>%
  ggplot(aes(year, m, fill = col)) +
  geom_col() +
  geom_text(aes(label = paste0(round(m, 1))), size = 3.5, hjust = -0.1, angle = 90) +
  geom_hline(aes(yintercept = mm), linewidth = 0.5, lty = 2, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = c("1" = "red", "2" = "orange" ,
                               "3" = "green", "4" = "#0096FF"), 
                    labels = c("1" = "Моного по-топло от средното", 
                               "2" = "По-топло от средното", 
                               "3" = "По-хладно от средното",
                               "4" = "Много по-хладно от средното")) +
  labs(x = NULL, y = "Средна максимална дневна температура (\u00B0C)", fill = "Легенда:",
       title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "top")
df %>% 
  filter(month %in% c(6)) %>% 
  summarise(s = round(sum(prec_sum, na.rm = T), 1), n = n(), .by = c(year, month)) %>%
  mutate(ss = mean(s), col = case_when(
    s > ss + 30 ~ "1",
    s < ss - 15 ~ "4",
    s > ss ~ "2",
    s <= ss ~ "3")) %>%
  ggplot(aes(year, s, fill = col)) +
  geom_col() +
  geom_text(aes(label = paste0(round(s, 0))), size = 3.5, vjust = -0.2) +
  geom_hline(aes(yintercept = ss), linewidth = 0.5, lty = 2, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = c("1" = "blue" , "2" = "#00BFC4" ,
                               "3" = "orange", "4" = "red"), 
                    labels = c("1" = "Много по-дъждовно от средното", 
                               "2" = "По-дъждовно от средното", 
                               "3" = "По-сухо от средното",
                               "4" = "Много по-сухо от средното")) +
  labs(x = NULL, y = "Месечно количество на валежите (mm)", fill = "Легенда:",
       title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "top")

temp %>% 
  filter(month %in% c(6), elev < 1200) %>% 
  summarise(m = round(mean(temp, na.rm = T), 1), n = n(), .by = c(year, month)) %>%
  mutate(mm = mean(m, na.rm = T), col = case_when(
    m > mm + 1.5 ~ "1",
    m > mm ~ "2",
    m < mm - 1.5 ~ "4",
    m <= mm ~ "3")) %>%
  ggplot(aes(year, m, fill = col)) +
  geom_col() +
  geom_text(aes(label = paste0(round(m, 1))), size = 5, vjust = -0.3) +
  geom_hline(aes(yintercept = mm), linewidth = 0.5, lty = 2, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = c("1" = "red", "2" = "orange" ,
                               "3" = "green", "4" = "#0096FF"), 
                    labels = c("1" = "Моного по-топло от средното", 
                               "2" = "По-топло от средното", 
                               "3" = "По-хладно от средното",
                               "4" = "Много по-хладно от средното")) +
  labs(x = NULL, y = "Средна дневна температура (\u00B0C)", fill = "Легенда:", title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), legend.position = "top")

rain %>% 
  filter(month %in% c(6), elev < 1200) %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), n = n(), .by = c(station, year, month)) %>%
  summarise(s = mean(s, na.rm = T), .by = c(year, month)) %>% 
  mutate(ss = mean(s), col = case_when(
    s > ss + 20 ~ "0",
    s < ss - 20 ~ "3",
    s > ss ~ "1",
    s <= ss ~ "2")) %>%
  ggplot(aes(year, s, fill = col)) +
  geom_col() +
  geom_text(aes(label = paste0(round(s, 0))), size = 5, vjust = -0.3) +
  geom_hline(aes(yintercept = ss), linewidth = 0.5, lty = 2, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = c("0" = "blue" , "1" = "#00BFC4" , "2" = "orange", "3" = "red"), 
                    labels = c("0" = "Много по-дъждовно от средното", 
                               "1" = "По-дъждовно от средното", 
                               "2" = "По-сухо от средното",
                               "3" = "Много по-сухо от средното")) +
  labs(x = NULL, y = "Месечно количество на валежите (mm)", fill = "Легенда:", title = NULL) +
  theme(text = element_text(size = 16), legend.position = "top")
#---------------------------------------------------------------
db <- daily %>% 
  group_by(location, year, month) %>% 
  summarise(s = sum(prec_sum, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, month) %>% 
  summarise(m = mean(s, na.rm = T)) %>% 
  #filter(month == 4) %>% 
  ungroup() %>%
  mutate(ss = mean(m), col = case_when(
    m > ss + 20 ~ "0",
    m > ss ~ "1", 
    m <= ss ~ "2"))

min_colors <- c("<-20 \u00B0C" = "purple", "-20:-10 \u00B0C" = "blue", ">-10 \u00B0C" = "lightblue")
max_colors <- c(">35 \u00B0C" = "red", "25-35 \u00B0C" = "orange", "<25 \u00B0C" = "green")

# df <- weather_history(
#   location = "Yambol",
#   start = "1970-01-01",
#   end = Sys.Date(),
#   hourly = c("temperature_2m", "precipitation"))

colors <- c("1" = "red", "2" = "orange" , "3" = "green", "4" = "lightblue", "5" = "blue")
labels <- c("1" = "Много горещо", "2" = "Горещо" , "3" = "Умерено", "4" = "Хладно", "5" = "Много хладно")

daily %>% 
  filter(month %in% c(4)) %>% 
  mutate(month = fct_recode(month, "Април" = "4")) %>% 
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

daily %>% 
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

m_month <- daily %>% 
  filter(year %in% c(1940:2023), 
         !location %in% c("Връх Ботев", "Връх Мусала", "Връх Черни връх")) %>%
  summarise(m = mean(temp_mean, na.rm = T), .by = month)

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
  #filter(month %in% c(11)) %>% 
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
  #filter(month %in% c(4)) %>% 
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

# Decade
daily %>% 
  #filter(month %in% c(2)) %>% 
  group_by(decade) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1)) %>%
  mutate(col = m < mean(m)) %>%
  ggplot(aes(decade, m, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(m)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = m), size = 5, vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Топло", "Студено")) +
  theme(text = element_text(size = 16), axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  labs(x = "Десетилетие", y = "Средна температура (\u00B0C)", fill = "Легенда:")

# Rain--------------------------------------------------------------------------
mean_rain <- daily %>% 
  #filter(month == 6) %>%
  group_by(location, year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  group_by(month, year) %>% 
  summarise(m = round(mean(sum, na.rm = T), 0), n = n())
daily %>% 
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
  #filter(month %in% c(4)) %>%
  group_by(year) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  ungroup() %>% 
  summarise(p = round(mean(sum, na.rm = T), 0), .by = year) %>%
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

# Decade
daily %>% 
  #filter(month %in% c(1)) %>%
  group_by(decade, year, location) %>%
  mutate(sum = sum(prec_sum, na.rm = T)) %>%
  ungroup() %>% 
  summarise(p = round(mean(sum, na.rm = T), 0), .by = decade) %>%
  mutate(col = p > mean(p)) %>%
  ggplot(aes(decade, p, fill = col)) +
  geom_col() +
  geom_hline(aes(yintercept = mean(p)), linewidth = 0.5, lty = 2, color = "black") +
  geom_text(aes(label = p), size = 5, vjust = -0.5) +
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(.01, .05))) +
  scale_fill_discrete(labels = c("Сухо", "Дъждовно")) +
  theme(text = element_text(size = 16), axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  labs(x = "Десетилетие", y = "Средногодишно количество на валежите (mm)", fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE))

daily %>% 
  #filter(location == "София") %>% 
  group_by(year, location, month) %>% 
  summarise(prec_s = sum(prec_sum, na.rm = T)) %>% 
  group_by(month) %>% 
  summarise(prec_m = mean(prec_s)) %>% 
  ggplot(aes(month, prec_m)) +
  geom_col(fill = "#00BFC4") +
  geom_text(aes(label = round(prec_m, 1)), size = 5, vjust = -0.5)
# Snow------------------------------------
df %>% 
  #filter(month == 6) %>%
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
  #filter(month == 8) %>%
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
