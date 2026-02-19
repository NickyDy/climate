library(tidyverse)
library(jsonlite)
library(tidygeocoder)
library(nanoparquet)
library(fs)

files <- dir_ls("climate", regexp = "parquet")
df <- map(files, read_parquet) %>%
  set_names(basename) %>%
  list_rbind(names_to = "town") %>%
  mutate(town = str_remove(town, ".parquet$"), time = ymd(time)) %>%
  relocate(time, .after = decade) %>%
  select(-date)

df <- df %>% filter(town %in% c("blagoevgrad", "burgas", "kurdzhali", "montana", "pleven",
                                "ruse", "shumen", "smolyan", "sofia", "varna", "vidin", "yambol"))

df %>% count(town) %>% print(n = Inf)

coord <- tibble(city = "Caracas") %>% 
  geocode(city, method = "osm")

json <- fromJSON(glue::glue("https://archive-api.open-meteo.com/v1/archive?latitude={coord$lat}&longitude={coord$long}&start_date=1940-01-01&end_date={Sys.Date()}&daily=temperature_2m_mean,temperature_2m_max,temperature_2m_min,precipitation_sum,rain_sum,snowfall_sum,wind_speed_10m_max,wind_direction_10m_dominant&timezone=auto"))

df <- json[["daily"]] %>% as_tibble() %>% 
  rename("temp_min" = "temperature_2m_min", 
         "temp_mean" = "temperature_2m_mean",
         "temp_max" = "temperature_2m_max", 
         "rain_sum" = "rain_sum",
         "snow_sum" = "snowfall_sum", 
         "prec_sum" = "precipitation_sum", 
         "wind_max" = "wind_speed_10m_max",
         "wind_dir" = "wind_direction_10m_dominant") %>% 
  mutate(year = factor(year(time)),
         month = factor(month(time)),
         day = factor(day(time)),
         decade = case_when(
           year %in% c(1940:1949) ~ "1940-те",
           year %in% c(1950:1959) ~ "1950-те",
           year %in% c(1960:1969) ~ "1960-те",
           year %in% c(1970:1979) ~ "1970-те",
           year %in% c(1980:1989) ~ "1980-те",
           year %in% c(1990:1999) ~ "1990-те",
           year %in% c(2000:2009) ~ "2000-те",
           year %in% c(2010:2019) ~ "2010-те",
           year %in% c(2020:2029) ~ "2020-те"))

write_parquet(df, "climate/caracas.parquet")

colors_temp <- c("1" = "red", "2" = "orange" , "3" = "green", "4" = "#0096FF", "5" = "blue")
labels_temp <- c("1" = "Много топло", "2" = "Топло" , "3" = "Умерено", "4" = "Хладно", "5" = "Много хладно")
colors_rain <- c("1" = "blue" , "2" = "#0096FF" , "3" = "green", "4" = "orange", "5" = "red")
labels_rain <- c("1" = "Много дъждовно", "2" = "Дъждовно", "3" = "Умерено", "4" = "Сухо", "5" = "Много сухо")

#city <- "yambol"

t_year <- df %>%
  filter(month %in% c(1:12), 
         #town == city
  ) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1), .by = c(year, month)) %>%
  summarise(mean_year = mean(m), .by = year) %>% 
  mutate(tot_mean = round(mean(mean_year), 1))

df %>% 
  filter(month %in% c(1:12), 
         #town == city
  ) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1), .by = c(year, month)) %>%
  group_by(month) %>% 
  mutate(mm = round(mean(m, na.rm = T), 1), 
         iqr = IQR(m), col = case_when(
           m < mm - iqr * 1.2 ~ "5",
           m > mm + iqr * 1.2 ~ "1",
           m < mm - iqr * 0.5 ~ "4",
           m > mm + iqr * 0.5 ~ "2",
           m <= mm + iqr * 0.5 ~ "3")) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(mean_year = mean(m, na.rm = T), 
         label_year = paste0(year, ": ", round(mean_year, 1), " (\u00B0C)"),
         total_mean = mean(mean_year, na.rm = T)) %>% 
  ungroup() %>%
  ggplot(aes(month, m)) +
  geom_col(aes(fill = col), show.legend = T) +
  geom_text(aes(label = round(m, 1)), size = 3, hjust = -0.1, angle = 90) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.6)), n.breaks = 4) +
  scale_fill_manual(values = colors_temp, labels = labels_temp) +
  labs(x = "Месеци", y = "Средна денонощна температура (\u00B0C)", fill = "Легенда:",
       title = paste0("Средно за периода (", first(t_year$year), "-", 
                      last(t_year$year), " г.): ", t_year$tot_mean, " (\u00B0C)")) +
  theme(text = element_text(size = 14), legend.position = "top",
        plot.title = element_text(color = "red", face = "bold"),
        legend.justification = c(1, 0)) +
  facet_wrap(vars(label_year))

d_year <- df %>% 
  filter(month %in% c(1:12), 
         #town == city
  ) %>%
  summarise(s = round(sum(prec_sum, na.rm = T), 1), .by = c(year, month)) %>%
  summarise(s_year = sum(s), .by = year) %>% 
  mutate(tot_mean = round(mean(s_year), 0))

df %>% 
  filter(month %in% c(1:12), 
         #town == city
  ) %>%
  summarise(s = round(sum(prec_sum, na.rm = T), 1), .by = c(year, month)) %>%
  group_by(month) %>% 
  mutate(ss = round(mean(s, na.rm = T), 1), 
         iqr = IQR(s), col = case_when(
           s < ss - iqr ~ "5",
           s > ss + iqr ~ "1",
           s < ss - iqr * 0.5 ~ "4",
           s > ss + iqr * 0.5 ~ "2",
           s <= ss + iqr * 0.5 ~ "3")) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(mean_year = sum(s, na.rm = T), 
         label_year = paste0(year, ": ", round(mean_year, 0), " (mm)")) %>% 
  ungroup() %>%
  ggplot(aes(month, s)) +
  geom_col(aes(fill = col), show.legend = T) +
  geom_text(aes(label = round(s, 0)), size = 3, hjust = -0.1, angle = 90) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.6)), n.breaks = 4) +
  scale_fill_manual(values = colors_rain, labels = labels_rain) +
  labs(x = "Месеци", y = "Месечно количество на валежите (mm)", fill = "Легенда:",
       title = paste0("Средно за периода (", first(d_year$year), "-", 
                      last(d_year$year), " г.): ", d_year$tot_mean, " (mm)")) +
  theme(text = element_text(size = 14), legend.position = "top",
        plot.title = element_text(color = "red", face = "bold"),
        legend.justification = c(1, 0)) +
  facet_wrap(vars(label_year))

#DECADE
df %>% 
  #filter(town %in% c("manaus")) %>% 
  summarise(m = round(mean(temp_mean, na.rm = T), 1), .by = c(decade)) %>%
  mutate(mm = round(mean(m, na.rm = T), 1), 
         iqr = IQR(m), col = case_when(
           m < mm - iqr * 1.2 ~ "5",
           m > mm + iqr * 1.2 ~ "1",
           m < mm - iqr * 0.5 ~ "4",
           m > mm + iqr * 0.5 ~ "2",
           m <= mm + iqr * 0.5 ~ "3")) %>%
  ggplot(aes(decade, m, fill = col)) +
  geom_hline(aes(yintercept = mm), linewidth = 0.5, lty = 2, color = "black") +
  geom_col() +
  geom_text(aes(label = paste0(round(m, 1))), size = 5, vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = colors_temp, labels = labels_temp) +
  labs(x = NULL, y = "Средна денонощна температура (\u00B0C)", fill = "Легенда:",
       title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), legend.position = "top")

df %>% 
  #filter(town %in% c("manaus")) %>% 
  summarise(s = round(sum(prec_sum, na.rm = T), 1), .by = c(decade, year)) %>%
  summarise(my = mean(s, na.rm = T), .by = c(decade)) %>% 
  mutate(ss = round(mean(my, na.rm = T), 1), 
         iqr = IQR(my), col = case_when(
           my < ss - iqr ~ "5",
           my > ss + iqr ~ "1",
           my < ss - iqr * 0.5 ~ "4",
           my > ss + iqr * 0.5 ~ "2",
           my <= ss + iqr * 0.5 ~ "3")) %>%
  ggplot(aes(decade, my, fill = col)) +
  geom_hline(aes(yintercept = ss), linewidth = 0.5, lty = 2, color = "black") +
  geom_col() +
  geom_text(aes(label = paste0(round(my, 0))), size = 5, vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = colors_rain, labels = labels_rain) +
  labs(x = NULL, y = "Средно месечно количество на валежите (mm)", fill = "Легенда:",
       title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), legend.position = "top")
#---------------------------------------------------------------
df %>% 
  summarise(s = round(sum(prec_sum, na.rm = T), 1), .by = c(decade, town, year)) %>%
  summarise(my = mean(s, na.rm = T), .by = c(decade, town)) %>%
  group_by(town) %>% 
  mutate(ss = round(mean(my, na.rm = T), 1), 
         iqr = IQR(my), col = case_when(
           my < ss - iqr ~ "5",
           my > ss + iqr ~ "1",
           my < ss - iqr * 0.5 ~ "4",
           my > ss + iqr * 0.5 ~ "2",
           my <= ss + iqr * 0.5 ~ "3")) %>%
  ungroup() %>% 
  ggplot(aes(decade, my, fill = col)) +
  geom_hline(aes(yintercept = ss), linewidth = 0.5, lty = 2, color = "black") +
  geom_col() +
  geom_text(aes(label = paste0(round(my, 0))), size = 5, vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 3) +
  scale_fill_manual(values = colors_rain, labels = labels_rain) +
  labs(x = NULL, y = "Средно месечно количество на валежите (mm)", fill = "Легенда:",
       title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 14), legend.position = "top", 
        axis.text.x = element_text(size = 10)) +
  facet_wrap(vars(town), ncol = 4)
