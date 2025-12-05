library(tidyverse)
library(rvest)
library(arrow)
library(tidytext)

temp_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") %>%
  html_element("table") %>% html_table() %>% 
  select(station = Станция, date = Дата, 
         hour = Час, temp = `Температура[°C]`, 
         weather = Време, wind_speed = `Вятър-скорост[m/s]`, 
         wind_dir = `Вятър-посока`, pressure = `Налягане[hPa]`) %>% 
  mutate(date = dmy(date))
temp_nimh <- read_parquet("climate/temp_nimh.parquet")
temp_nimh <- bind_rows(temp_nimh, temp_nimh_new) %>% distinct()
write_parquet(temp_nimh, "climate/temp_nimh.parquet")

temp_nimh_new %>% 
  mutate(station = reorder_within(station, temp, weather)) %>% 
  ggplot(aes(temp, station, fill = temp)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = temp), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_y_reordered() +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = NULL, x = "Температура (\u00B0C)", 
       title = paste0("Дата на измерване: ", unique(temp_nimh_new$date), "; ",
                      "Час на измерване: ", unique(temp_nimh_new$hour), ":00")) +
  facet_wrap(vars(weather), scales = "free_y", ncol = 4)
temp_nimh_new %>% 
  mutate(station = reorder_within(station, wind_speed, weather)) %>% 
  ggplot(aes(wind_speed, station, fill = wind_speed)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = wind_speed), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(y = NULL, x = "Скорост на вятъра (m/s)", 
       title = paste0("Дата на измерване: ", unique(temp_nimh_new$date), "; ",
                      "Час на измерване: ", unique(temp_nimh_new$hour), ":00")) +
  facet_wrap(vars(weather), scales = "free_y", ncol = 4)
#-------------------------------------------------------
rain_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") %>%
  html_element("center") %>% html_table() %>% slice(-c(1, 168)) %>% 
  select(id = X1, station = X2, rain = X3, mean_rain = X4) %>% view
  mutate(rain = str_replace(rain, "n.a.", ""),
         rain = parse_number(rain), date = Sys.Date(), .after = station)
rain_nimh <- read_parquet("climate/rain_nimh.parquet")
rain_nimh <- bind_rows(rain_nimh, rain_nimh_new) %>% distinct()
write_parquet(rain_nimh, "climate/rain_nimh.parquet")

rain_nimh_new %>% drop_na(rain) %>% 
  mutate(code = case_when(
    str_detect(id, "^1") ~ "1",
    str_detect(id, "^2") ~ "2",
    str_detect(id, "^3") ~ "3",
    str_detect(id, "^4") ~ "4",
    str_detect(id, "^5") ~ "5",
    str_detect(id, "^6") ~ "6",
    str_detect(id, "^7") ~ "7",
  )) %>% 
  mutate(station = reorder_within(station, rain, code)) %>% 
  ggplot(aes(rain, station, fill = rain)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = rain), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(y = NULL, x = "Валеж (mm)", 
       title = paste0("Дата на измерване: ", unique(rain_nimh_new$date), "; ")) +
  facet_wrap(vars(code), scales = "free_y", ncol = 7)

rain_nimh_new %>%
  mutate(code = case_when(
    str_detect(id, "^1") ~ "1",
    str_detect(id, "^2") ~ "2",
    str_detect(id, "^3") ~ "3",
    str_detect(id, "^4") ~ "4",
    str_detect(id, "^5") ~ "5",
    str_detect(id, "^6") ~ "6",
    str_detect(id, "^7") ~ "7",
  )) %>% 
  mutate(mean_rain = as.numeric(mean_rain),
         station = reorder_within(station, mean_rain, code)) %>% 
  ggplot(aes(mean_rain, station, fill = mean_rain)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = mean_rain), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.01, .4))) +
  theme(text = element_text(size = 14), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(y = NULL, x = "Валеж (mm)", 
       title = paste0("Месечна норма на валежа ", "(", unique(rain_nimh_new$date), ")")) +
  facet_wrap(vars(code), scales = "free_y", ncol = 7)
#----------------------------------------------------
bg <- read_html("http://weather.bg/index.php?koiFail=bg&lng=0") %>%
  html_element("table") %>% html_table() %>% pivot_longer(-Град) %>% 
  slice(-c(1:8)) %>% select(station = Град, date = name, weather = value)
pl <- read_html("http://weather.bg/index.php?koiFail=bg&lng=0") %>%
  html_element("#planini") %>% html_table() %>% pivot_longer(-Пункт) %>% 
  slice(-c(1:5)) %>% select(station = Пункт, date = name, weather = value)
eu <- read_html("http://weather.bg/index.php?koiFail=eubp&lng=0") %>%
  html_element("table") %>% html_table() %>% pivot_longer(-Град) %>% 
  slice(-c(1:6)) %>% select(station = Град, date = name, weather = value)
forcast <- bind_rows(bg, pl, eu)
temp <- forcast %>% filter(str_detect(weather, "^\\d")) %>% 
  separate_wider_delim(weather, delim = "/", names = c("Минимална температура", "Максимална температура")) %>% 
  mutate(across(3:4, parse_number))
weather <- forcast %>% filter(str_detect(weather, "^[:alpha:]"))
df <- inner_join(temp, weather) %>% 
  pivot_longer(`Минимална температура`:`Максимална температура`) %>%
  mutate(name = fct_rev(name)) %>% 
  arrange(station) %>% 
  filter(!weather == "n.a.")

df %>% 
  filter(station == "Бургас") %>% 
  ggplot(aes(value, station, fill = name)) +
  geom_col(position = "dodge") +
  facet_grid(weather ~ date, labeller = labeller(weather = label_wrap_gen(15))) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "top") +
  scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
  scale_fill_manual(values = c("Минимална температура" = "blue", "Максимална температура" = "red")) +
  geom_text(aes(label = paste0(value, " (\u00B0C)")), 
            position = position_dodge(width = 1), hjust = -0.1, size = 5) +
  labs(y = NULL, x = NULL, fill = "Легенда:", caption = "Източник на данните: НИМХ") +
  guides(fill = guide_legend(reverse = TRUE))




