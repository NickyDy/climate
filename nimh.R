library(tidyverse)
library(rvest)
library(arrow)
library(tidytext)

nimh <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") %>%
  html_element("table") %>% html_table() %>% 
  select(station = Станция, date = Дата, hour = Час, temp = `Температура[°C]`, weather = Време,
         wind_speed = `Вятър-скорост[m/s]`, wind_dir = `Вятър-посока`, pressure = `Налягане[hPa]`)
nimh %>% 
  mutate(station = reorder_within(station, temp, weather)) %>% 
  ggplot(aes(temp, station, fill = temp)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = temp), position = position_dodge(width = 1),
            hjust = -0.1, size = 5) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = NULL, x = "Температура (\u00B0C)", 
       title = paste0("Дата на измерване: ", unique(nimh$date), "; ",
                      "Час на измерване: ", unique(nimh$hour))) +
  facet_wrap(vars(weather), scales = "free_y")
