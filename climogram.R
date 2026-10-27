library(tidyverse)
library(nanoparquet)
library(fs)
library(patchwork)

files <- dir_ls("climate", regexp = "parquet")
df <- map(files, read_parquet) %>%
  set_names(basename) %>%
  list_rbind(names_to = "town") %>%
  mutate(town = str_remove(town, ".parquet$")) %>%
  select(-time)

df %>% count(town) %>% print(n = Inf)

df %>% filter(month %in% c(10:12)) %>% 
  summarise(h_town = mean(temp_mean, na.rm = T), .by = town) %>% view

triplot("manaus")

triplot <- function(city) {
  
  m_temp <- df %>% 
    filter(town == city) %>% 
    summarise(mean_temp = mean(temp_mean, na.rm = T), .by = c(month)) %>% 
    summarise(m_temp = mean(mean_temp))
  
  t <- df %>% 
    filter(town == city) %>% 
    summarise(mean_temp = mean(temp_mean, na.rm = T), .by = c(month)) %>%
    mutate(col = if_else(mean_temp > 0, "pos", "neg")) %>% 
    ggplot(aes(month, mean_temp, fill = col)) +
    geom_col(show.legend = F) +
    scale_fill_manual(values = c("pos" = "red", "neg" = "blue")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    geom_text(aes(label = paste0(round(mean_temp, 1), " (\u00B0C)")), size = 5, vjust = -0.3) +
    labs(x = NULL, y = "Средна денонощна\nтемпература", 
         title = paste0("Средна годишна температура: ", round(m_temp$m_temp, 1), " (\u00B0C)")) +
    theme_bw() +
    theme(text = element_text(size = 20), plot.title = element_text(color = "red", face = "bold"),
          axis.text = element_blank(), 
          axis.ticks = element_blank())
  
  s_rain <- df %>% 
    filter(town == city) %>% 
    summarise(sum_rain = sum(prec_sum, na.rm = T), .by = c(year, month)) %>% 
    summarise(mean_rain = mean(sum_rain), .by = c(month)) %>% 
    summarise(s_rain = sum(mean_rain))
  
  r <- df %>% 
    filter(town == city) %>% 
    summarise(sum_rain = sum(prec_sum, na.rm = T), .by = c(year, month)) %>% 
    summarise(mean_rain = mean(sum_rain), .by = c(month)) %>%
    ggplot(aes(month, mean_rain)) +
    geom_col(fill = "blue") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    geom_text(aes(label = paste0(round(mean_rain, 0), " mm")), size = 6, vjust = -0.3) +
    labs(x = NULL, y = "Средно количетсво\nна валежите",
         title = paste0("Средна годишна сума на валежите: ", round(s_rain$s_rain, 0), " mm")) +
    theme_bw() +
    theme(text = element_text(size = 20), plot.title = element_text(color = "blue", face = "bold"),
          axis.text = element_blank(), 
          axis.ticks = element_blank())
  
  s_snow <- df %>% 
    filter(town == city) %>% 
    summarise(sum_snow = sum(snow_sum, na.rm = T), .by = c(year, month)) %>% 
    summarise(mean_snow = mean(sum_snow), .by = c(month)) %>%
    summarise(s_snow = sum(mean_snow))
  
  s <- df %>% 
    filter(town == city) %>% 
    summarise(sum_snow = sum(snow_sum, na.rm = T), .by = c(year, month)) %>% 
    summarise(mean_snow = mean(sum_snow), .by = c(month)) %>%
    ggplot(aes(month, mean_snow)) +
    geom_col(fill = "#00BFC4") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    geom_text(aes(label = paste0(round(mean_snow, 0), " cm")), size = 6, vjust = -0.3) +
    coord_cartesian(expand = F, ylim = c(0, 100)) +
    labs(x = "Месеци", y = "Средна височина\nна снежната покривка",
         title = paste0("Средна годишна височина на снежната покривка: ", round(s_snow$s_snow, 0), " cm")) +
    theme_bw() +
    theme(text = element_text(size = 20), plot.title = element_text(color = "#00BFC4", face = "bold"),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
  
  t/r/s
  
}
