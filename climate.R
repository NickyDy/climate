library(tidyverse)
library(rvest)

rain <- read_rds("climate/rain.rds")
temp <- read_rds("climate/temp.rds")
#-------------------------------------------
rain_new <- read_html("https://www.stringmeteo.com/synop/prec_month.php") %>%
  html_element("table") %>% 
  html_table() %>%
  filter(str_detect(X1, "^[:punct:]\\d{1,2}[:punct:]")) %>% 
  select(2:17, 19:34) %>%
  rename(station = X2) %>% 
  rename_with(~ as.character(c(1:31)), starts_with("X")) %>%
  mutate(
    station = str_remove_all(station, "\\("), 
    station = str_remove_all(station, "\\)"),
    status = case_when(
      station %in% c("Видин", "Ловеч", "Разград", "Варна", "вр. Мургаш",
                     "София", "вр. Мусала", "Пазарджик", "Сливен", "Бургас",
                     "Сандански", "Кърджали") ~ "official", 
      station %in% c("с. Гложене", "Варна-Акчелар", "Варна-Боровец", "Климентово",
                     "Обзор", "Дупница", "Орландовци", "Бояна", "Княжево", "Панагюрище",
                     "Ямбол", "Петрич", "Стралджа", "Шумен", "с.Рупите") ~ "unofficial",
      str_detect(station, "Ключ") ~ "unofficial",
      str_detect(station, "Рупите") ~ "unofficial",
      str_detect(station, "Илинденци") ~ "unofficial",
      str_detect(station, "Конгур") ~ "unofficial"), .after = station,
    year = 2025, month = 12,
    elev = case_when(
      station == "Видин" ~ 31, station == "Ловеч" ~ 220, str_detect(station, "Конгур") ~ 1284,
      station == "Разград" ~ 345, station == "Варна" ~ 41, station == "Варна-Акчелар" ~ 180,
      station == "Варна-Боровец" ~ 193, station == "Климентово" ~ 281, station == "Обзор" ~ 20,
      station == "вр. Мургаш" ~ 1687, station == "Дупница" ~ 551, station == "София" ~ 586,
      station == "Орландовци" ~ 527, station == "Бояна" ~ 744, station == "Княжево" ~ 700,
      station == "вр. Мусала" ~ 2925, station == "Панагюрище" ~ 519, station == "Пазарджик" ~ 213,
      station == "Сливен" ~ 257, station == "Ямбол" ~ 147, station == "Бургас" ~ 16,
      station == "Петрич" ~ 200, station == "Сандански" ~ 206, station == "Стралджа" ~ 139,
      station == "Кърджали" ~ 330, station == "Шумен" ~ 218, station == "с. Гложене" ~ 64)) %>%
 mutate(decade = case_when(
    year %in% c("2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023", "2024", "2025") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
  pivot_longer(7:37, names_to = "day", values_to = "rain") %>%
  mutate(rain = str_remove(rain, "---")) %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(rain = parse_number(rain))

temp_new <- read_html("https://www.stringmeteo.com/synop/temp_month.php") %>%
  html_element("table") %>% 
  html_table() %>%
  filter(str_detect(X1, "^[:punct:]\\d{1,2}[:punct:]")) %>% 
  select(2:17, 19:34) %>%
  rename(station = X2) %>% 
  rename_with(~ as.character(c(1:31)), starts_with("X")) %>%
  mutate(
    station = str_remove_all(station, "\\("), 
    station = str_remove_all(station, "\\)"),
    station = str_squish(station),
    status = case_when(
      station %in% c("Видин", "Ловеч", "Разград", "Варна", "вр. Мургаш", "София",
                     "вр. Мусала", "Пазарджик", "Сливен", "Бургас", "Сандански",
                     "Кърджали", "Димитровгр. С.") ~ "official", 
      station %in% c("Гложене", "Варна-Акчелар", "Варна-Боровец", "Климентово",
                     "Обзор", "Дупница", "Орландовци", "Бояна", "Княжево",
                     "Панагюрище", "Ямбол", "Петрич", "Турну Мъгуреле Р.",
                     "Кълъраш Р.", "Одрин Т.", "Рилци", "Добри дол") ~ "unofficial"), .after = station,
    year = 2025, month = 12,
    elev = case_when(
      station == "Видин" ~ 31, station == "Гложене" ~ 64, station == "Ловеч" ~ 220, station == "Разград" ~ 345,
      station == "Варна" ~ 41, station == "Варна-Акчелар" ~ 180, station == "Варна-Боровец" ~ 193,
      station == "Климентово" ~ 281, station == "Обзор" ~ 20, station == "вр. Мургаш" ~ 1687,
      station == "Дупница" ~ 551, station == "София" ~ 586, station == "Орландовци" ~ 527,
      station == "Бояна" ~ 744, station == "Княжево" ~ 700, station == "вр. Мусала" ~ 2925,
      station == "Панагюрище" ~ 519, station == "Пазарджик" ~ 213, station == "Сливен" ~ 257,
      station == "Ямбол" ~ 147, station == "Бургас" ~ 16, station == "Петрич" ~ 200,
      station == "Сандански" ~ 206, station == "Кърджали" ~ 330, station == "Добри дол" ~ 118,
      station == "Турну Мъгуреле Р." ~ 31, station == "Димитровгр. С." ~ 125,
      station == "Кълъраш Р." ~ 12, station == "Одрин Т." ~ 42, station == "Рилци" ~ 378)) %>%
  mutate(decade = case_when(
    year %in% c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023", "2024", "2025") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
  pivot_longer(7:37, names_to = "day", values_to = "temp") %>% 
  mutate(temp = str_remove(temp, "---")) %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(temp = parse_number(temp))

rain <- bind_rows(rain, rain_new) %>% 
  mutate(month = factor(month, levels = c(1:12)))
temp <- bind_rows(temp, temp_new)
#--------------------------------
colors_temp <- c("1" = "red", "2" = "orange" , "3" = "green", 
                 "4" = "#0096FF", "5" = "blue")
labels_temp <- c("1" = "Много топло", "2" = "Топло" , "3" = "Умерено", 
                 "4" = "Хладно", "5" = "Много хладно")
colors_rain <- c("1" = "blue" , "2" = "#0096FF" , "3" = "green", 
                 "4" = "orange", "5" = "red")
labels_rain <- c("1" = "Много дъждовно", "2" = "Дъждовно", "3" = "Умерено", 
                 "4" = "Сухо", "5" = "Много сухо")

temp %>% 
  filter(month %in% c(12), elev < 1200, status == "official") %>%
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year)) %>%
  mutate(mm = mean(m, na.rm = T), iqr = IQR(m), col = case_when(
    m > mm + iqr * 1.2 ~ "1",
    m < mm - iqr * 1.2 ~ "5",
    m > mm + iqr * 0.5 ~ "2",
    m < mm - iqr * 0.5 ~ "4",
    m <= mm + iqr * 0.5 ~ "3")) %>%
  ggplot(aes(year, m, fill = col)) +
  geom_hline(aes(yintercept = mean(m)), 
             linewidth = 0.5, lty = 2, color = "black") +
  geom_col() +
  geom_text(aes(label = paste0(round(m, 1))), size = 5, vjust = -0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = colors_temp, labels = labels_temp) +
  labs(x = NULL, y = "Средна денонощна температура (\u00B0C)", 
       fill = "Легенда:", title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.justification = c(1, 0))
rain %>% 
  filter(month %in% c(12), elev < 1200, status == "official") %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(s = mean(s, na.rm = T), .by = c(year)) %>% 
  mutate(ss = mean(s), iqr = IQR(s), col = case_when(
    s > ss + iqr ~ "1",
    s < ss - iqr ~ "5",
    s > ss + iqr * 0.5 ~ "2",
    s < ss - iqr * 0.5 ~ "4",
    s <= ss + iqr * 0.5 ~ "3")) %>%
  ggplot(aes(year, s, fill = col)) +
  geom_hline(aes(yintercept = mean(s)), 
             linewidth = 0.5, lty = 2, color = "black") +
  geom_col() +
  geom_text(aes(label = paste0(round(s, 0))), size = 5, vjust = -0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = colors_rain, labels = labels_rain) +
  labs(x = NULL, y = "Средно количество на валежите (mm)", 
       fill = "Легенда:", title = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.justification = c(1, 0))
#--------------------------------------
month_mean_temp <- temp %>% 
  filter(month %in% c(12), elev < 1200, status == "official") %>%
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year)) %>%
  summarise(month_m = mean(m, na.rm = T))
  
temp %>% 
  filter(month %in% c(12), elev < 1200, status == "official") %>%
  summarise(mean_temp = mean(temp, na.rm = T), .by = c(year, month, day)) %>%
  group_by(year) %>% 
  mutate(temp_year = mean(mean_temp, na.rm = T), 
         label_year = paste0(year, ": ", round(temp_year, 1), " (\u00B0C)")) %>% 
  ungroup() %>%
  mutate(col = case_when(
    mean_temp <= 0 ~ "1",
    mean_temp <= 5 ~ "2", 
    mean_temp >= 27 ~ "4", 
    .default = "3")) %>%
  ggplot(aes(day, mean_temp)) +
  geom_col(aes(fill = col), show.legend = T) +
  theme(text = element_text(size = 14), legend.position = "top",
        plot.title = element_text(color = "red", face = "bold")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "orange", "4" = "red"),
                    labels = c("1" = "< 0 \u00B0C", "2" = "0-5 \u00B0C", 
                               "3" = "5-27 \u00B0C", "4" = "> 27 \u00B0C"),
                    name = "Легенда: ") +
  labs(x = "Ден", y = "Средна денонощна температура (\u00B0C)",
       title = paste0("Средно за месеца: ", round(month_mean_temp$month_m, 1), " (\u00B0C)")) +
  facet_wrap(vars(label_year), ncol = 3)

month_mean_rain <- rain %>% 
  filter(month %in% c(12), elev < 1200, status == "official") %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(s = mean(s, na.rm = T), .by = c(year)) %>%
  summarise(month_m = mean(s))
  
rain %>% 
  filter(month %in% c(12), elev < 1200, status == "official") %>%
  summarise(sum_rain = round(sum(rain, na.rm = T), 1), .by = c(station, year, month, day)) %>%
  summarise(mean_year = mean(sum_rain, na.rm = T), .by = c(year, month, day)) %>% 
  group_by(year) %>% 
  mutate(rain_year = sum(mean_year, na.rm = T), 
         label_year = paste0(year, ": ", round(rain_year, 0), " (mm)")) %>% 
  ungroup() %>% 
  mutate(col = case_when(
    mean_year <= 5 ~ "1",
    mean_year <= 15 ~ "2", 
    mean_year >= 30 ~ "4", 
    .default = "3")) %>%
  ggplot(aes(day, mean_year)) +
  geom_col(aes(fill = col), show.legend = T) +
  theme(text = element_text(size = 14), legend.position = "top",
        plot.title = element_text(color = "blue", face = "bold")) +
  scale_fill_manual(values = c("4" = "blue", "3" = "#0096FF", 
                               "2" = "green", "1" = "orange"),
                    labels = c("1" = "< 5 (mm)", "2" = "5-15 (mm)", 
                               "3" = "15-30 (mm)", "4" = "> 30 (mm)"),
                    name = "Легенда: ") +
  labs(x = "Ден", y = "Средно количетсво на валежите (mm)", 
       title = paste0("Средно за месеца: ", round(month_mean_rain$month_m, 0), " (mm)")) +
  facet_wrap(vars(label_year), ncol = 3)

# rain %>%
#   summarise(rain = sum(rain, na.rm = T), .by = c(station, year, month)) %>%
#   mutate(across(year:month, as.character)) %>%
#   mutate(date = make_date(year, month)) %>%
#   filter(rain > 100) %>%
#   ggplot(aes(date, rain)) +
#   geom_point() +
#   geom_smooth(se = F)
#----------------------
t_year <- temp %>% 
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
  summarise(mean_year = mean(m), .by = year) %>% 
  mutate(tot_mean = round(mean(mean_year), 1))
  
temp %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
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
  geom_text(aes(label = round(m, 1)), size = 3, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = colors_temp, labels = labels_temp) +
  labs(x = "Месеци", y = "Средна денонощна температура (\u00B0C)", fill = "Легенда:",
       title = paste0("Средно за периода (", first(t_year$year), "-", 
                      last(t_year$year), " г.): ", t_year$tot_mean, " (\u00B0C)")) +
  theme(text = element_text(size = 16), legend.position = "top",
        plot.title = element_text(color = "red", face = "bold"),
        legend.justification = c(1, 0)) +
  facet_wrap(vars(label_year))

d_year <- rain %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(sm = round(mean(s, na.rm = T), 1), .by = c(year, month)) %>%
  summarise(s_year = sum(sm), .by = year) %>% 
  mutate(tot_mean = round(mean(s_year), 0))

rain %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>%
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(sm = round(mean(s, na.rm = T), 1), .by = c(year, month)) %>%
  group_by(month) %>% 
  mutate(ss = round(mean(sm, na.rm = T), 1), 
         iqr = IQR(sm), col = case_when(
           sm < ss - iqr ~ "5",
           sm > ss + iqr ~ "1",
           sm < ss - iqr * 0.5  ~ "4",
           sm > ss + iqr * 0.5  ~ "2",
           sm <= ss + iqr * 0.5 ~ "3")) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(mean_year = sum(sm, na.rm = T), 
         label_year = paste0(year, ": ", round(mean_year, 0), " (mm)")) %>% 
  ungroup() %>% 
  ggplot(aes(month, sm)) +
  geom_col(aes(fill = col), show.legend = T) +
  geom_text(aes(label = round(sm, 0)), size = 4, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = colors_rain, labels = labels_rain) +
  labs(x = "Месеци", y = "Средно количество на валежите (mm)", fill = "Легенда:", 
       title = paste0("Средно за периода (", first(d_year$year), "-", 
                      last(d_year$year), " г.): ", d_year$tot_mean, " (mm)")) +
  theme(text = element_text(size = 16), legend.position = "top", 
        plot.title = element_text(color = "blue", face = "bold"),
        legend.justification = c(1, 0)) +
  facet_wrap(vars(label_year))
#=======================
climagram("Пловдив")

climagram <- function(station) {
total_rain <- rain %>%
  filter(station == {{station}}) %>%
  summarise(sum_rain = sum(rain, na.rm = T), 
            .by = c(station, year, month)) %>%
  summarise(mean_rain = mean(sum_rain), .by = c(month)) %>% 
  summarise(s = sum(mean_rain)) %>% pull()
rain %>%
  filter(station == {{station}}) %>% 
  summarise(sum_rain = sum(rain, na.rm = T), .by = c(station, year, month)) %>% 
  summarise(mean_rain = mean(sum_rain), .by = c(month)) %>%
  ggplot(aes(month, mean_rain)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = paste0(round(mean_rain, 0), " mm")), size = 6, vjust = -0.3) +
  annotate(geom = "text", x = 3, y = 140, 
           label = paste0("Средна годишна сума (", station, ")", ": ", round(total_rain, 0), " (mm)"), 
           size = 7, color = "blue", fontface = "bold") +
  coord_cartesian(expand = F, ylim = c(0, 150)) +
  labs(x = "Месеци", y = "Средно месечно количество на валежите") +
  theme_bw() +
  theme(text = element_text(size = 20), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())
}

total_rain <- rain %>%
  filter(station %in% c("София", "Ямбол", "Елхово", "Бургас", "Варна", "Кърджали",
                        "Пловдив", "Велико Търново", "Сливен", "Видин", "Русе",
                        "Търговище", "Разград", "Пазарджик", "Ловеч",
                        "Сандански", "вр. Мусала", "вр. Мургаш", "Стралджа",
                        "Панагюрище", "Враца")) %>%
  summarise(sum_rain = sum(rain, na.rm = T), 
            .by = c(station, year, month)) %>%
  summarise(mean_rain = mean(sum_rain), .by = c(station, month)) %>% 
  summarise(s = sum(mean_rain), .by = station)

rain %>%
  filter(station %in% c("София", "Ямбол", "Елхово", "Бургас", "Варна", "Кърджали",
                        "Пловдив", "Велико Търново", "Сливен", "Видин", "Русе",
                        "Търговище", "Разград", "Пазарджик", "Ловеч",
                        "Сандански", "вр. Мусала", "вр. Мургаш", "Стралджа",
                        "Панагюрище", "Враца")) %>%
  summarise(sum_rain = sum(rain, na.rm = T), .by = c(station, year, month)) %>% 
  summarise(mean_rain = mean(sum_rain), .by = c(station, month)) %>%
  ggplot(aes(month, mean_rain)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = paste0(round(mean_rain, 0))), size = 5, vjust = -0.3) +
  geom_text(data = total_rain, x = 4.5, y = 162,
           label = paste0("Средна годишна сума", ": ", 
                          round(total_rain$s, 0), " (mm)"),
           size = 4, color = "blue", fontface = "bold") +
  coord_cartesian(expand = F, ylim = c(0, 170)) +
  labs(x = "Месеци", y = "Средно месечно количество на валежите (mm)") +
  theme_bw() +
  theme(text = element_text(size = 16), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(vars(station), ncol = 5)

temp %>%
  filter(elev <= 1200, status == "official") %>% 
  summarise(mean_temp = mean(temp, na.rm = T), .by = c(month)) %>%
  ggplot(aes(month, mean_temp)) +
  geom_col(fill = "red") +
  geom_text(aes(label = paste0(round(mean_temp, 2))), size = 5, vjust = -0.3) +
  labs(x = "Месеци", y = "Средна денонощна температура (\u00B0C)") +
  theme_bw() +
  theme(text = element_text(size = 16), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())
rain %>%
  filter(elev <= 1200, status == "official") %>%
  summarise(sum_rain = sum(rain, na.rm = T), .by = c(station, year, month)) %>% 
  summarise(mean_rain = mean(sum_rain), .by = c(month)) %>%
  ggplot(aes(month, mean_rain)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = paste0(round(mean_rain, 0), " mm")), size = 6, vjust = -0.3) +
  labs(x = "Месеци", y = "Средно месечно количество на валежите") +
  theme_bw() +
  theme(text = element_text(size = 20), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())
#--------------------------------------
mean_temp_month <- temp %>%
  drop_na(temp) %>% 
  filter(month %in% c(10), elev < 1200, status == "official") %>%
  summarise(m = round(mean(temp), 1),	n = n(), .by = year)
temp %>%
  drop_na(temp) %>% 
  filter(month %in% c(10), elev < 1200, status == "official") %>%
  mutate(m = mean(temp)) %>% 
  group_by(year) %>%
  mutate(col = mean(temp) > m) %>% 
  ggplot(aes(year, temp)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_temp_month, aes(year, m), color = "white") +
  geom_text(data = mean_temp_month, aes(year, m, label = m), 
            size = 14, vjust = -0.5, size.unit = "pt") +
  geom_hline(aes(yintercept = mean(temp)), 
             linewidth = 0.5, lty = 2, color = "black") +
  labs(x = NULL, y = "Средна месечна температура (\u00B0C)", 
       fill = "Легенда:", title = "Месец: ") +
  theme(text = element_text(size = 16), legend.position = "top") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), 
                    labels = c("По-хладно от средното", "По-топло от средното")) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(labels = paste0(mean_temp_month$year, "\n(n = ", mean_temp_month$n, ")")) +
  guides(fill = guide_legend(reverse = TRUE))

mean_rain_month <- rain %>% 
  drop_na(rain) %>% 
  filter(month %in% c(10), elev < 1200, status == "official") %>%
  group_by(station, year, month) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>% 
  summarise(m = round(mean(sum), 0), n = n(), .by = c(year, month))
rain %>%
  drop_na(rain) %>% 
  filter(month %in% c(10), elev < 1200, status == "official") %>%
  group_by(station, year, month) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(year) %>%
  mutate(col = mean(sum) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_month, aes(year, m), color = "white") +
  geom_text(data = mean_rain_month, aes(year, m, label = m), 
            position = position_dodge(width = 1), 
            size = 14, vjust = -0.5, size.unit = "pt") +
  geom_hline(aes(yintercept = mean(sum)), 
             linewidth = 0.5, lty = 2, color = "black") +
  labs(x = NULL, y = "Месечно количество на валежите (mm)", 
       fill = "Легенда:", title = "Месец: ") +
  theme(text = element_text(size = 16), legend.position = "top") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), 
                    labels = c("По-сухо от средното", "По-дъждовно от средното")) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(labels = paste0(mean_rain_month$year, "\n(n = ", mean_rain_month$n, ")")) +
  guides(fill = guide_legend(reverse = TRUE))
#--------------------------------------------
mean_temp_year <- temp %>% 
  drop_na(temp) %>% 
  filter(elev < 1200, status == "official") %>%
  summarise(m = round(mean(temp), 2), n = n(), .by = year)
temp %>%
  drop_na(temp) %>% 
  filter(elev < 1200, status == "official") %>%
  mutate(m = mean(temp)) %>%
  group_by(year) %>%
  mutate(col = mean(temp) > m) %>%
  ggplot(aes(year, temp)) +
  geom_hline(aes(yintercept = mean(temp)), linewidth = 0.5, lty = 2, color = "black") +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_temp_year, aes(year, m), color = "white") +
  geom_text(data = mean_temp_year, aes(year, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  labs(x = "Години", y = "Средна годишна температура (\u00B0C)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "top") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("По-студена от средното",
                                                                 "По-топла от средното")) +
  scale_y_continuous(n.breaks = 20) +
  scale_x_discrete(labels = paste0(mean_temp_year$year, "\n(n = ", mean_temp_year$n, ")"))

mean_rain_year <- rain %>% 
  drop_na(rain) %>% 
  filter(elev < 1200, status == "official") %>%
  group_by(station, year) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>% 
  summarise(m = round(mean(sum), 0), n = n(), .by = year)
rain %>%
  drop_na(rain) %>% 
  filter(elev < 1200, status == "official") %>%
  group_by(station, year) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(year) %>%
  mutate(col = mean(sum) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_hline(aes(yintercept = mean(sum)), linewidth = 0.5, lty = 2, color = "black") +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_year, aes(year, m), color = "white") +
  geom_text(data = mean_rain_year, aes(year, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  labs(x = "Години", y = "Годишно количество на валежите (mm)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "top") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), 
                    labels = c("По-суха от средното", "По-дъждовна от средното")) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(labels = paste0(mean_rain_year$year, "\n(n = ", mean_rain_year$n, ")"))
#------------------------------------------
mean_temp_decade <- temp %>% 
  drop_na(temp) %>% 
  filter(elev < 1200) %>%
  summarise(m = round(mean(temp), 2), n = n(), .by = decade)
temp %>%
  drop_na(temp) %>% 
  filter(elev < 1200) %>%
  mutate(m = mean(temp)) %>%
  group_by(decade) %>%
  mutate(col = mean(temp) > m) %>%
  ggplot(aes(decade, temp)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_temp_decade, aes(decade, m), color = "white") +
  geom_text(data = mean_temp_decade, aes(decade, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(temp)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Десетилетие", y = "Средна годишна температура (\u00B0C)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "top") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),
                    labels = c("По-студено от средното", "По-топло от средното"))

mean_rain_decade <- rain %>% 
  drop_na(rain) %>% 
  filter(elev < 1200) %>%
  group_by(station, year, decade) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>% 
  summarise(m = round(mean(sum), 0), n = n(), .by = decade)
rain %>%
  drop_na(rain) %>% 
  filter(elev < 1200) %>%
  group_by(station, year, decade) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(decade) %>%
  mutate(col = mean(sum) > su) %>%
  ggplot(aes(decade, sum)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_decade, aes(decade, m), color = "white") +
  geom_text(data = mean_rain_decade, aes(decade, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(sum)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Десетилетие", y = "Средно годишно количество на валежите (mm)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "top") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                    labels = c("По-сухо от средното", "По-дъждовно от средното"))
#----------------------------------------------------
write_rds(rain, "climate/rain.rds")
write_rds(temp, "climate/temp.rds")
write_rds(rain, "shiny/climate/rain.rds")
write_rds(temp, "shiny/climate/temp.rds")
