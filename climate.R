library(tidyverse)
library(rvest)
library(arrow)

rain <- read_parquet("climate/rain.parquet")
temp <- read_parquet("climate/temp.parquet")
#-------------------------------------------
rain_new <- read_html("https://www.stringmeteo.com/synop/prec_month.php") %>%
  html_element("table") %>% html_table() %>%
  select(2:17, 19:34) %>% slice(12:21, 25:32) %>%
  rename(station = X2) %>% rename_with(~ as.character(c(1:31)), starts_with("X")) %>%
  mutate(
    station = str_remove_all(station, "\\("), 
    station = str_remove_all(station, "\\)"),
    status = case_when(
      station %in% c("Видин", "Ловеч", "Разград", "Варна", "вр. Мургаш",
                     "София", "вр. Мусала", "Пазарджик", "Сливен", "Бургас",
                     "Сандански", "Кърджали") ~ "official", 
      station %in% c("с. Гложене", "Варна-Акчелар", "Варна-Боровец", "Климентово",
                     "Обзор", "Дупница", "Орландовци", "Бояна", "Княжево", "Панагюрище",
                     "Ямбол", "Петрич", "Стралджа", "Шумен") ~ "unofficial",
      str_detect(station, "Конгур") ~ "unofficial"), 
    .after = station,
    year = 2024, month = 9,
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
  #mutate(station = fct_recode(station, "Гложене" = "с. Гложене")) %>%
  mutate(decade = case_when(
    year %in% c("2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023", "2024") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
  pivot_longer(7:37, names_to = "day", values_to = "rain") %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(across(c(3, 8), as.double))

temp_new <- read_html("https://www.stringmeteo.com/synop/temp_month.php") %>%
  html_element("table") %>% html_table() %>%
  select(2:17, 19:34) %>% slice(12:21, 25:31) %>%
  rename(station = X2) %>% rename_with(~ as.character(c(1:31)), starts_with("X")) %>%
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
                     "Кълъраш Р.", "Одрин Т.", "Рилци", "Добри дол") ~ "unofficial"), 
    .after = station,
    year = 2024, month = 9,
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
    year %in% c("2020", "2021", "2022", "2023", "2024") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
  pivot_longer(7:37, names_to = "day", values_to = "temp") %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(across(c(3, 8), as.double))

rain <- bind_rows(rain, rain_new) %>% 
  mutate(month = factor(month, levels = c(1:12)))
temp <- bind_rows(temp, temp_new)
#-----------------------------------------------
temp %>% 
  filter(month %in% c(9), elev < 1200, status == "official") %>%
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
  mutate(mm = mean(m, na.rm = T), iqr = IQR(m), col = case_when(
    m > mm + iqr ~ "1",
    m > mm ~ "2",
    m < mm - iqr ~ "4",
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
  labs(x = NULL, y = "Средна денонощна температура (\u00B0C)", 
       fill = "Легенда:", title = "Месец: Септември") +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 16), legend.position = "top")

rain %>% 
  filter(month %in% c(9), elev < 1200, status == "official") %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(s = mean(s, na.rm = T), .by = c(year, month)) %>% 
  mutate(ss = mean(s), iqr = IQR(s), col = case_when(
    s > ss + iqr / 1.5 ~ "0",
    s < ss - iqr / 1.5 ~ "3",
    s > ss ~ "1",
    s <= ss ~ "2")) %>%
  ggplot(aes(year, s, fill = col)) +
  geom_col() +
  geom_text(aes(label = paste0(round(s, 0))), size = 5, vjust = -0.3) +
  geom_hline(aes(yintercept = ss), linewidth = 0.5, lty = 2, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 10) +
  scale_fill_manual(values = c("0" = "blue" , "1" = "#0096FF" , 
                               "2" = "orange", "3" = "red"), 
                    labels = c("0" = "Много по-дъждовно от средното", 
                               "1" = "По-дъждовно от средното", 
                               "2" = "По-сухо от средното",
                               "3" = "Много по-сухо от средното")) +
  labs(x = NULL, y = "Месечно количество на валежите (mm)", 
       fill = "Легенда:", title = "Месец: Септември") +
  theme(text = element_text(size = 16), legend.position = "top")
#---------------------------------------------------------------
colors <- c("1" = "red", "2" = "orange" , "3" = "green", "4" = "#0096FF", "5" = "blue")
labels <- c("1" = "Много по-топло от средното", "2" = "По-топло от средното" ,
            "3" = "Умерено", "4" = "По-хладно от средното", "5" = "Много по-хладно от средното")

t_year <- temp %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
  summarise(mean_year = mean(m), .by = year)
  
temp %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
  group_by(month) %>%
  mutate(mm = round(mean(m, na.rm = T), 1), 
         iqr = IQR(m), col = case_when(
    m < mm - iqr * 1.2 ~ "5",
    m > mm + iqr * 1.2 ~ "1",
    m < mm - iqr ~ "4",
    m > mm + iqr ~ "2",
    m <= mm + iqr ~ "3")) %>% 
  ungroup() %>%
  ggplot(aes(month, m)) +
  geom_col(aes(fill = col), show.legend = T) +
  geom_text(aes(label = round(m, 1)), size = 3.5, vjust = -0.2) +
  geom_text(data = t_year,
            aes(label = paste(round(mean_year, 1), "(\u00B0C)"), x = 7, y = 40),
            size = 8, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 4) +
  scale_fill_manual(values = colors, labels = labels) +
  labs(x = "Месеци", y = "Средна денонощна температура (\u00B0C)", fill = "Легенда:") +
  theme(text = element_text(size = 16), legend.position = "top") +
  facet_wrap(vars(year))

d_year <- rain %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(sm = round(mean(s, na.rm = T), 1), .by = c(year, month)) %>%
  summarise(s_year = sum(sm), .by = year)
  
rain %>%
  filter(month %in% c(1:12), elev < 1200, status == "official") %>% 
  summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
  summarise(sm = round(mean(s, na.rm = T), 1), .by = c(year, month)) %>%
  group_by(month) %>% 
  mutate(ss = round(mean(sm, na.rm = T), 1), 
         iqr = IQR(sm), col = case_when(
           sm < ss - iqr * 1.2 ~ "5",
           sm > ss + iqr * 1.2 ~ "1",
           sm < ss - iqr / 1.2 ~ "4",
           sm > ss + iqr / 1.2 ~ "2",
           sm <= ss + iqr ~ "3")) %>% 
  ungroup() %>%
  ggplot(aes(month, sm)) +
  geom_col(aes(fill = col), show.legend = T) +
  geom_text(aes(label = round(sm, 0)), size = 4, vjust = -0.2) +
  geom_text(data = d_year, 
            aes(label = paste(round(s_year, 0), "(mm)"), x = 7, y = 200), 
            size = 8, vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)), n.breaks = 4) +
  scale_fill_manual(values = c("1" = "blue" , "2" = "#0096FF" , "3" = "green",
                               "4" = "orange", "5" = "red"), 
                    labels = c("1" = "Много по-дъждовно от средното", 
                               "2" = "По-дъждовно от средното",
                               "3" = "Умерено",
                               "4" = "По-сухо от средното",
                               "5" = "Много по-сухо от средното")) +
  labs(x = "Месеци", y = "Месечно количество на валежите (mm)", fill = "Легенда:", title = NULL) +
  theme(text = element_text(size = 16), legend.position = "top") +
  facet_wrap(vars(year))
#---------------------------------------------------------------
mean_temp_month <- temp %>%
  drop_na(temp) %>% 
  filter(month %in% c(7), elev < 1200) %>%
  summarise(m = round(mean(temp), 1),	n = n(), .by = year)
temp %>%
  drop_na(temp) %>% 
  filter(month %in% c(7), elev < 1200) %>%
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
       fill = "Легенда:", title = "Месец: Юли") +
  theme(text = element_text(size = 16), legend.position = "top") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), 
                    labels = c("По-хладно от средното", "По-топло от средното")) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(labels = paste0(mean_temp_month$year, "\n(n = ", mean_temp_month$n, ")")) +
  guides(fill = guide_legend(reverse = TRUE))

mean_rain_month <- rain %>% 
  drop_na(rain) %>% 
  filter(month %in% c(7), elev < 1200) %>%
  group_by(station, year, month) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>% 
  summarise(m = round(mean(sum), 0), n = n(), .by = c(year, month))
rain %>%
  drop_na(rain) %>% 
  filter(month %in% c(7), elev < 1200) %>%
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
       fill = "Легенда:", title = "Месец: Юли") +
  theme(text = element_text(size = 16), legend.position = "top") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), 
                    labels = c("По-сухо от средното", "По-дъждовно от средното")) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(labels = paste0(mean_rain_month$year, "\n(n = ", mean_rain_month$n, ")")) +
  guides(fill = guide_legend(reverse = TRUE))
#--------------------------------------------
mean_temp_year <- temp %>% 
  drop_na(temp) %>% 
  filter(elev < 1200) %>%
  summarise(m = round(mean(temp), 2), n = n(), .by = year)
temp %>%
  drop_na(temp) %>% 
  filter(elev < 1200) %>%
  mutate(m = mean(temp)) %>%
  group_by(year) %>%
  mutate(col = mean(temp) > m) %>%
  ggplot(aes(year, temp)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_temp_year, aes(year, m), color = "white") +
  geom_text(data = mean_temp_year, aes(year, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(temp)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средна годишна температура (\u00B0C)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "top") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("По-студена от средното",
                                                                 "По-топла от средното")) +
  scale_y_continuous(n.breaks = 20) +
  scale_x_discrete(labels = paste0(mean_temp_year$year, "\n(n = ", mean_temp_year$n, ")"))

mean_rain_year <- rain %>% 
  drop_na(rain) %>% 
  filter(elev < 1200) %>%
  group_by(station, year) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>% 
  summarise(m = round(mean(sum), 0), n = n(), .by = year)
rain %>%
  drop_na(rain) %>% 
  filter(elev < 1200) %>%
  group_by(station, year) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(year) %>%
  mutate(col = mean(sum) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_year, aes(year, m), color = "white") +
  geom_text(data = mean_rain_year, aes(year, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(sum)), linewidth = 0.5, lty = 2, color = "black") +
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
write_parquet(rain, "climate/rain.parquet")
write_parquet(temp, "climate/temp.parquet")
