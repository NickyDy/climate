library(tidyverse)
library(rvest)

rain <- read_rds("~/Desktop/R/climate/rain.rds")
temp <- read_rds("~/Desktop/R/climate/temp.rds")

rain_new <- read_html("https://www.stringmeteo.com/synop/prec_month.php") %>%
	html_element("table") %>% html_table() %>%
	select(2:17, 19:34) %>% slice(12:21, 25:34, 38:42) %>%
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
      							 "Ямбол", "Петрич", "Стралджа", "Шумен") ~ "unofficial"), 
    .after = station,
    year = 2023, month = 6,
    elev = case_when(
      station == "Видин" ~ 31, station == "Ловеч" ~ 220,
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
    year %in% c("2020", "2021", "2022", "2023") ~ "20s")) %>%
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
      							 "Кълъраш Р.", "Одрин Т.", "Рилци") ~ "unofficial"), 
    .after = station,
    year = 2023, month = 6,
    elev = case_when(
      station == "Видин" ~ 31, station == "Гложене" ~ 64, station == "Ловеч" ~ 220, station == "Разград" ~ 345,
      station == "Варна" ~ 41, station == "Варна-Акчелар" ~ 180, station == "Варна-Боровец" ~ 193,
      station == "Климентово" ~ 281, station == "Обзор" ~ 20, station == "вр. Мургаш" ~ 1687,
      station == "Дупница" ~ 551, station == "София" ~ 586, station == "Орландовци" ~ 527,
      station == "Бояна" ~ 744, station == "Княжево" ~ 700, station == "вр. Мусала" ~ 2925,
      station == "Панагюрище" ~ 519, station == "Пазарджик" ~ 213, station == "Сливен" ~ 257,
      station == "Ямбол" ~ 147, station == "Бургас" ~ 16, station == "Петрич" ~ 200,
      station == "Сандански" ~ 206, station == "Кърджали" ~ 330,
      station == "Турну Мъгуреле Р." ~ 31, station == "Димитровгр. С." ~ 125,
      station == "Кълъраш Р." ~ 12, station == "Одрин Т." ~ 42, station == "Рилци" ~ 378)) %>%
  mutate(decade = case_when(
    year %in% c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
	pivot_longer(7:37, names_to = "day", values_to = "temp") %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(across(c(3, 8), as.double))

rain <- bind_rows(rain, rain_new) %>% 
  mutate(month = factor(month, levels = c(1:12)))
temp <- bind_rows(temp, temp_new)
#-----------------------------------------------
mean_temp_month <- temp %>%
	drop_na() %>% 
	filter(month %in% c(6), elev < 1200, day %in% c(1:18)) %>%
	group_by(year) %>% 
	summarise(m = round(mean(temp), 2),	n = n())
temp %>%
	drop_na() %>% 
  filter(month %in% c(6), elev < 1200, day %in% c(1:18)) %>%
  mutate(m = mean(temp)) %>%
  group_by(year) %>%
  mutate(col = mean(temp) > m) %>% 
	ggplot(aes(year, temp)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
	geom_point(data = mean_temp_month, aes(year, m), color = "black") +
	geom_text(data = mean_temp_month, aes(year, m, label = m), size = 4, vjust = -0.5) +
	geom_hline(aes(yintercept = mean(temp)), linewidth = 0.5, lty = 2, color = "black") +
	labs(x = "Години", y = "Средна месечна температура (ºС)", fill = "Легенда:", title = "Месец: Юни") +
  theme(text = element_text(size = 14), legend.position = "right") +
	scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("Студен", "Топъл")) +
	scale_y_continuous(n.breaks = 10) +
	scale_x_discrete(labels = paste0(mean_temp_month$year, "\n(n = ", mean_temp_month$n, ")")) +
	guides(fill = guide_legend(reverse = TRUE))

mean_rain_month <- rain %>% 
  drop_na() %>% 
  filter(month %in% c(6), elev < 1200, day %in% c(1:18)) %>%
  group_by(station, year, month) %>%
  mutate(sum = sum(rain)) %>%
  group_by(month, year) %>% 
  summarise(m = round(mean(sum), 2), n = n())
rain %>%
  drop_na() %>% 
  filter(month %in% c(6), elev < 1200, day %in% c(1:18)) %>%
  group_by(station, year, month) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(year) %>%
  mutate(col = mean(sum) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_month, aes(year, m), color = "black") +
  geom_text(data = mean_rain_month, aes(year, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(sum)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средно месечно количество на валежите (mm)", fill = "Легенда:", 
       title = "Месец: Юни") +
  theme(text = element_text(size = 14), legend.position = "right") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Сух", "Дъждовен")) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(labels = paste0(mean_rain_month$year, "\n(n = ", mean_rain_month$n, ")")) +
  guides(fill = guide_legend(reverse = TRUE))
#--------------------------------------------
mean_temp_year <- temp %>% 
	drop_na() %>% 
	filter(elev < 1200) %>%
	group_by(year) %>% 
	summarise(m = round(mean(temp), 2), n = n())
temp %>%
	drop_na() %>% 
  filter(elev < 1200) %>%
  mutate(m = mean(temp)) %>%
  group_by(year) %>%
  mutate(col = mean(temp) > m) %>%
  ggplot(aes(year, temp)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
	geom_point(data = mean_temp_year, aes(year, m), color = "black") +
	geom_text(data = mean_temp_year, aes(year, m, label = m), 
						position = position_dodge(width = 1), size = 4, vjust = -0.5) +
	geom_hline(aes(yintercept = mean(temp)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средна годишна температура (ºС)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "right") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), labels = c("Студена", "Топла")) +
	scale_y_continuous(n.breaks = 20) +
	scale_x_discrete(labels = paste0(mean_temp_year$year, "\n(n = ", mean_temp_year$n, ")")) +
	guides(fill = guide_legend(reverse = TRUE))

mean_rain_year <- rain %>% 
	drop_na() %>% 
	filter(station == "Ловеч") %>%
	group_by(station, year) %>%
	mutate(sum = sum(rain)) %>%
	group_by(year) %>% 
	summarise(m = round(mean(sum), 0), n = n())
rain %>%
	drop_na() %>% 
  filter(station == "Ловеч") %>%
  group_by(station, year) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(year) %>%
  mutate(col = mean(sum) > su) %>% 
  ggplot(aes(year, sum)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_year, aes(year, m), color = "black") +
	geom_text(data = mean_rain_year, aes(year, m, label = m), 
						position = position_dodge(width = 1), size = 4, vjust = -0.5) +
	geom_hline(aes(yintercept = mean(sum)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Години", y = "Средно годишно количество на валежите (mm)", fill = "Легенда:") +
  theme(text = element_text(size = 14), legend.position = "right") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Суха", "Дъждовна")) +
	scale_y_continuous(n.breaks = 10) +
	scale_x_discrete(labels = paste0(mean_rain_year$year, "\n(n = ", mean_rain_year$n, ")")) +
	guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
mean_temp_decade <- temp %>% 
  drop_na() %>% 
  filter(elev < 1200) %>%
  group_by(decade) %>% 
  summarise(m = round(mean(temp), 2), n = n())
temp %>%
	drop_na() %>% 
  filter(elev < 1200) %>%
  mutate(m = mean(temp)) %>%
  group_by(decade) %>%
  mutate(col = mean(temp) > m) %>%
  ggplot(aes(decade, temp)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_temp_decade, aes(decade, m), color = "black") +
  geom_text(data = mean_temp_decade, aes(decade, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(temp)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Десетилетие", y = "Средна годишна температура (ºС)") +
  theme(text = element_text(size = 14), legend.position = "none") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))

mean_rain_decade <- rain %>% 
  drop_na() %>% 
  group_by(station, year, decade) %>%
  mutate(sum = sum(rain)) %>%
  group_by(decade) %>% 
  summarise(m = round(mean(sum), 0), n = n())
rain %>%
	drop_na() %>% 
  filter(elev < 1200) %>%
  group_by(station, year, decade) %>%
  mutate(sum = sum(rain)) %>%
  ungroup() %>%
  mutate(su = mean(sum)) %>%
  group_by(decade) %>%
  mutate(col = mean(sum) > su) %>%
  ggplot(aes(decade, sum)) +
  geom_boxplot(aes(fill = col), fatten = NULL) +
  geom_point(data = mean_rain_decade, aes(decade, m), color = "black") +
  geom_text(data = mean_rain_decade, aes(decade, m, label = m), 
            position = position_dodge(width = 1), size = 4, vjust = -0.5) +
  geom_hline(aes(yintercept = mean(sum)), linewidth = 0.5, lty = 2, color = "black") +
  labs(x = "Десетилетие", y = "Средно годишно количество на валежите (mm)") +
  theme(text = element_text(size = 14), legend.position = "none") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"))
#----------------------------------------------------
write_rds(rain, "~/Desktop/R/climate/rain.rds")
write_rds(temp, "~/Desktop/R/climate/temp.rds")
