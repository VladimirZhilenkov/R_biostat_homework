# ---
# title: T-test
# ---
# 
# 
# This file uses UTF-8 encoding.
# Reopen it if cyrillic letters are not displayed properly.
# (In RStudio: File -> Reopen with encoding)
# More information ypu could find in the presentation.
# 
# # Необходимые пакеты для работы на этой паре
# ggplot2 и dplyr
# 
# 
# #Tidy data
# 
# Как сделать данные из Mc'Donalds красивыми
library(dplyr)



# # Практика по ggplot2
library(ggplot2)
# Загрузим необходимые пакеты
library(RColorBrewer)
library(DAAG)

# # EDA
# 
# Открываем данные и делаем общий dataframe
finch_12 <- read.csv("finch_beaks_2012.csv")
finch_75 <- read.csv("finch_beaks_1975.csv")
colnames(finch_75) <- colnames(finch_12)
finch_12$year <- 2012
finch_75$year<-1975
finch_full <- rbind(finch_12, finch_75)
finch_full$year <- as.factor(finch_full$year)

# Для чего нужна базовая графика -- первый взгляд на данные
str(finch_full)
plot(finch_full)

# # Посмотрим на распределения переменных (длина клюва и глубина клюва)


# Распределение по длине клюва
p_len <- ggplot(finch_full, aes(blength)) +
  geom_density() +
  facet_grid(year~species) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme_bw()

# Распределение по глубине клюва
p_dep <- ggplot(finch_full, aes(bdepth)) +
  geom_histogram() +
  facet_grid(year~species) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme_bw()

# Склеить графики
library(cowplot)
plot_grid(p_dep, p_len)
p_len


# # Взаимосвязи переменных
# Создание нового фактора -- вид + год
finch_full$Population <- factor(paste(finch_full$species, finch_full$year, sep = "_"))

# Отрисовка графика
ggplot(finch_full, aes(blength, bdepth, color = Population)) +
  geom_point() +
  facet_grid(year~.) + 
  theme_bw()

# Данные из dplyr можно передать в ggplot

finch_full %>% mutate(ratio = blength) %>% group_by(year, species) %>% summarise(mean = mean(ratio)) %>% ggplot(aes(year, mean, color = species))+geom_point()

