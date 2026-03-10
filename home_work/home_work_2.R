library(dplyr)
library(ggplot2)

lmobile <- read.csv("/Volumes/New/учеба/bio_stat/data/Large_Mobile_Invertebrates.csv")
benthic <- read.csv("/Volumes/New/учеба/bio_stat/data/Sessile_Benthic_Invertebrates.csv")
smobile <- read.csv("/Volumes/New/учеба/bio_stat/data/Small_Mobile_Invertebrates.csv")
water <- read.csv("/Volumes/New/учеба/bio_stat/data/Water_Chemistry.csv")


lmobile <- lmobile %>% arrange(Location_m, Side)
benthic  <- benthic  %>% arrange(Location_m, Side)
smobile  <- smobile  %>% arrange(Location_m, Side)


all(lmobile$Side == benthic$Side)

combined <- cbind(
  lmobile,
  benthic %>% select(-Location_m, -Side),
  smobile %>% select(-Location_m, -Side)
)

combined[is.na(combined)] <- 0

summary(water)

filtered_water <- water %>%
  filter(Salinty > mean(Salinty, na.rm = TRUE),
         pH > mean(pH, na.rm = TRUE))


summary(combined$Acorn.Barnacles)
summary(combined$Limpets)



ggplot(combined %>% filter(Acorn.Barnacles > 0, Limpets > 0), 
       aes(x = Acorn.Barnacles, y = Limpets)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Взаимосвязь балянусов и блюдечек",
    subtitle = "Только ненулевые наблюдения (log scale)",
    x = "Балянусы (Acorn Barnacles)",
    y = "Блюдечки (Limpets)"
  ) +
  theme_bw()



combined %>%
  select(-Location_m, -Side) %>%
  colSums() %>%
  sort(decreasing = TRUE)


ggplot(combined, aes(x = Location_m, y = Acorn.Barnacles)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  scale_y_log10() +
  labs(
    title = "Распределение балянусов по локации",
    subtitle = "Самый обильный вид (n = 52074)",
    x = "Локация (м)",
    y = "Количество балянусов (log scale)"
  ) +
  theme_bw()


# Фильтруем только две крайние точки
water_filtered <- water %>%
  filter(Location_m %in% c(0.3, 46.1)) %>%
  filter(!is.na(Temperature_C)) %>%
  mutate(Location_m = factor(Location_m, labels = c("0.3 м (поверхность)", "46.1 м (глубина)")))

ggplot(water_filtered, aes(x = Location_m, y = Temperature_C, fill = Location_m)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(
    title = "Сравнение температуры на разных глубинах",
    x = "Глубина",
    y = "Температура (°C)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

#Ожидаемый вывод: на глубине 46 м температура должна быть ниже и стабильнее (меньше разброс), чем на поверхности 0.3 м где температура сильнее меняется под влиянием солнца и погоды.

