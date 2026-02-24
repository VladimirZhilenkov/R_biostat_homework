library(dplyr)
library(ggplot2)

lmobile <- read.csv("/Volumes/New/bio_stat/data/Large_Mobile_Invertebrates.csv")
benthic <- read.csv("/Volumes/New/bio_stat/data/Sessile_Benthic_Invertebrates.csv")
smobile <- read.csv("/Volumes/New/bio_stat/data/Small_Mobile_Invertebrates.csv")
water <- read.csv("/Volumes/New/bio_stat/data/Water_Chemistry.csv")


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






