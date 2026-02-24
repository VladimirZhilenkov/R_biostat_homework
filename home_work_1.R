library(ggplot2)


# первый график
data(mtcars)

ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl), shape = factor(cyl))) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("4" = 16, "6" = 17, "8" = 15))

#________________________________________________________________________
# второй график
data(diamonds)

ggplot(diamonds, aes(x=carat, y=price, colour = cut)) +
  geom_point()
#________________________________________________________________________
# третий график
install.packages("rio")
library(rio)
library(dplyr)
linelist <- rio::import("/Users/vl/Downloads/linelist_cleaned.rds")

linelist <- linelist %>% 
  filter(!is.na(gender))

ggplot(linelist, aes(x = outcome, fill = gender)) +
  geom_bar()
#________________________________________________________________________ 
# четвертый график
install.packages("ggExtra")
library(ggExtra)

p <- ggplot(linelist, aes(x = age, y = wt_kg, colour = gender)) +
  geom_point() +
  theme(legend.position = "bottom") # перенос легенды под график
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)

#________________________________________________________________________
# пятый график
install.packages("patchwork")
library(patchwork)

p1 <- ggplot(linelist, aes(y = hospital, fill = outcome)) +
  geom_bar() +
  labs(title = "Cases by outcome", x = "n", y = "hospital")

p2 <- ggplot(linelist, aes(y = hospital, fill = age_cat)) +
  geom_bar() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Cases by age", x = "n", y = "hospital")

p1 + p2









