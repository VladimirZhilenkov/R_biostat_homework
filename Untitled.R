install.packages("pmlbr")
install.packages("tidyverse")  # включает dplyr, ggplot2 и др.
install.packages("gridExtra")  # для расположения графиков рядом

# Загружаем библиотеки
library(pmlbr)
library(tidyverse)
library(gridExtra)

# Загружаем датасет penguins
# Функция fetch_data загружает данные из репозитория PMLB [citation:7]
penguins_raw <- fetch_data("penguins")

# Посмотрим на структуру данных
str(penguins_raw)
head(penguins_raw)

# Приведем имена переменных к более удобному виду (удалим лишние точки)
penguins <- penguins_raw %>%
  rename_with(~ gsub("_", ".", .x))

# Узнаем названия колонок
names(penguins)

#1. Работа с пропущенными значениями: определить переменные с пропусками, создать подмножества с полными данными и только с пропущенными значениями.

sum(is.na(penguins))

#2. Фильтрация по одному условию: создать подмножество данных только для пингвинов вида Adelie, подсчитать количество наблюдений.

# Создадим факторную переменную для видов пингвинов
penguins <- penguins %>%
  mutate(species = factor(target,
                          levels = c(0, 1, 2),
                          labels = c("Adelie", "Chinstrap", "Gentoo")))

str(penguins)

nrow(penguins[penguins$species == "Adelie", ])


nrow(penguins %>% filter(species == "Adelie"))

#3. Фильтрация по нескольким условиям (И): найти всех пингвинов вида Gentoo, живущих на острове Biscoe, подсчитать их количество.


gentoo_biscoe <- penguins %>%
  filter(species == "Gentoo", island == 1)


#4. Фильтрация по нескольким условиям (ИЛИ): создать подмножество для пингвинов с длиной клюва больше 50 мм или массой тела больше 5000 г.

long_beak_or_heavy <- penguins %>%
  filter(bill.length.mm > 50 | body.mass.g > 5000)

#5. Исключение данных: создать подмножество, содержащее все наблюдения, кроме пингвинов вида Chinstrap.

not_chinstrap <- penguins %>%
  filter(species != "Chinstrap")

no_chin = penguins %>%
  filter(!species %in% c("Chinstrap"))

#6. Выбор и переименование переменных: создать новый датафрейм с переменными вида, острова, длины клюва и массы тела, переименовав две последние переменные.

penguins_selected <- penguins %>%
  select(species, island, bill.length.mm, body.mass.g) %>%
  rename(bill_length_mm = bill.length.mm, body_mass_g = body.mass.g)

#7. Сортировка данных: найти 5 самых тяжелых пингвинов, вывести их вид, массу и остров в порядке убывания массы.

penguins %>%
  arrange(desc(body.mass.g)) %>%
  select(species, body.mass.g, island) %>%
  head(5)

#8. Гистограмма распределения: построить гистограмму распределения длины клюва, используя разное количество бинов (10, 20, 30) для сравнения.

p1 <- ggplot(penguins, aes(x = bill.length.mm)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "black") +
  labs(title = "Гистограмма длины клюва (10 бинов)", x = "Длина клюва (мм)", y = "Частота")

p2 <- ggplot(penguins, aes(x = bill.length.mm)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black") +
  labs(title = "Гистограмма длины клюва (20 бинов)", x = "Длина клюва (мм)", y = "Частота")

p3 <- ggplot(penguins, aes(x = bill.length.mm)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Гистограмма длины клюва (30 бинов)", x = "Длина клюва (мм)", y = "Частота")

grid.arrange(p1, p2, p3, nrow = 1)


#9. Ящик с усами (boxplot): сравнить распределение массы тела для трех видов пингвинов, определить самый тяжелый вид и вид с наибольшим разбросом массы.

ggplot(penguins, aes(x = species, y = body.mass.g, fill = species)) +
  geom_boxplot() +
  labs(title = "Распределение массы тела по видам пингвинов", x = "Вид пингвина", y = "Масса тела (г)") +
  theme_minimal() +
  theme(legend.position = "none")

penguins %>% group_by(species) %>%
  summarise(sd_val = sd(body.mass.g))

#10. Точечная диаграмма (scatter plot): построить график зависимости длины клюва от длины ласт, раскрасив точки по видам и добавив линии регрессии для каждого вида.

ggplot(penguins, aes(x = bill.length.mm, y = flipper.length.mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Зависимость длины клюва от длины ласт", x = "Длина клюва (мм)", y = "Длина ласт (мм)") +
  theme_minimal()

#11. Комбинированный график (facet): создать точечный график зависимости массы тела от глубины клюва, разделив на три панели по видам пингвинов.

ggplot(penguins, aes(x = bill.depth.mm, y = body.mass.g, color = species)) +
  geom_point() +
  facet_wrap(~ species) +
  labs(title = "Зависимость массы тела от глубины клюва по видам", x = "Глубина клюва (мм)", y = "Масса тела (г)") +
  theme_minimal()

#12. Столбчатая диаграмма: подсчитать и визуализировать количество пингвинов каждого вида на каждом острове с расположением столбцов рядом.

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество пингвинов по видам на каждом острове", x = "Остров", y = "Количество") +
  theme_minimal() +
  theme(legend.title = element_blank())

#13. Плотность распределения: сравнить распределение длины ласт для разных видов пингвинов, используя полупрозрачные графики плотности.

ggplot(penguins, aes(x = flipper.length.mm, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Распределение длины ласт по видам пингвинов", x = "Длина ласт (мм)", y = "Плотность") +
  theme_minimal() +
  theme(legend.title = element_blank())

#14. Работа с подмножеством в графиках: отфильтровать пингвинов с длиной клюва более 45 мм и построить для них violin plot распределения массы тела по видам.

penguins %>%
  filter(bill.length.mm > 45) %>%
  ggplot(aes(x = species, y = body.mass.g, fill = species)) +
  geom_violin() +
  labs(title = "Распределение массы тела для пингвинов с длиной клюва > 45 мм", x = "Вид пингвина", y = "Масса тела (г)") +
  geom_boxplot(width = 0.1, fill = "white") +  # добавляем boxplot поверх violin plot
  theme_minimal() 

#15. Сохранение графика: создать любой график и сохранить его в формате PNG с разрешением 300 dpi и размерами 8x6 дюймов.

p <- ggplot(penguins, aes(x = bill.length.mm, y = body.mass.g, color = species)) +
  geom_point() +
  labs(title = "Зависимость массы тела от длины клюва", x = "Длина клюва (мм)", y = "Масса тела (г)") +
  theme_minimal()
ggsave("penguins_scatter.png", plot = p, width = 8, height = 6, dpi = 300)
