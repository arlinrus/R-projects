#Используя в блиблиотке tidyverse данные Iris, выполним следующие шаги:
#Работы выполнили Астапенкова Арина и Цибарт Алина

#1)Создайте tibble-таблицу на основе исходной.

pkgs <- c("tidyverse", "skimr")
#install.packages(pkgs)

# Загрузка пакетов
library(tidyverse)  # включает dplyr с функцией glimpse()
library(skimr)
library(naniar)
library(dplyr)

#Загрузим наши данные----------------------------------------
data("iris")
iris_tbl <- tibble(iris) 

#Посмотрим на наши данные------------------------------------
View(iris_tbl) # можем вывести все значения в таблице    
glimpse(iris_tbl) # структурный обзор
#skim(iris_tbl) %>% knitr::kable()

#Описание данных---------------------------------------------
#sepal length - длина чашелистника
#sepal width - ширина чашелистника
#petal length - длина лепестка
#petal width - ширина лепестка
#species - виды

#Посмотрим на количество пропусков в данных------------------
gg_miss_var(iris_tbl)  

#пропущенных данных нету


#2)Приведите пример фильтрации данных по одной или нескольким переменным.
#Проведем фильтрацию по виду цветка `virginica`:
df_filtered_v <- iris %>%
  filter(Sepal.Length > 6 & Sepal.Length < 7, Species == "virginica")  
head(df_filtered_v)

#Проведем фильтрацию по виду цветка `setosa`:
df_filtered_s <- iris %>%
  filter(Petal.Width == 0.4, Species == "setosa")
head(df_filtered_s)


#3)Сгруппируйте данные по одной переменной и выполните агрегацию----
#(например, найдите среднее значение, сумму или количество строк в каждой группе), используя group_by() и summarize().

df_summary <- iris %>%
  group_by(Species) %>%
  summarize(
    avg_petal_length = mean(Petal.Length),
    median_petal_length = median(Petal.Length),
    sd_petal_length = sd(Petal.Length),
    n = n() 
  )
print(paste("Среднее значение длины лепестка:", df_summary$avg_petal_length[1]))
print(paste("Медиана длины лепестка:", df_summary$median_petal_length[1]))
print(paste("Стандартное отклонение длины лепестка:", df_summary$sd_petal_length[1]))

#Таким образом: среднее значение длины лепестка: 1.462, медиана длины лепестка: 1.5, стандартное отклонение длины лепестка: 0.173663996480184.

#4)Найдите сводные статистики используя библиотеку skimr.-----------
skimmed <- skim(iris_tbl)
print(skimmed)

#5)Создайте новые таблицы с помощью функций pivot_longer() и pivot_wider(), а также across() для упрощения обращения к столбцам.
#pivot_longer---------------
long_data <- 
  iris_tbl |>
  pivot_longer(
    cols = starts_with('Sepal'),
    names_to = "measurement",
    values_to = "value"
  ) |>
  mutate(
    type = ifelse(grepl("Length", measurement), "Length", "Width"),
    value_scaled = scale(value)
  )

print(head(long_data))


#pivot_wider----------------
wide_data <- 
  long_data |>
  group_by(Species, type, measurement) |>
  summarise(value = mean(value), .groups = "drop") |>
  pivot_wider(
    names_from = "measurement",
    values_from = "value"
  )

print(head(wide_data))

#across--------------------
library(dplyr)

across_summary <- 
  iris_tbl |>
  group_by(Species) %>%
  summarize(across(
    .cols = starts_with("Sepal"),  
    .fns = list(
      mean = mean,
      sd = sd
    ),
    .names = "{.fn}_{.col}"
  )
  )

print(across_summary)







