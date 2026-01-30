# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

friends <- friends

# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |>
  slice_head(n = 6) |>
  pull(speaker)
  
# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |>
  unnest_tokens(word, text) |>
  filter(!str_detect(word, "\\d")) |>
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  group_by(speaker) |> 
  count(word, sort = TRUE) |> 
  slice_head(n = 500) |> 
  mutate(tf = n / sum(n))

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = speaker, values_from = tf)


# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()

set.seed(123)
X <- friends_tf_wide |>
  select(where(is.numeric)) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  as.matrix()

km.out <- kmeans(
  scale(X),
  centers = 3,
  nstart = 20
)

# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(
  X,
  center = TRUE,
  scale. = TRUE
)

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q

q <- fviz_pca_biplot(
  pca_fit,
  geom.var = "text",                     
  geom.ind = "point",                    
  habillage = factor(km.out$cluster),    
  select.var = list(cos2 = 20),          
  repel = TRUE
)

q