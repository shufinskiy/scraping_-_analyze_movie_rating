# Загружаемые библиотеки
library(data.table)
library(tidyverse)
library(reshape)
library(ggthemes)

# Загрузка данных
kp <- fread("F:/dataset/movie/kinopoisk.csv")
imdb <- fread("F:/dataset/movie/imdb_db.csv")

# Удаление дубликатов
table <- imdb %>%
  inner_join(kp, by = c("NAME", "YEAR"))

table2 <- table[which(duplicated(table$NAME)),]

table <- table %>%
  anti_join(table2)

# Удаление фильмов "Одержимость" и "Кухня"
table <- table[-с(1196,1704),]

# Переименование и удаление столбцов
table <- table %>%
  rename_at(vars(ends_with(".x")), list(~str_c(str_extract(., "[A-Z]{1,}"), "_IMDB"))) %>%
  rename_at(vars(ends_with(".y")), list(~str_c(str_extract(., "[A-Z]{1,}"), "_KP")))

# Округление рейтинга Кинопоиска до десятых и расчёт разницы в оценках
table <- table %>%
  mutate(RATING_KP = round(RATING_KP, digits = 1)) %>%
  mutate(DELTA = RATING_IMDB - RATING_KP)

mean(table$DELTA)

# Гистограмма разницы оценок
gg <- ggplot(table, aes(x = DELTA)) + 
  geom_histogram(binwidth = 0.1,color = "black", fill = "gray60") +
  labs(title = "Гистограмма разницы в оценках фильмов между Кинопоиском и IMDB",
       caption = "Источник: kinopoisk.ru\nimdb.com") +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6))
ggsave("hist_movie.jpeg", gg, width = 4.5, height = 5.5, units = "in")

# t-test для оценок
t_table <- table %>%
  select(RATING_IMDB, RATING_KP) %>%
  melt(., variable_name = "site") %>%
  mutate(site = as.factor(site))

t.test(t_table$value ~ t_table$site)

# Зависимость разницы в оценках от кол-ва голосов
# Расчёт коэффициента корреляции
cor.test(table$DELTA, table$VOTES_KP)

gg1 <- ggplot(table, aes(x = VOTES_KP, y = DELTA)) +
  geom_point() +
  labs(title = "Диаграмма рассеяния разницы в оценках\n в зависимости от числа голосов на Кинопоиске",
       caption = "Источник: kinopoisk.ru\nimdb.com") +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6))
ggsave("votes_movie.jpeg", gg1, width = 4.5, height = 5.5, units = "in")

# Сравнение оценок по жанрам фильма для жанров на Кинопоиске
# Удаление столбца GENRE_IMDB
table_genre <- table[,-4]
# Разделение столбца GENRE на три
table_genre <- table_genre %>%
  separate(GENRE_KP, sep = ",", into = c("GENRE_1", "GENRE_2", "GENRE_3"))

table_genre <- setDT(table_genre)

# Преобразование данных в long таблицу
table_genre <- data.table::melt(table_genre, measure = patterns("^GENRE"),
                                value.name = "GENRE")

# Удаление строк с пропущенными значениями GENRE,
# подсчёт среднего рейтинга и количества упоминаний
table_genre <- table_genre %>%
  mutate(GENRE = str_trim(GENRE, side = "both")) %>%
  na.omit(.) %>%
  group_by(GENRE) %>%
  transmute(RATING = round(mean(RATING_KP), digits =1),
            COUNT = n()) %>%
  ungroup()  %>%
  unique() %>%
  filter(COUNT > 150) %>%
  mutate(GENRE = as.factor(GENRE))

# График срдней оценки Кинопоиска по жанрам фильмов
gg2 <- ggplot(table_genre, aes(x = RATING, y = GENRE, size = COUNT, colour = COUNT)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(title = "Средняя оценка фильмов по жанрам на Кинопоиске",
       caption = "Источник: kinopoisk.ru") +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        legend.key.size = unit(0.25, "cm"),
        panel.grid = element_line(size = 0.3, colour = "grey60", linetype = "dotted"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        axis.ticks = element_blank())
ggsave("genre_movie_kp.jpeg", gg2, width = 4.5, height = 5.5, units = "in")

# Сравнение оценок по жанрам фильма для жанров на IMDB
# Удаление столбца GENRE_KP
table_genre1 <- table[,-8]
# Разделение столбца GENRE на три
table_genre1 <- table_genre1 %>%
  separate(GENRE_IMDB, sep = ",", into = c("GENRE_1", "GENRE_2", "GENRE_3"))

table_genre1 <- setDT(table_genre1)

# Преобразование данных в long таблицу
table_genre1 <- data.table::melt(table_genre1, measure = patterns("^GENRE"),
                                value.name = "GENRE")

# Удаление строк с пропущенными значениями GENRE,
# подсчёт среднего рейтинга и количества упоминаний
table_genre1 <- table_genre1 %>%
  mutate(GENRE = str_trim(GENRE, side = "both")) %>%
  na.omit(.) %>%
  group_by(GENRE) %>%
  transmute(RATING = round(mean(RATING_IMDB), digits =1),
            COUNT = n()) %>%
  ungroup()  %>%
  unique() %>%
  filter(COUNT > 150) %>%
  mutate(GENRE = as.factor(GENRE))

# График средней оценки Кинопоиска по жанрам фильмов
gg2 <- ggplot(table_genre1, aes(x = RATING, y = GENRE, size = COUNT, colour = COUNT)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(title = "Средняя оценка фильмов по жанрам на IMDB",
       caption = "Источник: imdb.com") +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        legend.key.size = unit(0.25, "cm"),
        panel.grid = element_line(size = 0.3, colour = "grey60", linetype = "dotted"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        axis.ticks = element_blank())
ggsave("genre_movie_imdb.jpeg", gg2, width = 4.5, height = 5.5, units = "in")
