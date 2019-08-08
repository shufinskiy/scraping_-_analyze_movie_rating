# Загружаемые библиотеки
library(rvest)
library(selectr)
library(xml2)
library(jsonlite)
library(tidyverse)

# ПРИМЕЧАНИЕ: Кинопоиск блокирует множественные запросы, поэтому 
# запускайте данный код для не более чем 4 лет, например lapply(seq(2015, 2018)
# или используйте Sys.sleep() дляприостановки выполнения на заданное время
kinopoisk <- lapply(seq(2010, 2018), function(m){
  # Получение url
  url <- paste0("https://www.kinopoisk.ru/top/navigator/m_act[year]/", m,"/m_act[rating]/1%3A/order/rating/perpage/200/page/1/#results")
  
  # Использование функции для прочтения XML и HTML файлов
  webpage <- read_html(url)

  # Получение количества фильмов за год
  number_html <- html_nodes(webpage, ".pagesFromTo")
  number <- html_text(number_html)
  number <- number[1]
  number <- str_replace(number, ".{2,}из", "")
  number <- as.numeric(str_trim(number))
  
  # Расчёт количества страниц с фильмами
  page_number <- ceiling(number/200)
  
  # Получение адресов страниц с фильмами
  page <- sapply(seq(1:page_number), function(n){
    list_page <- paste0("https://www.kinopoisk.ru/top/navigator/m_act[year]/", m, "/m_act[rating]/1%3A/order/rating/perpage/200/page/", n, "/#results")
  })

  table <- lapply(page, function(k){
    webpage <- read_html(k)
    # Если вы хотите получить результат для более чем 4 лет,
    # используйте функцию Sys.sleep(). C указанным значением времени задержки
    # функция работает корректно. Возможно, можно указать и меньший промежуток времени.
#    Sys.sleep(3)
    
    # Получение названия фильмов
    # html_nodes - функция извлечения частей HTML-документа
    # с помощью css или XPath селекторов
    name_html <- html_nodes(webpage, ".name a")
    
    # html_text - функция извлечения текста из HTML-документа
    name <- html_text(name_html)
    
    # Замена(удаление) частей текста
    name <- str_replace(name, "\\(.{1,}\\)", "")
    # Удаление лишних пробелов
    name <- str_trim(name, side = "both")
    # Перевод всех букв в нижний регистр
    name <- tolower(name)
    name <- str_replace_all(name, "[:punct:]", "")

    # Получение рейтингов фильмов
    rating_html <- html_nodes(webpage, ".numVote")
    rat_vot <- html_text(rating_html)
    rating <- as.numeric(str_trim(str_remove(rat_vot, "\\(.{1,}\\)"), side = "both"))
    
    # str_extract - функция извлечения той части текста,
    # которая совпадает с указанным паттерном 
    votes <- as.numeric(str_replace_all(str_extract(rat_vot, "\\(.{1,}\\)"),
                                        "([:punct:])|([:space:])", ""))
    # Получение жанров фильмов
    genre_html <- html_nodes(webpage, ".gray_text")
    genre <- html_text(genre_html)
    genre <- str_replace_all(genre, "\n", "")
    
    # Получение номеров элементов (функция which()), в которых
    # есть указанный паттерн (функция str_detect())
    detect <- which(str_detect(genre, "(реж\\.)|(\\()"))
    genre <- genre[detect]
    genre <- str_extract(genre, "\\(.{1,}\\)")
    genre <- str_remove_all(genre, "(\\()|(\\))|(\\...)")

    # Объединение данных в таблицу
    df <- data.frame(NAME = name,
                     RATING = rating,
                     VOTES = votes,
                     GENRE = genre,
                     YEAR = m, stringsAsFactors = FALSE)
  })
  # Конвертация из списка в таблицу
  table <- do.call(rbind.data.frame, table)
  })

# Конвертация из списка в таблицу
kinopoisk <- do.call(rbind.data.frame, kinopoisk)

# Сохранение результатов
write.csv(imdb, "kinopoisk.csv", row.names = FALSE)
