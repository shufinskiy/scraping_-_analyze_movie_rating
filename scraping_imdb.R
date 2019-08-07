# Загружаемые библиотеки
library(rvest)
library(selectr)
library(xml2)
library(jsonlite)
library(tidyverse)

imdb <- lapply(seq(2017,2018), function(m){
  # Получение url
  url <- paste0("https://www.imdb.com/search/title/?title_type=feature&release_date=", m, "-01-01,", m, "-12-31&num_votes=500,&sort=user_rating,desc&count=250&start=251&ref_=adv_nxt")
  
  # Использование функции для прочтения XML и HTML файлов
  webpage <- read_html(url)
  
  # Получение количества фильмов за год
  number_html <- html_nodes(webpage, ".desc")
  number <- html_text(number_html)[1]
  number <- str_extract(number, "of.{1,}titles")
  number <- as.numeric(str_trim(str_remove_all(number, "([:alpha:])|([:punct:])"), side = "both"))
  # Расчёт количества страниц с фильмами
  page_number <- ceiling(number/250) 
  
  # Получение адресов страниц с фильмами
  page <- lapply(seq(1, 1+250*(page_number-1), length.out = page_number), function(n){
    list_page <- paste0("https://www.imdb.com/search/title/?title_type=feature&release_date=", m, "-01-01,", m, "-12-31&num_votes=500,&sort=user_rating,desc&count=250&start=", n, "&ref_=adv_nxt")
  })
  
  table <- sapply(page, function(k){
    webpage <- read_html(k)
    # Получение названия фильмов
    # html_nodes - функция извлечения частей HTML-документа
    # с помощью css или XPath селекторов
    name_html <- html_nodes(webpage, ".lister-item-header a")
    
    # html_text - функция извлечения текста из HTML-документа
    name <- html_text(name_html)
    # Перевод всех букв в нижний регистр
    name <- tolower(name)
    name <- str_replace_all(name, "[:punct:]", "")
    
    # Получение рейтингов фильмов
    rating_html <- html_nodes(webpage, ".ratings-imdb-rating")
    rating <- html_text(rating_html)
    rating <- as.numeric(str_trim(str_replace_all(rating, "\n", ""), side = "both"))
    
    # Получить кол-во голосов за фильм
    votes_html <- html_nodes(webpage, ".sort-num_votes-visible")
    votes <- html_text(votes_html)
    votes <- str_replace_all(votes, "\n", "")
    votes <- str_replace_all(votes, "\\|.{1,}", "")
    votes <- as.numeric(str_trim(str_replace_all(votes, "([:alpha:])|([:punct:])", ""), side = "both"))
    
    # Получение жанров фильмов
    genre_html <- html_nodes(webpage, ".genre")
    genre <- html_text(genre_html)
    genre <- str_trim(str_remove_all(genre, "\n"), side = "both")
    
    # Объединение данных в таблицу
    df <- data.frame(NAME = name,
                     RATING = rating,
                     VOTES = votes,
                     GENRE = genre,
                     YEAR = m,
                     stringsAsFactors = FALSE)
  })
  # Конвертация из списка в таблицу
  table <- do.call(rbind.data.frame, table)
})

# Конвертация из списка в таблицу
imdb <- do.call(rbind.data.frame, imdb)

# Сохранение результатов
write.csv(imdb, "imdb_db.csv", row.names = FALSE)
