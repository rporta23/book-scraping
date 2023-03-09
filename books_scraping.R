library(tidyverse)
library(rvest)

# scraping from goodreads
robotstxt::paths_allowed(
  paths = c("https://www.goodreads.com/list/show/76908.Activist_memoirs")
)

url <- "https://www.goodreads.com/list/show/76908.Activist_memoirs"
webpage <- rvest::read_html(url)

titles <- webpage |>
  rvest::html_elements(".bookTitle span") |>
  rvest::html_text()

authors <- webpage |>
  rvest::html_elements(".authorName span") |>
  rvest::html_text()

links <- webpage |>
  rvest::html_elements(".bookTitle") |>
  rvest::html_attr("href") %>%
  paste0("https://goodreads.com", .)

link1 <- links[1]

description <- link1 |>
  rvest::read_html() |>
  rvest::html_elements(".DetailsLayoutRightParagraph__widthConstrained .Formatted") |>
  rvest::html_text()

get_descriptions <- function(url){
  descriptions <- url |>
    rvest::read_html() |>
    rvest::html_elements(".DetailsLayoutRightParagraph__widthConstrained .Formatted") |>
    rvest::html_text()
  return(descriptions[1])
}

descriptions <- map_chr(links, get_descriptions)

books_data <- tibble(titles, authors, descriptions, links)

write_csv(books_data, "Desktop/Spring-2023/CSO_22-23/goodreads.csv")

bdata <- read_csv(here::here("goodreads.csv"))

url <- bdata$links[1]

get_genres <- function(url){
  genres <- url |>
    rvest::read_html() |>
    rvest::html_elements(".BookPageMetadataSection__genreButton .Button__labelItem") |>
    rvest::html_text()
  return(genres)
}

genres <- map(bdata$links, get_genres)

bdata2 <- bdata |>
  mutate(genres = (genres))


get_year <- function(url){
  year <- url |>
    rvest::read_html() |>
    rvest::html_elements("p:nth-child(2)") |>
    rvest::html_text()
  return(year)
}

# not working
year <- map(bdata$links, get_year)

write_(bdata2, "goodreads.csv")

write_rds(bdata2, "goodreads.rda")

gr <- read_rds(here::here("goodreads.rda"))
