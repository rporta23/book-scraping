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

### amazon best sellers
amazon_url <- "https://www.amazon.com/Best-Sellers-Books-Social-Activist-Biographies/zgbs/books/9681289011"
robotstxt::paths_allowed(
  paths = c(amazon_url)
)

amazon_webpage <- rvest::read_html(amazon_url)

titles <- amazon_webpage |>
  rvest::html_elements(".a-link-normal ._cDEzb_p13n-sc-css-line-clamp-1_1Fn1y") |>
  rvest::html_text()

authors <- amazon_webpage |>
  rvest::html_elements(".a-size-small ._cDEzb_p13n-sc-css-line-clamp-1_1Fn1y") |>
  rvest::html_text()

# gets repeats and review links that we don't want
links <- amazon_webpage |>
  rvest::html_elements(".a-link-normal") |>
  rvest::html_attr("href") %>%
  paste0("https://www.amazon.com/", .)

# filter duplicates
amazon_links <- unique(links)

# get rid of review links
is_review_link <- map_lgl(amazon_links, ~str_detect(.x, "product-reviews"))

# final vector of links
amazon_links <- amazon_links[!is_review_link]

get_descriptions_amazon <- function(url){
  descriptions <- url |>
    rvest::read_html() |>
    rvest::html_elements(".a-expander-partial-collapse-content") |>
    rvest::html_text()
  return(str_c(descriptions, collapse = " ") |> substr(1, 1000))
}

a <- get_descriptions_amazon(amazon_links[6])

amazon_descriptions <- map(amazon_links, get_descriptions_amazon)

amazon_data <- tibble(titles, authors, descriptions = as.character(amazon_descriptions), links = amazon_links, genres = rep(NA, 30))

data_all <- bind_rows(gr, amazon_data)

write_rds(data_all, "data_all.rda")

data_all <- read_rds(here::here("data_all.rda"))

#### parnassus scraping

# links for each page
p_urls <- c("https://www.parnassusbooks.net/browse/book/BIO032000",
"https://www.parnassusbooks.net/browse/book/BIO032000?page=1",
"https://www.parnassusbooks.net/browse/book/BIO032000?page=2",
"https://www.parnassusbooks.net/browse/book/BIO032000?page=3",
"https://www.parnassusbooks.net/browse/book/BIO032000?page=4"
)

test_url <- p_urls[5]

get_links_p <- function(url) {
  links <- url |>
    rvest::read_html() |>
    rvest::html_elements(".book-title a") |>
    rvest::html_attr("href") %>%
    paste0("https://www.parnassusbooks.net/", .)

  return(links)
}

links_p <- map(p_urls, get_links_p)

## all parnassus links
links_p <- flatten_chr(links_p)

get_title_p <- function(url){
  title <- url |>
    rvest::read_html() |>
    rvest::html_elements(".page-title") |>
    rvest::html_text()
  return(title[[1]])
}

get_author_p <- function(url){
  author <- url |>
    rvest::read_html() |>
    rvest::html_elements(".author a") |>
    rvest::html_text()
  return(author)
}

get_description_p <- function(url){
  description <- url |>
    rvest::read_html() |>
    rvest::html_elements(".inner-tabs") |>
    rvest::html_text()
  return(description[[1]])
}

#test_url <- links_p[33]

#t <- get_description_p(test_url)

titles <- map(links_p, get_title_p)

authors <- map(links_p, get_author_p)

descriptions <- map(links_p, get_description_p)

data_p <- tibble(titles = as.character(titles), authors, descriptions = as.character(descriptions),
                 links = links_p, genres = rep(NA, 50)) |>
  separate(titles, into = c("titles", "discard"), sep = " \\(") |>
  select(-discard)

repeats <- map_lgl(data_p$titles, ~`%in%`(.x, data_all$titles))

data_p2 <- data_p |>
  filter(!repeats)

data_all2 <- data_all |>
  mutate(authors = as.list(authors)) |>
  bind_rows(data_p2)

write_rds(data_all2, "data_all2.rda")

## read in all data
data_all_read <- read_rds(here::here("data_all2.rda"))

