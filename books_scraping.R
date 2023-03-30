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


amazon_data <- data_all |>
  filter(map_lgl(genres, is_null))

gr_data <- data_all |>
  filter(!map_lgl(genres, is_null)) |>
  mutate(authors = as.list(authors))

amazon_data2 <- amazon_data |>
  group_by(titles) |>
  summarise(count = n())

amazon_repeats <- map_lgl(amazon_data$titles, ~`%in%`(.x, gr_data$titles))

#### manually remove duplicates
write_csv(amazon_data, "temp.csv")

amazon_data_new <- read_csv("temp.csv") |>
  mutate(authors = as.list(authors))

data_norepeats <- gr_data |>
  bind_rows(amazon_data_new) |>
  bind_rows(data_p2)

write_rds(data_norepeats, "data_all_norepeats.rda")

data_norepeats <- read_rds("data_all_norepeats.rda")

#### Defining categories

# list of categories
categories <- c("Urbanism", "BIPOC", "Women's Rights", "Disability Justice", "LGBT","Environmentalism",
                "Eduction", "Public Health", "Religion", "Arts", "Government", "International Relations")

# keywords for each category
pm_keys <- c("urbanism", "architecture", "design", "landscape")
gov_keys <- c("public policy", "public affairs", "government", "political",
              "police", "cop", "politics", "conviction", "totalitarian", "legislation")
race_keys <- c("civil rights", "segregation", "racism", "slavery", "Civil War", "plantation", "merican indian")
w_keys <- c("women’s rights", "women's liberation", "women's participation", "women's issues",
            "women's right", "gutsy women", "women's movement", "feminist", "feminism", "macho world",
            "cult of beauty", "women", "sexist", "suffragists")
dj_keys <- c("disability", "autism", "blind", "deaf", "wheelchair", "ableist", "ableism")
lgbt_keys <- c("gay", "lesbian", "queer", "transgender")
env_keys <- c("conservation", "environmentalism", "animal", "plant", "ecosystem", "ecology", "biosystem", "water crisis",
              "environmental", "hurricane", "biodiversity", "elephant")
ed_keys <- c("education", "teacher", "student", "university", "literacy")
health_keys <- c("public health", "pandemic", "disease", "health care", "doctor", "nurse", "medical")
rel_keys <- c("minister", "christian", "jesus", "religion", "church", "islam", "spirituality", "religious", "jewish", "judaism", "muslim", "hindu")
arts_keys <- c("music", "dance", "performance", "creativity", "paint")
war_keys <- c("war ")


# function to check if description contains category keywords
check_keywords_title <- function(keys, title){
  keys_in_title <- map_lgl(keys, ~str_detect(title, .x))
  if(TRUE %in% keys_in_title){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# function to check if description contains category keywords
check_keywords <- function(keys, description){
  keys_in_description <- map_lgl(keys, ~str_detect(description, .x))
  if(TRUE %in% keys_in_description){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# function to define category given description
define_category <- function(title, description){
  title <- tolower(title)
  description <- tolower(description)
  category = case_when(
    check_keywords_title(lgbt_keys, title) ~ "LGBT",
    check_keywords_title(race_keys, title) ~ "BIPOC",
    check_keywords_title(dj_keys, title) ~ "Disability Justice",
    check_keywords_title(health_keys, title) ~ "Public Health",
    check_keywords_title(env_keys, title) ~ "Environmentalism",
    check_keywords_title(w_keys, title) ~ "Women's Rights",
    check_keywords_title(gov_keys, title) ~ "Government",
    check_keywords_title(rel_keys, title) ~ "Religion",
    check_keywords_title(arts_keys, title) ~ "Arts",
    check_keywords_title(ed_keys, title) ~ "Education",
    check_keywords_title(pm_keys, title) ~ "Urbanism",
    check_keywords_title(war_keys, title) ~ "International Relations",
    check_keywords(lgbt_keys, description) ~ "LGBT",
    check_keywords(race_keys, description) ~ "BIPOC",
    check_keywords(dj_keys, description) ~ "Disability Justice",
    check_keywords(health_keys, description) ~ "Public Health",
    check_keywords(env_keys, description) ~ "Environmentalism",
    check_keywords(w_keys, description) ~ "Women's Rights",
    check_keywords(gov_keys, description) ~ "Government",
    check_keywords(rel_keys, description) ~ "Religion",
    check_keywords(arts_keys, description) ~ "Arts",
    check_keywords(ed_keys, description) ~ "Education",
    check_keywords(pm_keys, description) ~ "Urbanism",
    check_keywords(war_keys, description) ~ "International Relations",
    TRUE ~ "Uncategorized"
  )

  return(category)
}

#category <- map_chr(data_norepeats$descriptions, define_category)

data_categories <- data_norepeats |>
  mutate(category = category)

write_rds(data_categories, "data_categories.rda")

data_cat <- readRDS("data_categories.rda")

data_cat2 <- data_cat[2:165,]

category_new <- map_chr(data_cat2$descriptions, define_category)

data_cat3 <- data_cat2 |>
  mutate(category = category_new)

data_cat4 <- data_cat3 |>
  rename(title = titles, description = descriptions, link = links)

n <- data_cat3 |>
  filter(category == "Women's Rights")

write_rds(data_cat4, "data_categories.rda")

## redifine categories round 2 3/30/23

data_books <- readRDS("data_categories.rda")
category_new <- map2(data_books$title, data_books$description, ~define_category(.x, .y))

data_books2 <- data_books |>
  mutate(category = as.character(category_new))

write_rds(data_books2, "data_categories.rda")

