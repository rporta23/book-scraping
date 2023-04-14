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

