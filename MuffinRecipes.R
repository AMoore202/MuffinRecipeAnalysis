# Load packages
library(tidyverse)
library(rvest)
library(xml2)
library(magrittr)
library(stringi)

#################
### Webscraping
#################

# Function for scraping links on each page of seach results
LinkExtractor <- function(nodes){
  links <- map_chr(c(1:length(nodes)),~nodes[.x] %>%
                     html_node(".grid-card-image-container") %>%
                     html_children() %>% 
                     extract(1) %>% 
                     as.character() %>% 
                     str_extract("https[:print:]+/\\\"") %>% 
                     str_sub(end=-2L)
  )
  return(links)
}

# Scape links from first 50 pages of muffins
links <- map(c(1:50),~read_html(paste0("https://www.allrecipes.com/search/results/?wt=muffin&page=",.x)) %>%
  html_node("body") %>% 
  html_node(".slider-container") %>% 
  html_node(".site-content") %>% 
  html_node("#main-content") %>% 
  html_node("#searchResultsApp") %>% 
  html_node("#fixedGridSection") %>% 
  html_nodes('.fixed-recipe-card') %>% 
  LinkExtractor()
) %>% unlist()

# Function for converting ingredients with fractions into numerics
FractionConverter <- function(string){
  if(str_detect(string,"[0-9]+[\\s]+[0-9]+/")){
    whole <- string %>% 
      str_split("\\s") %>% 
      extract2(1) %>% 
      extract(1) %>% 
      as.character() %>% 
      as.numeric()
    fraction <- string %>% 
      str_trim() %>% 
      str_split("\\s") %>% 
      extract2(1) %>% 
      extract(str_split(str_trim(string),"\\s")[[1]] %>% length()) %>% 
      str_split("/") %>% 
      extract2(1)
    decimal <- as.numeric(as.character(fraction[1])) / as.numeric(as.character(fraction[2]))
    return(whole+decimal)
  }else{
    if(str_detect(string,"/")){
      fraction <- string %>% 
        str_trim() %>% 
        str_split("/") %>% 
        extract2(1)
      decimal <- as.numeric(as.character(fraction[1])) / as.numeric(as.character(fraction[2]))
      return(decimal)
    }else{
      return(
        string %>% 
          as.character() %>% 
          as.numeric()
      ) 
    }
  }
}

# Strings of key words representing measurements and preparations
measurementKeyWords_singular <- "cup|Cup|tsp|Tsp|TSP|teaspoon|Teaspoon|tbsp|Tbsp|TBSP|tablespoon|Tablespoon|ml|ML|milliliter|Milliliter|liter|Liter|MG|milligram|Milligram|gram|Gram|\\soz|OZ|ounce|Ounce|qrt|QRT|dash|Dash|pinch|Pinch|pint|Pint|pound|Pound"
measurementKeyWords_plural <- "cups|Cups|tsps|Tsps|TSPs|TSPS|teaspoons|Teaspoons|tbsps|Tbsps|TBSPs|TBSPS|tablespoons|Tablespoons|mls|MLs|MLS|milliliters|Milliliters|liters|Liters|mgs|Mgs|MGS|milligrams|Milligrams|grams|Grams|ozs|OZs|OZS|ounces|Ounces|qrts|QRTs|QRTS|dashes|Dashes|pinches|Pinches|pints|Pints|pounds|Pounds"
preparationKeyWords <- "^sliced|^diced|^cubed|^chopped|^finely chopped|^minced|^divided|^melted|^softened|^warmed|^mashed|^ground|^finely ground|^freshly ground|^packed|^grated|^freshly grated|^zested|^lightly packed|^firmly packed|^shredded|^crumbled"

# Function for splitting ingredients
IngredientSplitter <- function(string){
  string_cleaned <- stri_trans_general(string,"latin-ascii") %>% 
    str_remove("\\(Optional\\)") %>% 
    str_trim()
  ingredient_raw <- case_when(
    str_detect(string_cleaned,"\\([0-9\\.]+\\s[A-Za-z]+\\)") ~ str_split(string_cleaned,"\\([0-9\\.]+\\s[A-Za-z]+\\)[\\s]+[A-Za-z]+"),
    str_detect(string_cleaned,measurementKeyWords_plural) ~ str_split(string_cleaned,measurementKeyWords_plural),
    str_detect(string_cleaned,measurementKeyWords_singular) ~ str_split(string_cleaned,measurementKeyWords_singular),
    str_detect(string_cleaned,"[0-9]+/[0-9]+") ~ str_split(string_cleaned,"[0-9/\\s]+[0-9]"),
    str_detect(string_cleaned,"[0-9]+") ~ str_split(string_cleaned,"[0-9]+"),
    TRUE ~ list(c("",string_cleaned))
  ) %>% 
    extract2(1) %>% 
    extract(2) %>% 
    str_trim()
  result <- list(
    value = if(str_detect(string_cleaned,"[0-9]")){
      str_extract(string_cleaned,"[0-9/\\s]+") %>% 
        FractionConverter()
    }else{
      NA
    },
    measure = case_when(
      str_detect(string_cleaned,"\\([0-9\\.]+\\s[A-Za-z]+\\)") ~ str_extract(string_cleaned,"\\([0-9\\.]+\\s[A-Za-z]+\\)\\s[A-Za-z]+"),
      str_detect(string_cleaned,measurementKeyWords_plural) ~ str_extract(string_cleaned,measurementKeyWords_plural) %>% str_sub(end=-2L),
      str_detect(string_cleaned,measurementKeyWords_singular) ~ str_extract(string_cleaned,measurementKeyWords_singular),
      TRUE ~ NA_character_
    ),
    ingredient = case_when(
      str_detect(ingredient_raw,",|\\s\\-\\s") ~ str_split_fixed(ingredient_raw,",|\\s\\-\\s",2) %>% extract(1) %>% str_trim(),
      str_detect(ingredient_raw,preparationKeyWords) ~ str_split_fixed(ingredient_raw,preparationKeyWords,2) %>% extract(2) %>% str_trim(),
      TRUE ~ ingredient_raw
    ),
    preparation = case_when(
      str_detect(ingredient_raw,",|\\s\\-\\s") ~ str_split_fixed(ingredient_raw,",|\\s\\-\\s",2) %>% extract(2) %>% str_trim(),
      str_detect(ingredient_raw,preparationKeyWords) ~ str_extract(ingredient_raw,preparationKeyWords),
      TRUE ~ NA_character_
    )
  )
  return(result)
}

# Function for scraping individual recipes
RecipeScraper <- function(url){
  webpage <- read_html(url) %>% 
    html_node("body") %>% 
    html_node(".docked-sharebar-content-container") %>% 
    html_node("main") %>% 
    html_node(".recipe-container") %>% 
    html_children() %>% 
    extract(2) %>% 
    html_node(".recipe-content")
  data <- list(
    title = webpage %>% 
      html_node(".main-header") %>% 
      html_node(".intro") %>% 
      html_children() %>% 
      html_node("h1") %>% 
      html_text(),
    ratings = webpage %>% 
      html_node(".main-header") %>% 
      html_node(".recipe-review-container") %>% 
      html_node(".component") %>% 
      html_node(".ratings-dropdown-menu") %>% 
      html_node(".recipe-ratings-list") %>% 
      html_node("ul") %>% 
      html_nodes("li") %>% 
      map_dbl(.,~ html_node(.x,".rating-count") %>% 
                html_text() %>% 
                str_extract("[0-9]+") %>% 
                as.character() %>% 
                as.numeric()
              ) %>% {
                tibble(
                  rank = sort(c(1:5),decreasing = TRUE),
                  votes = .
                )
              },
    servingSize = webpage %>% 
      html_node(".two-col-content-wrapper") %>% 
      html_node(".recipe-content-container") %>% 
      html_node(".recipe-shopper-wrapper") %>% 
      html_node("section") %>% 
      html_node(".section-headline") %>% 
      html_children() %>% 
      extract(3) %>% 
      html_text() %>% 
      str_extract("[0-9]+") %>% 
      as.character() %>% 
      as.numeric(),
    ingredients = webpage %>% 
      html_node(".two-col-content-wrapper") %>% 
      html_node(".recipe-content-container") %>% 
      html_node(".recipe-shopper-wrapper") %>%
      html_node("section") %>% 
      html_node("fieldset") %>% 
      html_node("ul") %>% 
      html_nodes("li") %>% 
      map_df(.,~ html_node(.x,"label") %>% 
                html_node("span") %>% 
                html_node("span") %>% 
                html_text() %>% 
                str_remove_all("\n") %>% 
                str_trim() %>% 
                IngredientSplitter()
        ),
    instructions = webpage %>% 
      html_node(".two-col-content-wrapper") %>% 
      html_node(".recipe-content-container") %>% 
      html_node(".recipe-instructions") %>% 
      html_node("fieldset") %>% 
      html_node("ul") %>% 
      html_nodes("li") %>% 
      map_chr(.,~ html_node(.x,".section-body") %>% 
                html_node(".paragraph") %>% 
                html_node("p") %>% 
                html_text()
        )
      )
  return(data)
}

# Scrape all recipes
data <- map(links,RecipeScraper)

# Save data for easy retrieval
saveRDS(data,"muffinData.rds")
data <- readRDS("muffinData.rds")

#################
### Data cleaning
#################

# Sort out non-muffin recipes and English muffin recipes
flours <- "flour|Flour|muffin mix|cake mix|Cake Mix|baking mix|Baking Mix|quinoa|dough|panettone|meal"
data_muffins <- data[map_lgl(data,~any(str_detect(.x$ingredients$ingredient,flours)))] %>% length()
data_muffins <- data[map_lgl(data_muffins,~!str_detect(.x$title,"[:print:]+[Ee]nglish[:print:]+|[Ee]nglish[:print:]+|[:print:]+[Ee]nglish"))]

tibble(title = map_chr(data_muffins,"title"),check = map_lgl(data_muffins,~!str_detect(.x$title,"[:print:]+[Ee]nglish[:print:]+|[Ee]nglish[:print:]+|[:print:]+[Ee]nglish"))) %>% View()

for (i in c(1:length(data_muffins))){
  # Scale all ingredients to 12 muffins
  data_muffins[[i]]$ingredients$value12 <- 
    data_muffins[[i]]$ingredients$value * (12 / data_muffins[[i]]$servingSize)
  
  # Scale ingredients (as best possible) to teaspoons
  data_muffins[[i]]$ingredients$value12Tsp <-
    map_dbl(c(1:nrow(data_muffins[[i]]$ingredients)),~
              case_when(
                data_muffins[[i]]$ingredients$measure[.x] == "cup" ~ data_muffins[[i]]$ingredients$value12[.x] * 49,
                data_muffins[[i]]$ingredients$measure[.x] == "tablespoon" ~ data_muffins[[i]]$ingredients$value12[.x] * 3,
                data_muffins[[i]]$ingredients$measure[.x] == "teaspoon" ~ data_muffins[[i]]$ingredients$value12[.x],
                data_muffins[[i]]$ingredients$measure[.x] %>% is.character() ~ data_muffins[[i]]$ingredients$value12[.x]
                )
              )
  
  # Create a single rating score
  data_muffins[[i]]$score <- list(
    rating = sum(
      map_dbl(c(1:5),~ data_muffins[[i]]$ratings$rank[.x] * data_muffins[[i]]$ratings$votes[.x])
    ) / sum(data_muffins[[i]]$ratings$votes),
    # Create a value of number of votes
    votes = sum(data_muffins[[i]]$ratings$votes)
  )
}

# Create vector of ingredients
ingredientsVector <- map(data_muffins,~ .x$ingredients$ingredient) %>% 
  unlist() %>% 
  na.omit()

# Create index table to determine whether the plural or singular form of an ingredients
# should be used for standardization
ingredientPlurIndexTable <- ingredientsVector[str_detect(ingredientsVector,"s$")] %>% 
  unique() %>% {
    tibble(
      plural = .,
      singular = str_sub(.,end = -2L)
    )
  } %>% {
    mutate(.,
      toCheck = map_chr(.$plural, ~
                      if_else(
                        length(ingredientsVector[ingredientsVector == .x]) > length(ingredientsVector[ingredientsVector == str_sub(.x, end = -2L)]),
                        str_sub(.x, end = -2L),
                        .x
                      )
                      ) %>% 
        {str_c("^", ., "$")}
        ,
      changeTo = map_chr(.$plural, ~
                        if_else(
                          length(ingredientsVector[ingredientsVector == .x]) > length(ingredientsVector[ingredientsVector == str_sub(.x, end = -2L)]),
                          .x,
                          str_sub(.x, end = -2L)
                        )
      )
    )
  }

# Function for changing singular to plural and vice-versa
PlurChanger <- function(ingredient){
  if(str_detect(ingredient,ingredientPlurIndexTable$toCheck) %>% any()){
    return(ingredientPlurIndexTable[str_detect(ingredient,ingredientPlurIndexTable$toCheck),4] %>% extract2(1) %>% extract(1))
  }else{
    return(ingredient)
  }
}

# standardize ingredient names
for (i in c(1:length(data_muffins))){
  data_muffins[[i]]$ingredients$ingredient <- map_chr(data_muffins[[i]]$ingredients$ingredient,PlurChanger)
}

# Create flat ingredient tables for Tableau practice
ingredientsTable <- map(data_muffins,~ .x$ingredients %>% 
      mutate(title = .x$title)
    ) %>% 
  bind_rows()
write_csv(ingredientsTable,"ingredientsTable.csv")

ratingsTable <- map_df(data_muffins,"score") %>% 
          mutate(title = map_chr(data_muffins,"title"))
write_csv(ratingsTable,"ratingsTable.csv")

#################
### Analysis
#################

# See the most common ingredients
map(data_muffins,~ .x$ingredients$ingredient) %>% 
  unlist() %>% 
  as.factor() %>% 
  summary()

tibble(
  ingredient = map(data_muffins,~ .x$ingredients$ingredient)
)
  

map(data_muffins,~ .x$instructions %>% str_extract("[0-9]+[\\s]+degrees[\\s]+F") %>% na.omit()) %>% 
  map_dbl(length)

map(data_muffins,~ .x$instructions %>% str_extract("[0-9]+[\\s]+degre[es]+") %>% na.omit()) %>% 
  map_dbl(length) %>% as.factor() %>% summary()

map(data_muffins,~ .x$instructions %>% str_extract("[0-9]+[\\s]+degre[es]+") %>% na.omit()) %>% 
  map_dbl(length) %>% {tibble(title = map_chr(data_muffins,"title"),instances = .)} %>% View()

