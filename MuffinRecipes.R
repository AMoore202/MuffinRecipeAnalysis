# Load packages
library(tidyverse)
library(rvest)
library(xml2)
library(magrittr)
library(stringi)

#################
### Webscraping
#################

# Function for scaping links on each page of seach results
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

# Strings of key words representing measurements
measurementKeyWords_singular <- "cup|Cup|tsp|Tsp|TSP|teaspoon|Teaspoon|tbsp|Tbsp|TBSP|tablespoon|Tablespoon|ml|ML|milliliter|Milliliter|liter|Liter|MG|milligram|Milligram|gram|Gram|oz|OZ|ounce|Ounce|qrt|QRT|dash|Dash|pinch|Pinch|pint|Pint"
measurementKeyWords_plural <- "cups|Cups|tsps|Tsps|TSPs|TSPS|teaspoons|Teaspoons|tbsps|Tbsps|TBSPs|TBSPS|tablespoons|Tablespoons|mls|MLs|MLS|milliliters|Milliliters|liters|Liters|mgs|Mgs|MGS|milligrams|Milligrams|grams|Grams|ozs|OZs|OZS|ounces|Ounces|qrts|QRTs|QRTS|dashes|Dashes|pinches|Pinches|pints|Pints"

# Function for splitting ingredients
IngredientSplitter <- function(string){
  string_cleaned <- stri_trans_general(string,"latin-ascii")
  result <- list(
    value = if(str_detect(string_cleaned,"[0-9]")){
      str_extract(string_cleaned,"[0-9/\\s]+") %>% 
        FractionConverter()
    }else{
      NA
    },
    measure = if(str_detect(string_cleaned,measurementKeyWords_plural)){
      str_extract(string_cleaned,measurementKeyWords_plural) %>% 
        str_sub(end=-2L)
    }else{
      if(str_detect(string_cleaned,measurementKeyWords_singular)){
        str_extract(string_cleaned,measurementKeyWords_singular)
      }else{
        NA
      }
    },
    ingredient = extract2(if(str_detect(string_cleaned,measurementKeyWords_plural)){
      str_split(string_cleaned,measurementKeyWords_plural)
    }else{
      if(str_detect(string_cleaned,measurementKeyWords_singular)){
        str_split(string_cleaned,measurementKeyWords_singular)
      }else{
        if(str_detect(string_cleaned,"[0-9]+/[0-9]+")){
          str_split(string_cleaned,"[0-9/\\s]+[0-9]")
        }else{
          if(str_detect(string_cleaned,"[0-9]+")){
            str_split(string_cleaned,"[0-9]+") 
          }else{
            list(c("",string_cleaned))
          }
        }
      }
    },1) %>% 
      extract(2) %>% 
      str_trim()
  )
  return(result)
}

# Function for scaping individual recipes
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

# Scape all recipes
data <- map(links,RecipeScraper)

# Save data for easy retrieval
saveRDS(data,"muffinData.rds")
data <- readRDS("muffinData.rds")

#################
### Data cleaning
#################

# Sort out non-muffin recipes
flours <- "flour|Flour|muffin mix|cake mix|Cake Mix|baking mix|Baking Mix|quinoa|dough|panettone|meal"
data_muffins <- data[which(map_lgl(data,~any(str_detect(.x$ingredients$ingredient,flours))))]

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

#################
### Analysis
#################

map(data_muffins,~.x$ingredients$ingredient) %>% 
  unlist() %>% 
  as.factor() %>% 
  summary()





