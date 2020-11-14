# Load packages
library(tidyverse)
library(rvest)
library(xml2)
library(magrittr)

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