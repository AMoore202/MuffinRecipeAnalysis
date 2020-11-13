library(tidyverse)
library(rvest)
library(xml2)

Page1 <- read_html("https://www.allrecipes.com/search/results/?wt=muffin&page=1")

Nodes <- Page1 %>% html_node("body") %>% 
  html_node(".slider-container") %>% 
  html_node(".site-content") %>% 
  html_node("#main-content") %>% 
  html_node("#searchResultsApp") %>% 
  html_node("#fixedGridSection") %>% 
  html_nodes('.fixed-recipe-card')