# Data Acquisition
library(RSQLite)
con <- RSQLite::dbConnect(drv = SQLite(),
                          dbname = "DS_101/00_data/02_chinook/Chinook_Sqlite.sqlite")
dbListTables(con)

tbl(con, "Album")

album_tbl <- tbl(con, "Album") %>% collect()

album_tbl

x <- dbGetQuery(con,"SELECT * FROM Artist")

dbDisconnect(con)
con

# install.packages("httr")
library(httr)
GET("https://swapi.dev/api/people/?page=3")

##Glue

library(glue)
name <- "Fred"
glue("My name is {name}.")

resp <- GET("https://swapi.dev/api/people/1/")
sw_api <- function(path){
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp)
}

resp <- sw_api("/people/1")
resp

library(jsonlite)
rawToChar(resp$content)

# R Lists
data_list <- list(strings = c("string1", "string2"),
                  numbers = c(1,2,3),
                  boolean = TRUE,
                  100.23,
                  tibble(
                    A = c(1,2),
                    B = c("x", "y")
                  )
                  
                  )

# they can be accessed just like vectors

resp %>%
  .$content %>%
  rawToChar() %>%
  fromJSON() %>%
  .$name

# alternative ways of accessing contents of json files

content(resp, as = "text") # gives plain text
content(resp, as = "parsed") # gives a list format
content(resp) # this will assume you want to parse it as a list

# Getting Stock information from alphavantage
token <- "XXBARNN5T73M7FD4"
alphaResp <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))

# Securing Credentials
alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker <- "WDI.DE"
GET(alphavantage_api_url, query = list("function" = "Global_QUOTE",
    symbol = ticker,
    apikey = Sys.getenv("alphaToken")
    )
    )
# Example Getting all companies from the S&P 500
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
library(xml2)
library(rvest)
library(tibble)
sp_500 <- url %>%
  read_html() %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble()

# Example Getting all companies from the Nasdaq 100
urlNasdaq <- "https://en.wikipedia.org/wiki/NASDAQ-100"
library(xml2)
library(rvest)
library(tibble)
nasdaq_100 <- urlNasdaq %>%
  read_html() %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble()

#example Get the 250 top rated movies from IMDB
urlIMDB <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
htmlIMDB <- urlIMDB %>%
  read_html()

rankIMDB <- htmlIMDB %>%
  html_nodes(css = ".titleColumn") %>%
  html_text() #%>%
  #stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  #as.numeric()

titlesIMDB <- htmlIMDB %>%
  html_nodes(".titleColumn > a") %>%
  html_text()

yearIMDB <- htmlIMDB %>%
  html_nodes(".titleColumn  .secondaryInfo") %>%
  html_text() %>%
  stringr::str_extract(pattern = "[0-9]+") %>%
  as.numeric()

peopleIMDB <- htmlIMDB %>%
  html_nodes(".titleColumn  > a") %>%
  html_attr("title")

ratingsIMDB <- htmlIMDB %>%
  html_nodes(css = ".imdbRating > strong") %>%
  html_text() %>%
  as.numeric()

numberOfRatingsIMDB <- htmlIMDB %>%
  html_nodes(css = ".imdbRating > strong") %>%
  html_attr("title") %>%
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)") %>%
  stringr::str_replace_all(pattern = ",", replacement  = "") %>%
  as.numeric()

imdb_tbl <- tibble(rankIMDB, titlesIMDB, yearIMDB, peopleIMDB, numberOfRatingsIMDB)

# R interations
library(purrr)
numbers <- c(1:5)
number_list <- map(numbers, print)
library(jsonlite)
bike_data_lst <- fromJSON("bike_data.json")

# To access the data from this json file

bike_data_lst[["productDetail"]][["variationAttributes"]][["values"]][[1]][["value"]]
 # or

bike_data_lst %>% 
  purrr::pluck("productDetail","variationAttributes","values",1,"displayValue")

# Business Case
# Web scrape all bike models from a manufacturer to put it into a database
# with competitor product data. 
# We need a way to get all individual product URLs to scrape the contents for the database

# The URL base path is https://www.canyon.com/en-de/
# The URL for the product families are https://www.canyon.com/en-de/road-bikes/
# The URL for the product categories don't matter too much in this example, but this is the link https://www.canyon.com/en-de/road-bikes/endurance-bikes/endurace/
# We first load the relevant libraries
library(tidyverse)
library(rvest)
library(xopen)
library(jsonlite)
library(glue)
library(stringi)

# We now get URLs for each of the product categories
url_home <- "https://www.canyon.com/en-de"
xopen(url_home)
html_home <- read_html(url_home)
bike_family_tbl <- html_home %>%
  html_nodes(".js-navigationDrawer__list--secondary") %>%
  html_attr("id") %>%
  discard(.p = ~stringr::str_detect(.x, "WMN|WOMEN|GEAR|OUTLET")) %>%
  enframe(name = "position", value = "family_class") %>%
  mutate(family_id = str_glue("#{family_class}"))

# The next step is to get the bike product category urls
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")

bike_category_tbl <- html_home %>%
  html_nodes(css = family_id_css) %>%
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr("href") %>%
  
  enframe(name = "position", value = "subdirectory") %>%
  mutate(url = glue("https://www.canyon.com{subdirectory}")) %>%
  distinct(url)

# Step 2 Get the URL for each individual bike for each product category

bike_category_url <- bike_category_tbl$url[1]
xopen(bike_category_url)

html_bike_category <- read_html(bike_category_url)
bike_url_tbl <- html_bike_category %>%
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href")  %>%
  str_remove(pattern = "\\?.*") %>%
  enframe(name = "position", value = "url")

bike_description <- html_bike_category %>%
  html_nodes('.productTile__productSummaryLeft > meta[itemprop = "description"]') %>%
  html_attr("content") %>%
  enframe(name = "position", value = "description")

# Getting even more data from JSON files
library(purrr)
bike_json_tbl <- html_bike_category %>%
  html_nodes(css = ".productGrid__listItem.xlt-producttile > div") %>%
  html_attr("data-gtm-impression") %>%
  map(fromJSON) %>%
  map(purrr::pluck, 2, "impressions") %>%
  map(na_if, "not defined") %>%
  map(na_if, "") %>%
  map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
  bind_rows() %>%
  as_tibble() %>%
  rowid_to_column(var="position") %>%
  left_join(bike_description) %>%
  left_join(bike_url_tbl)

# We now get the bike data for every category

get_bike_data <- function(url){
  
  html_bike_category <- read_html(url)
  bike_url_tbl <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href")  %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  bike_description <- html_bike_category %>%
    html_nodes('.productTile__productSummaryLeft > meta[itemprop = "description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Getting even more data from JSON files
  library(purrr)
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = ".productGrid__listItem.xlt-producttile > div") %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>%
    map(purrr::pluck, 2, "impressions") %>%
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var="position") %>%
    left_join(bike_description) %>%
    left_join(bike_url_tbl)
  
}

bike_category_url <- bike_category_tbl$url[1]  
bike_data_tbl <- get_bike_data(url = bike_category_url)

# we now do a map() to run this function for all categories
bike_category_url_vec <- bike_category_tbl %>% pull(url)
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

bike_data_tbl <- bind_rows(bike_data_lst)
saveRDS(bike_data_tbl, "bike_data_tbl.rds")

# We could have done the same thing with a for loop instead
bike_data_tbl <- tibble()

for (i in seq_along(bike_category_tbl$url)){
  bike_category_url <- bike_category_tbl$url[i]
  bike_data_tbl <- bind_rows(bike_data_tbl, get_bike_data(bike_category_url))
  Sys.sleep(5)
  print(i)
}

# The next step is clean the table, there are other products listed below some categories
# These have different ID lengths and we can filter them out

bike_data_tbl %>% 
  group_by(id) %>%
  filter(n() > 1)  %>%
  arrange(id) %>%
  View()

bike_data_cleaned_tbl <- bike_data_tbl %>%
  filter(nchar(.$id) == 4) %>%
  filter(!(name %>% str_detect("Frameset"))) %>%
  distinct(id, .keep_all = T) %>%
  mutate(category = replace(category, name == "Speedmax CF SLX 8.0 SL", "Road/Triathlon Bike/Speedmax")) %>%
  separate(col = category, into = c("category_1",
                                     "category_2",
                                     "category_3"),
           sep = "(?<!\\s)/(?!\\s)") %>% 
  rename("year" = "dimension50")  %>% 
  rename("model" = "name") %>%
  rename("gender" = "dimension63") %>%
  rename("price_euro" = "metric4") %>%
  mutate(year = replace_na(year, 2021)) %>% 
  mutate(frame_material = case_when(model %>% str_detect(" CF ") ~ "carbon",
                                    model %>% str_detect(" CFR ") ~ "carbon",
                                    TRUE ~ "aluminum")) %>%
  select(-c(position, brand, variant, starts_with("dim"),
            quantity, feedProductId, price, metric5)) %>%
  select(id, model, year, frame_material, price_euro, everything())

saveRDS(bike_data_cleaned_tbl, "bike_data_cleaned_tbl.rds")

# We now want to get all the color variations for each bike
  
# 3.1a Get all color variations for each bike

# Extract all bike urls
bike_url_vec <- bike_data_cleaned_tbl %>% 
  pull(url) 

# Create function to get the variations
get_colors <- function(myurl) {
  
  myurl %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")

  }


bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl, "bike_data_colors_tbl.rds")

# We are now going to get the same data of the colors but in a faster way using multiple cores
library(furrr)
plan("multiprocess")
bike_data_colors_fast_tbl <- bike_data_cleaned_tbl %>%
  mutate(colors = future_map(bike_url_vec, get_colors))

# Now we have to build the URLs for each variant by using the IDs as query parameters

bike_data_colors_fast_tbl <- bike_data_colors_fast_tbl %>%
  unnest(colors) %>%
  mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
  select(-url) %>%
  mutate(url_color = ifelse(str_detect(colors, pattern = "/"),
                            stringi::stri_replace_last_fixed(url_color, "/", "%2F"),
                            url_color))

# We now have the URL for each color, and get the stock availability for each size and color.

get_sizes <- function(mySizeUrl) {
  json <- mySizeUrl %>%
    read_html() %>%
    html_nodes(css = "script") %>%
    as.character() %>%
    str_subset(pattern = "window.deptsfra") %>%
    str_replace("^[^\\{]+", "") %>%
    str_replace("[^\\}]+$", "") %>%
    fromJSON(flatten = T) %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 2) %>%
    select(id, value, availability.onlyXLeftNumber) %>%
    rename(id_size = id) %>%
    rename(size = value) %>%
    rename(stock_availability = availability.onlyXLeftNumber) %>%
    as_tibble()
}

bike_url_color_vec <- bike_data_colors_fast_tbl %>% pull(url_color)

bike_data_sizes_tbl <- bike_data_colors_fast_tbl %>% 
  mutate(size = future_map(bike_url_color_vec, get_sizes))

bike_data_sizes_tbl <- bike_data_sizes_tbl %>%
  unnest(size)

saveRDS(bike_data_sizes_tbl, "bike_data_sizes_tbl.rds")

# Using Seleniun framework for websites that use Javascript
library(RSelenium)
# Start the headless browser
driver <- rsDriver(browser = "firefox")
remDr  <- driver$client

# Open the url
url    <- "https://www.canyon.com/en-de/road-bikes/race-bikes/aeroad/"
remDr$navigate(url)

# Locate and click the button
button <- remDr$findElement(using = "css", ".productGrid__viewMore")
button$clickElement()

# Get the html
html <- remDr$getPageSource() %>% 
  unlist() %>% 
  read_html()
