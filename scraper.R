library(rvest)
library(httr)

links <- list()

# first get all the URLs redirecting to the main dishes 
for (i in 1:10) {
  url <- sprintf("https://kuchnialidla.pl/przepisy/dania-glowne/%d#lista", i)
  tryCatch(
    {
      document <- GET(url, user_agent('Hi it is a student project'))
      document <- content(document)
      page_links <- html_attr(html_nodes(document, "a"), "href")
      
      start_index <- which(page_links == '/ocena') + 1
      end_index <- grep('#lista', page_links, ignore.case = TRUE)[1] - 1
      links <- c(links, page_links[start_index:end_index])
    }, 
    error = function(e) e
  )
}

links <- unique(links)
links <- paste0("https://kuchnialidla.pl", links, sep='')

# create an empty data frame to store all the recipes info
recipes <- data.frame(
  Name = character(),
  Ingredients = I(list())
  stringsAsFactors = FALSE
)

# iterate through the links 
# for (i in 1:length(links)) {
for (i in 1:5) {
  title <-  sub(".*/([^/]+)$", "\\1", links[i])
  tryCatch(
    {
      recipe <- GET(links[1], user_agent('Hi it is a student project')) %>% content()
      node <- html_element(recipe, xpath= '//*[@id="details"]/div[2]/div[1]')
      ingred <- html_elements(node, 'li') %>% html_text()
      ingred <- sub("\\s*-.*", "", ingred) 
    },
    error = function(e) e
  )
  new_row <- data.frame(
    Name = title,
    Ingredients = ingred
    stringsAsFactors = FALSE
  )
  recipes <- rbind(recipes, new_row)
}




