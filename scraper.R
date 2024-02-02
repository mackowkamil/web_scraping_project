library(rvest)
library(httr)
library(stringdist)

to_letters_only <- function(input_string) {
  modified_string <- gsub("[^a-zA-ZąćęłńóśźżĄĆĘŁŃÓŚŹŻ ]", "", input_string)
  return(modified_string)
}

list_to_letters_only <- function(list) {
  new_list <- list()
  for (elem in list) {
    new_list <- c(new_list, to_letters_only(elem))
  }
  return(new_list)
}

links <- list()

# first get all the URLs redirecting to the main dishes 
for (i in 1:118) {
  if (i%%20 == 0) {print(sprintf("%s percent of the links are scraped", round(100 * i/118, digits=0)))}
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
  Ingredients = I(list()),
  Energy_kcal = character(),
  Fat_g = character(),
  Carbohydrates_g = character(),
  Fiber_g = character(),
  Protein_g = character(),
  stringsAsFactors = FALSE
)

# iterate through the links 
for (i in 1:length(links)) {
  if (i%%100 == 0) {print(sprintf("%s percent of the recipes are scraped", round(100 * i/length(links), digits=0)))}
  tryCatch(
    {
      title <-  sub(".*/([^/]+)$", "\\1", links[i])
      recipe <- GET(links[i], user_agent('Hi it is a student project')) %>% content()
      
      # get ingredients 
      node <- html_element(recipe, xpath= '//*[@id="details"]/div[2]/div[1]')
      ingred <- html_elements(node, 'li') %>% html_text()
      ingred <- sub("\\s*-.*", "", ingred)
      ingred_list <- lapply(list(ingred), tolower)
      
      # get dietary information
      table <- html_element(recipe, xpath= '//*[@id="details"]/div[2]/div[2]/table') %>% html_table()
      macro <- table[,2,2]
      macro <- sub("\\s*[a-zA-Z]+$", "", macro[-1])
      
      # add new recipe to the data frame    
      new_row <- data.frame(
        Name = title,
        Ingredients = I(ingred_list),
        Energy_kcal = macro[1],
        Fat_g = macro[2],
        Carbohydrates_g = macro[3],
        Fiber_g = macro[4],
        Protein_g = macro[5],
        stringsAsFactors = FALSE
      )
      recipes <- rbind(recipes, new_row)
    }, 
    error = function(e) e
  )
}

# get rid of the rows with empty "Ingredients"
recipes <- recipes[sapply(recipes$Ingredients, function(x) length(x) > 0),]

# this function returns all excluded products for given diet restriction
download_exluded_products_for_diet <- function(group_of_products_to_avoid) {
  # iterate over group of products to avoid
  # retrieve specific products (or ingredients) for each group
  
  avoid_products_links <- list()
  for(prod_group in group_of_products_to_avoid) {
    avoid_products_links <- c(avoid_products_links, sprintf("https://www.tabelakalorii.net/zywnosc/%s", prod_group))
  }
  
  all_products_to_avoid <- list()
  for(link in avoid_products_links) {
    tryCatch(
      {
        products_list_page <- GET(link, user_agent('Hi it is a student project'))
        products_list_page <- content(products_list_page, encoding = "UTF-8")
        all_products_to_avoid <- c(all_products_to_avoid, products_list_page %>% html_nodes('tbody td.food') %>% html_text())
      }, 
      error = function(e) e
    )
  }
  all_products_to_avoid <- list_to_letters_only(all_products_to_avoid)
  all_products_to_avoid <- lapply(all_products_to_avoid, tolower)
  return(all_products_to_avoid)
}

# create some sample diet restrictions
vege_avoid_list <- c("wedliny-i-mieso-obiadowe", "wieprzowina", "dziczyzna", "drob-i-ptactwo", "mieso-i-produkty-miesne", 
                     "kielbasa", "podroby", "ryby-i-owoce-morza", "wolowina-i-cielecina")
dairy_avoid_list <- c("mleko-i-produkty-mleczne", "ser", "ser-topiony", "ser-w-plasterkach", "jogurt")
vegan_avoid_list <- c(vege_avoid_list, dairy_avoid_list)

# extract products for given diet restrictions
vege_excluded_products <- download_exluded_products_for_diet(vege_avoid_list)
vegan_excluded_products <- download_exluded_products_for_diet(vegan_avoid_list)
dairy_excluded_products <- download_exluded_products_for_diet(dairy_avoid_list)

# add some products to avoid that were not present in the tabelakalorii.net
vege_excluded_products <- c(vege_excluded_products, "mięso mielone wołowe", "mięso mielone wieprzowe") 
vegan_excluded_products <- c(vegan_excluded_products, "mięso mielone wołowe", "mięso mielone wieprzowe")
dairy_excluded_products <- c(dairy_excluded_products, "śmietanka kremówka 30%", "parmezan tarty", "twaróg z czosnkiem i koprem",
                             "twaróg z czosnkiem i koprem", "śmietana kwaśna", "śmietana 30%", "ser feta", "parmiggiano reggiano",
                             "ser górski", "ser ricotta", "ser pecorino", "śmietana 18%", "ser żółty", "burrata", "mozzarella, tarta",
                             "ser twaróg tłusty", "jogurt naturalny", "ser la brique", "skyr naturalny", "ser gorgonzola", "ser cheddar",
                             "sos tzatziki", "serek grani", "grana padano", "ser graviera, tarty", "śmietana 12%", "ser scamorza", "śmietana 22%",
                             "ser mascarpone", "ser żółty w plastrach", "ser halloumi", "ser gruyere", "ser maślany, plastry", "cremé fraîche",
                             "śmietana 36%", "ser brie", "ser pont", "ser chaource","ser kolumb", "ser twaróg półtłusty")

# write data to csv files
write.table(recipes, "output/recipes.csv", sep = ";", row.names = FALSE)
write.csv(unlist(dairy_excluded_products), "output/dairy_excluded_products.csv", row.names = FALSE)
