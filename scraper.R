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
for (i in 1:40) {
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
# for (i in 1:length(links)) {
for (i in 1:40) {
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
}

download_exluded_products_for_diet <- function(group_of_products_to_avoid) {
  # iterate over group of products to avoid
  # retrieve specific products (or ingridients) for each group
  
  avoid_products_links <- list()
  for(prod_group in group_of_products_to_avoid) {
    avoid_products_links <- c(avoid_products_links, sprintf("https://www.tabelakalorii.net/zywnosc/%s", prod_group))
  }
  
  all_products_to_avoid <- list()
  for(link in avoid_products_links) {
    products_list_page <- GET(link, user_agent('Hi it is a student project'))
    products_list_page <- content(products_list_page)
    all_products_to_avoid <- c(all_products_to_avoid, products_list_page %>% html_nodes('tbody td.food') %>% html_text())
  }
  all_products_to_avoid <- list_to_letters_only(all_products_to_avoid)
  all_products_to_avoid <- lapply(all_products_to_avoid, tolower)
  return(all_products_to_avoid)
}

vege_avoid_list <- c("wedliny-i-mieso-obiadowe", "wieprzowina", "dziczyzna", "drob-i-ptactwo", "mieso-i-produkty-miesne", "kielbasa", "podroby")
vegan_avoid_list <- c(vege_avoid_list, "ryby-i-owoce-morza")
dairy_avoid_list <- c("mleko-i-produkty-mleczne", "ser", "ser-topiony", "ser-w-plasterkach")

vege_excluded_products <- download_exluded_products_for_diet(vege_avoid_list)
vegan_excluded_products <- download_exluded_products_for_diet(vegan_avoid_list)
dairy_excluded_products <- download_exluded_products_for_diet(dairy_avoid_list)

get_all_valid_recipes_for_diet <- function(all_recipes, diet) {
  all_valid_recipes <- data.frame(
    Name = character(),
    Ingredients = I(list()),
    Energy_kcal = character(),
    Fat_g = character(),
    Carbohydrates_g = character(),
    Fiber_g = character(),
    Protein_g = character(),
    stringsAsFactors = FALSE
  )
  for (i in 1:length(all_recipes)) {
    is_valid <- is_recipe_valid_for_given_diet(all_recipes[i, "Name"], all_recipes[i, "Ingredients"], diet, 2)
    if(is_valid) {
      all_valid_recipes <- rbind(all_valid_recipes, all_recipes[i, ])
    }
  }
  
  return(all_valid_recipes)
}

is_recipe_valid_for_given_diet <- function(recipe_name, ingredients, excluded_products, fixed_distance) {
  is_valid <- TRUE
  ingredients <- list_to_letters_only(ingredients)
  for (i in 1:length(ingredients)) {
    for (excluded in excluded_products) {
      lev_dist <- stringdist::stringdist(ingredients[i], excluded, method = "lv")
      if (lev_dist < fixed_distance) {
        print(sprintf("Recipe %s not valid because: %s, %s. Distance: %d", recipe_name, ingredients[i], excluded, lev_dist))
        is_valid <- FALSE
      }
    }
  }
  return(is_valid);
}

#get_all_valid_recipes_for_diet(recipes, vege_excluded_products)
result <- get_all_valid_recipes_for_diet(recipes, vege_excluded_products)
print(result)
