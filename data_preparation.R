library(stringdist)

# this function returns all recipes that qualifies for a user
# qualification is positive if every product of the recipe is not on restricted products list
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
  for (i in 1:nrow(all_recipes)) {
    if (i%%100 == 0) {print(sprintf("%s percent of the recipes are processed", round(100 * i/nrow(all_recipes), digits=0)))}
    is_valid <- is_recipe_valid_for_given_diet(all_recipes[i, "Name"], all_recipes[i, "Ingredients"], diet, 2)
    if(is_valid) {
      all_valid_recipes <- rbind(all_valid_recipes, all_recipes[i, ])
    }
  }
  
  return(all_valid_recipes)
}

# this function determines if given recipe is valid for provided excluded products list
is_recipe_valid_for_given_diet <- function(recipe_name, ingredients, excluded_products, fixed_distance) {
  is_valid <- TRUE
  ingredients <- list_to_letters_only(ingredients)
  for (i in 1:length(ingredients)) {
    for (excluded in excluded_products) {
      lev_dist <- stringdist::stringdist(ingredients[[i]], excluded, method = "lv")
      if (lev_dist < fixed_distance) {
        # print(sprintf("Recipe %s not valid because it contains: %s, but: %s is marked as forbidden product. Letters differance: %d", recipe_name, ingredients[i], excluded, lev_dist))
        is_valid <- FALSE
      }
    }
  }
  return(is_valid);
}

# sample of valid products for each diet - we will focus on vege diet
valid_recipes_for_vege <- get_all_valid_recipes_for_diet(recipes, vege_excluded_products)
# valid_recipes_for_vegan <- get_all_valid_recipes_for_diet(recipes, vegan_excluded_products)
# valid_recipes_for_dairyfree <- get_all_valid_recipes_for_diet(recipes, dairy_excluded_products)

# this method gets most commonly used products given your diet restrictions (excluded_products)
# number of returned products is determined by fixed_number_of_products variable
get_most_valueable_products_for_diet <- function(all_recipes, excluded_products, fixed_number_of_products) {
  all_valid_recipes <- valid_recipes_for_vege
  products <- list()
  for (i in 1:length(all_valid_recipes)) {
    ingredients <- all_valid_recipes[i, "Ingredients"]
    ingredients <- list_to_letters_only(ingredients)
    if(length(ingredients > 0)) {
      for(j in 1:length(ingredients)) {
        products <- c(products, ingredients[[j]])
      }
    }
  }
  products <- sapply(products, as.character)
  occurences <- table(products)
  print(occurences)
  sorted_occurences <- sort(occurences, decreasing = TRUE)
  top_x_products <- head(sorted_occurences, fixed_number_of_products)
  return(top_x_products)
}

# sample of getting most commonly used X products for each diet
best_products_vege <- get_most_valueable_products_for_diet(recipes, vege_excluded_products, 80)
# best_products_vegan <- get_most_valueable_products_for_diet(recipes, vegan_excluded_products, 80)
# best_products_dairy_free <- get_most_valueable_products_for_diet(recipes, dairy_excluded_products, 80)

# this method filters only those recipes that you can cook given your available products list
find_recipes_for_given_products <- function(recipes, available_products) {
  all_ok_recipe_names <- list()
  for (i in 1:nrow(recipes)) {
    recipe_possible_to_do <- TRUE
    ingredients <- recipes[i, "Ingredients"]
    ingredients <- list_to_letters_only(ingredients)
    ingredients <- sapply(list_to_letters_only(ingredients), as.character)
    for(j in 1:length(ingredients)) {
      product_names <- names(available_products)
      is_in_list <- ingredients[[j]] %in% product_names
      if(is_in_list == FALSE) {
        # print(sprintf("Recipe %s not valid because it requires: %s", recipes[i, "Name"], ingredients[[j]]))
        recipe_possible_to_do <- FALSE
      }
    }
    
    if(recipe_possible_to_do) {
      all_ok_recipe_names <- c(all_ok_recipe_names, recipes[i, "Name"])
    }
  }
  all_ok_recipe_names <-sapply(all_ok_recipe_names, as.character)
  return(all_ok_recipe_names)
}

# sample of recipes possible to cook given list of available products
possible_recipes_from_products_vege <- find_recipes_for_given_products(recipes, best_products_vege)
# possible_recipes_from_products_vegan <- find_recipes_for_given_products(recipes, best_products_vegan)
# possible_recipes_from_products_dairy_free <- find_recipes_for_given_products(recipes, best_products_dairy_free)
