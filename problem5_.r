# BAN401 - Applied Programming and Data Analysis for Business

#------------------------- Solution of Problem 5


items_df <-
  tibble::tibble( #we access to tibble--library trough tibble:: and extract command 
                  #tibble() in order to create a modern data frame. 
  items = c("small item", "medium item", "large item", "extra-large item",
           "luxury item", "premium item", "bulk pack"),
        #->first column contains the given items by creating a vector with character 
        #. elements.
  costs = c(1,2,5,10,20,50,100)
        #->second column contains corresponding costs of items in dollar by creating
        #. a vector of numbers with data tyoe double as default.
)


#recursive function:
count_combinations <- function(target, costs, index) {
  #base cases
  #(1) found valid combinations where the sum of item costs is exactly equal to 200:
  if (target == 0) 
    return(1)       
  #(2) found not valid combinations where the sum of item costs is higher than 
  #.   200 or there are no more items left :
  if (target < 0 | index < 1) 
    return(0)
  
  #recursive syntax: include or exclude the current item
  include_current_item <- count_combinations(target - costs[index],
                                             costs,
                                             index)
  #-> if item is included then cost of item will deduct the target spend volume
  exclude_current_item <- count_combinations(target,
                                             costs,
                                             index - 1)
  #->if item is excluded, then index will deduct in order to change the item
  
  #total combination paths is the sum of including and excluding the current item:
  return(as.numeric(include_current_item + exclude_current_item))
}


#calculate the number of combinations and store the value to the object
# total_combination_number:
total_combination_number <- count_combinations(target =200,
                                               costs = items_df$costs, 
                                               index = length(items_df$costs))

#->here we use the self-defined count_combinations()-function and provide
#. starting values to the arguments

#print the result:
cat("How many item combinations add up to $200?", "\n", "-> There are", 
    total_combination_number, "unique item purchase combinations.")







