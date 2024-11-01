# BAN401 - Applied Programming and Data Analysis for Business

# Explanation of problem:
# 


#------------------------- Solution of Problem 5


items_df <-
  tibble::tibble(
  items = c("small item", "medium item", "large item", "extra-large item",
           "luxury item", "premium item", "bulk pack"),
  costs = c(1,2,5,10,20,50,100)
)


# Target amount
target_amount <- 200

# Recursive function:
count_combinations <- function(target, costs, index) {
  # Base cases
  # (1) found valid combinations where the sum of item costs is exactly equal to 200:
  if (target == 0) 
    return(1)       
  # (2) found not valid combinations where the sum of item costs is higher than 
  #.    200 or there are no more items left :
  if (target < 0 | index < 1) 
    return(0)
  
  # Recursive calls: include or exclude the current item
  include_current_item <- count_combinations(target - costs[index], costs, index)
  exclude_current_item <- count_combinations(target, costs, index - 1)
  
  # Total ways is the sum of including and excluding the current item
  return(as.numeric(include_current_item + exclude_current_item))
}

# Calculate the number of combinations without memoization
total_combination_number <- count_combinations(target_amount, items_df$costs, length(items_df$costs))

# Print the result
cat("How many item combinations add up to $200?", "\n", "-> There are", total_combination_number, "unique item purchase combinations.")







