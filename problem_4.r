# BAN401 - Applied Programming and Data Analysis for Business

#------------------------- Solution of Problem 4

#------------ a)
routes_df <- #the created tibble is stored in object routes_df.
  tibble::tibble( #we create access to tibble library through tibble:: and use 
                  #the command tibble() in order to create a modern data frame 
                  #object.
    route = c("City A -> B", "City B -> C", "City A -> C"),
    #first column "route" contains route, it can be interpreted as unique identifier.
    distance = c(350, 500, 750),
    #second column "distance"contains the specific distances in km for each route.
    ferries = c(1, 2, 3),
    #third column "ferries" contains number of ferries for each route.
    tolls = c(3, 5, 6)
    #fourth column "tolls" contains number of toll stations for each route.
  )

vehicle_type_cost_list <-#the created list is stored in vehicle_type_cost_list
  list(# list() is a command to create a list that contains plenty possible objects 
       #which do not necessarily have to be the same data type.
    ET = c("Energy cost per km" = 0.2, 
           "Price per ferry ride" = 180,
           "Toll cost per station" = 40),
    #->ET (Electric Trucks) is defined as the first object in the list.
    #->it contains three elements. 
    DT = c("Fuel cost per km" = 1.5,
           "Price per ferry ride" = 300,
           "Toll cost per station" = 80)
    #->DT (Diesel Trucks) is defined as the second object in the list.
    #->it contains three elements.
  )

#------------ a)


et_total <- vector(mode = "double", length = 3)
dt_total <- vector(mode = "double", length = 3)
#->we create two empty vectors with data type double and a length of three in order
#  to receive a vector which can store the explicit vales we create in the upcoming 
#. for-loop. These vectors are named et_total (Total transportation costs of 
#. electric trucks) and dt_total (Total transportation costs of diesel trucks).

for(i in 1:nrow(routes_df)) {#iterate i from one to three which is the number of 
                             #rows in routes_df and therefore the number of routes.
  et_total[i] <- sum(routes_df[i,c(2,3,4)] * vehicle_type_cost_list$ET)
  #-> multiplication of the values of routes_df for each row and all numerical 
  #.  values with the corresponding elements in object ET of the truck cost list.
  #-> iterative process, that means first route is stored as first item in et_total
  #.  up until the last route.
  dt_total[i] <- sum(routes_df[i,c(2,3,4)] * vehicle_type_cost_list$DT)
  #-> multiplication of the values of routes_df for each row and all numerical 
  #.  values with the corresponding elements in object DT of the truck cost list.
  #-> iterative process, that means first route is stored as first item in dt_total
  #.  up until the last route.
}

routes_df$et_total <- et_total 
#->create new column in routes_df called et_total which stores et_total created above. 
routes_df$dt_total <- dt_total
#->create new column in routes_df called dt_total which stores dt_total created above. 
routes_df
#------------ b)
routes_df$savings <- routes_df$dt_total - routes_df$et_total
#->create new column named savings which is created by the total transportation
#. costs of diesel trucks minus the total transportation costs of electric cars for 
#. each route.
routes_df
#->final tibble










