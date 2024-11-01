# BAN401 - Applied Programming and Data Analysis for Business

# Explanation of problem:
# 
library(magrittr) # in order to use pipe-operator
library(dplyr) # in order to use efficient and time saving code structure 


#------------------------- Solution of Problem 4

#-------------------------------------------------------------------------------
# Approach 2:
#------------ a)
routes_df <- 
  tibble::tibble(
    route = c("City A -> B", "City B -> C", "City A -> C"),
    distance = c(350, 500, 750),
    ferries = c(1, 2, 3),
    tolls = c(3, 5, 6)
  )

vehicle_type_cost_list <-
  list(
    ET = c("Energy cost per km" = 0.2,
           "Price per ferry ride" = 180,
           "Toll cost per station" = 40),
    DT = c("Fuel cost per km" = 1.5,
           "Price per ferry ride" = 300,
           "Toll cost per station" = 80)
  )

# Calculation of total costs:

# (1) ET 
et_route_ab <- as.double(vehicle_type_cost_list[[1]][1] * routes_df$distance[1] + 
                            vehicle_type_cost_list[[1]][2] * routes_df$ferries[1] +
                            vehicle_type_cost_list[[1]][3] * routes_df$tolls[1])



et_route_bc <- as.double(vehicle_type_cost_list[[1]][1] * routes_df$distance[2] +
                            vehicle_type_cost_list[[1]][2] * routes_df$ferries[2] +
                            vehicle_type_cost_list[[1]][3] * routes_df$tolls[2])

et_route_ac <- as.double(vehicle_type_cost_list[[1]][1] * routes_df$distance[3] +
                            vehicle_type_cost_list[[1]][2] * routes_df$ferries[3] +
                            vehicle_type_cost_list[[1]][3] * routes_df$tolls[3])

# (2)DT

dt_route_ab <- as.double(vehicle_type_cost_list[[2]][1] * routes_df$distance[1] +
                            vehicle_type_cost_list[[2]][2] * routes_df$ferries[1] +
                            vehicle_type_cost_list[[2]][3] * routes_df$tolls[1])

dt_route_bc <- as.double(vehicle_type_cost_list[[2]][1] * routes_df$distance[2] +
                            vehicle_type_cost_list[[2]][2] * routes_df$ferries[2] +
                            vehicle_type_cost_list[[2]][3] * routes_df$tolls[2])

dt_route_ac <- as.double(vehicle_type_cost_list[[2]][1] * routes_df$distance[3] +
                            vehicle_type_cost_list[[2]][2] * routes_df$ferries[3] +
                            vehicle_type_cost_list[[2]][3] * routes_df$tolls[3])

routes_df$et_total <- c(et_route_ab, et_route_bc, et_route_ac)
routes_df$dt_total <- c(dt_route_ab, dt_route_bc, dt_route_ac)

routes_df <- routes_df %>%
  mutate(et_total = c(et_route_ab, et_route_bc, et_route_ac),
         dt_total = c(dt_route_ab, dt_route_bc, dt_route_ac))


#------------ b)
routes_df <- routes_df %>%
  mutate(savings = dt_total - et_total)









#-------------------------------------------------------------------------------
# Approach 2:
#------------ a)
vehicle_type_cost_df <- as.data.frame(vehicle_type_cost_list)

et_total <- vector(mode = "double", length = 3)
dt_total <- vector(mode = "double", length = 3)

for(i in 1:nrow(routes_df)) {
  et_total[i] <- sum(routes_df[i,c(2,3,4)] * vehicle_type_cost_df$ET)
  dt_total[i] <- sum(routes_df[i,c(2,3,4)] * vehicle_type_cost_df$DT)
}

routes_df$et_total <- et_total
routes_df$dt_total <- dt_total

#------------ b)
routes_df$savings <- routes_df$dt_total - routes_df$et_total

routes_df











