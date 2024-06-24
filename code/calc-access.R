options(java.parameters = '-Xmx6G')

library(r5r)
library(tidyverse)
library(here)
library(sf)

network_folders <- here("network-files") |>
  dir()

point_folders <- here("block-points") |>
  dir()

n_pts <- tibble(place = "dummy",
                n_origin = 0,
                n_destination = 0) |>
  filter(place != "dummy")


for(i in 1:length(network_folders)) {
  print(i)
  print(network_folders[i])
  if(network_folders[i] != point_folders[i]) {
    stop("point folder doesn't match network folder")
  }
  blocks <- here("block-points",
                 point_folders[i],
                 "block-data.geojson") |>
    st_read()
  
  o_blocks <- blocks |>
    filter(population > 0)
  
  d_blocks <- blocks |>
    filter(n_jobs > 0)
  
  this_pts <- tibble(place = network_folders[i],
                     n_origin = nrow(o_blocks),
                     n_destination = nrow(d_blocks)) 
  
  n_pts <- rbind(n_pts, this_pts)
  
}

n_pts <- n_pts |>
  mutate(n_pairs = n_origin * n_destination) |>
  arrange(n_pairs)

n_pts <- n_pts |>
  mutate(start_time = Sys.time(),
         end_time = Sys.time(),
         already_was_done = TRUE)


for(i in 1:nrow(n_pts)) {
  print(i)
  print(n_pts$place[i])
  print(Sys.time())
  
  already_done <- "access_calc.csv" %in% dir(here("block-points",
                                              n_pts$place[i]))
  
  if(!already_done) {
    n_pts$start_time[i] <- Sys.time()
    
    blocks <- here("block-points",
                   n_pts$place[i],
                   "block-data.geojson") |>
      st_read()
    
    o_blocks <- blocks |>
      filter(population > 0)
    
    d_blocks <- blocks |>
      filter(n_jobs > 0)
    
    print(paste0("Starting calc for ",
                 n_pts$place[i],
                 ": ",
                 n_pts$n_pairs[i],
                 " O-D pairs"))
    
    this_core <- here("network-files",
                      n_pts$place[i]) |> 
      setup_r5()
    
    no_car_access <- accessibility(this_core,
                                   origins = o_blocks,
                                   destinations = d_blocks,
                                   opportunities_colnames = "n_jobs",
                                   mode = "TRANSIT",
                                   departure_datetime = 
                                     as.POSIXct("07-02-2024 14:00:00", 
                                                format = "%d-%m-%Y %H:%M:%S"),
                                   time_window = 60,
                                   decay_function = "logistic",
                                   cutoffs = 30,
                                   decay_value = 5,
                                   max_walk_time = 60,
                                   max_trip_duration = 60)
    
    car_access <- accessibility(this_core,
                                   origins = o_blocks,
                                   destinations = d_blocks,
                                   opportunities_colnames = "n_jobs",
                                   mode = "CAR",
                                   departure_datetime = 
                                     as.POSIXct("07-02-2024 14:00:00", 
                                                format = "%d-%m-%Y %H:%M:%S"),
                                   time_window = 60,
                                   decay_function = "logistic",
                                   cutoffs = 30,
                                   decay_value = 5,
                                   max_walk_time = 60,
                                   max_trip_duration = 60)
  
    stop_r5()
    
    car_access <- car_access |>
      rename(car_access = accessibility) |>
      select(id, car_access)
      
    no_car_access <- no_car_access |>
      rename(no_car_access = accessibility) |>
      select(id, no_car_access)
    
    block_access <- 
      blocks |>
      st_drop_geometry() |>
      left_join(car_access) |>
      left_join(no_car_access) |>
      filter(population > 0)
  
    write_csv(block_access,
              here("block-points",
                   n_pts$place[i],
                   "access_calc.csv"))
    
    n_pts$end_time[i] <- Sys.time()
    n_pts$already_was_done[i] <- FALSE
    
    write_csv(n_pts,
              here("access_calc_progress.csv"))
  } 
}

