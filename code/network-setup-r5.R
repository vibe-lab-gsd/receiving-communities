options(java.parameters = '-Xmx6G')

library(r5r)
library(tidyverse)
library(here)
library(sf)

network_folders <- here("network-files") |>
  dir()

for(i in 114:length(network_folders)) {
  paste0(i, " of ", length(network_folders)) |>
    print()
  print(network_folders[i])

  this_core <- here("network-files",
                    network_folders[i]) |> 
    setup_r5()
  
  stop_r5()
  
}
