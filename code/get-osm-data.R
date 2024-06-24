# Calculate the average accessibility by auto and non-auto modes


library(tidyverse)
library(osmdata)
library(tigris)
library(sf)
library(here)
library(readxl)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Get list of MSAs
cbsas <- here("geography-data",
              "list1_2023.xlsx") |>
  read_excel(skip=2) |>
  select(`CBSA Code`,
         `CBSA Title`,
         `County/County Equivalent`,
         `State Name`,
         `FIPS State Code`,
         `FIPS County Code`) |>
  filter(`State Name` != "Alaska" &
           `State Name` != "Hawaii" &
           `State Name` != "Puerto Rico") |>
  group_by(`CBSA Title`) |>
  summarise(n_counties = n()) 

# Get MSA Boundaries for where we don't have network data

# Check for the ones where we already have data
# all_cbsa_folders <- cbsas$`CBSA Title` |>
#   str_replace_all(' ', '-') |>
#   str_replace_all(',', '') |>
#   str_replace_all('/', '-')
# 
# cbsas <- cbsas |>
#   mutate(folder_name = all_cbsa_folders)
# 
# have_cbsa_folders <- here("network-files") |>
#   dir()
# 
# new_cbsas <- cbsas |>
#   filter(folder_name %!in% have_cbsa_folders)

# boundary <- core_based_statistical_areas(year = 2023) |>
#   filter(NAME %in% new_cbsas$`CBSA Title`) |>
#   st_transform("WGS84") 
# 
# folders <- boundary$NAME |>
#   str_replace_all(' ', '-') |>
#   str_replace_all(',', '') |>
#   str_replace_all('/', '-')
# 
# boundary <- boundary |>
#   mutate(folder_name = folders)
######################################
# Get as many as can be gotten within a 600-second timeout
# for( i in 1:nrow(boundary)) {
#   this_xml <- here("network-files", boundary$folder_name[i], "streets.osm")
#   this_dir <- here("network-files", boundary$folder_name[i])
#   
#   dir.create(this_dir)
#   
#   if(!file.exists(this_xml)) {
#     tryCatch(
#         opq(bbox = st_bbox(boundary$geometry[i]),
#         osm_types = "way",
#         timeout = 600) |>
#       add_osm_feature(key = 'highway') |>
#       osmdata_xml(this_xml),
#       
#       error=function(e){})
#   }
#   paste0("Done with ", 
#          i, 
#          " of ", 
#          nrow(boundary),
#          " at ",
#          Sys.time(),
#          ": ",
#          boundary$NAME[i]) |> 
#     print()
# }

# # Delete directories that aren't in the list
# dont_need <- have_cbsa_folders[have_cbsa_folders %!in% all_cbsa_folders & 
#                                  have_cbsa_folders != "get_gtfs.bat"]
# 
# for ( i in 1:length(dont_need)) {
#   this_dir <- here("network-files", dont_need[i])
#   
#   unlink(this_dir, recursive = TRUE, force = TRUE)
# }

####################
have_cbsa_folders <- here("network-files") |>
  dir() |>
  sort()

boundary <- core_based_statistical_areas(year = 2023) |>
  st_transform("WGS84") 

folders_b <- boundary$NAME |>
  str_replace_all(' ', '-') |>
  str_replace_all(',', '') |>
  str_replace_all('/', '-')

boundary <- boundary |>
  mutate(folder_name = folders_b)

folders_c <- cbsas$`CBSA Title` |>
  str_replace_all(' ', '-') |>
  str_replace_all(',', '') |>
  str_replace_all('/', '-')

cbsas <- cbsas |>
  mutate(folder_name = folders_c) |>
  arrange(n_counties)

# Try again to get the ones you missed
for( i in 1:nrow(cbsas)) {
  these_dirs <- here("network-files", cbsas$folder_name[i]) |>
    dir()
  
  has_pbf <- "streets_01.pbf" %in% these_dirs
  has_osm <- "streets.osm" %in% these_dirs
  
  if(has_pbf | has_osm) {
    paste0("Already have ", cbsas$folder_name[i], " ", i, " of ", nrow(cbsas)) |>
      print()
  } else {
    paste0("Trying ", cbsas$folder_name[i],
           " at ",
           Sys.time(), " ", i, " of ", nrow(cbsas)) |>
      print()
   
    this_xml <- here("network-files", cbsas$folder_name[i], "streets.osm")
    this_dir <- here("network-files", cbsas$folder_name[i])
    
    this_bbox <- st_bbox(boundary$geometry[boundary$folder_name == cbsas$folder_name[i]])
    
    tryCatch(
      opq(bbox = st_bbox(this_bbox),
          osm_types = "way",
          timeout = 600) |>
        add_osm_feature(key = 'highway') |>
        osmdata_xml(this_xml),
      
      error=function(e){})
    
  }
}

## Quick little fix
missing_folders <- have_cbsa_folders

for( i in 1:length(missing_folders)) {
  these_dirs <- here("network-files", missing_folders[i]) |>
    dir()
  
  has_pbf <- "streets_01.pbf" %in% these_dirs
  has_osm <- "streets.osm" %in% these_dirs
  
  if(has_pbf | has_osm) {
    paste0("Already have ", missing_folders[i], " ", i, " of ", length(missing_folders)) |>
      print()
  } else {
    paste0("Trying ", missing_folders[i],
           " at ",
           Sys.time(), " ", i, " of ", length(missing_folders)) |>
      print()
    
    this_xml <- here("network-files", missing_folders[i], "streets.osm")
    this_dir <- here("network-files", missing_folders[i])
    
    this_bbox <- st_bbox(boundary$geometry[boundary$folder_name == missing_folders[i]])
    
    tryCatch(
      opq(bbox = st_bbox(this_bbox),
          osm_types = "way",
          timeout = 600) |>
        add_osm_feature(key = 'highway') |>
        osmdata_xml(this_xml),
      
      error=function(e){})
    
  }
}
