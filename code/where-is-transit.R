# Which MSAs what fixed-route transit service?

library(tidyverse)
library(tigris)
library(sf)
library(here)
library(rjson)
library(readxl)

'%!in%' <- function(x,y)!('%in%'(x,y))

sf_use_s2(FALSE)

#### Read GTFS feed info
# From https://mobilitydatabase.org/

feed_catalog_url <- "https://bit.ly/catalogs-csv"

############## Get list of data sources for all GTFS in United States
feed_urls <- feed_catalog_url |>
  read_csv() |>
  filter(location.country_code == "US",
         !is.na(location.bounding_box.minimum_longitude),
         data_type == "gtfs") 

############ Get sf dataframe of bounding boxes for each gtfs feed.
box_coords_1 = matrix(c(feed_urls$location.bounding_box.minimum_longitude[1],
                      feed_urls$location.bounding_box.minimum_latitude[1],
                      feed_urls$location.bounding_box.minimum_longitude[1],
                      feed_urls$location.bounding_box.maximum_latitude[1],
                      feed_urls$location.bounding_box.maximum_longitude[1],
                      feed_urls$location.bounding_box.maximum_latitude[1],
                      feed_urls$location.bounding_box.maximum_longitude[1],
                      feed_urls$location.bounding_box.minimum_latitude[1],
                      feed_urls$location.bounding_box.minimum_longitude[1],
                      feed_urls$location.bounding_box.minimum_latitude[1]),
                    ncol=2, byrow=TRUE)
  
box_poly_1 <- st_polygon(list(box_coords_1))

box_coords_2 = matrix(c(feed_urls$location.bounding_box.minimum_longitude[2],
                        feed_urls$location.bounding_box.minimum_latitude[2],
                        feed_urls$location.bounding_box.minimum_longitude[2],
                        feed_urls$location.bounding_box.maximum_latitude[2],
                        feed_urls$location.bounding_box.maximum_longitude[2],
                        feed_urls$location.bounding_box.maximum_latitude[2],
                        feed_urls$location.bounding_box.maximum_longitude[2],
                        feed_urls$location.bounding_box.minimum_latitude[2],
                        feed_urls$location.bounding_box.minimum_longitude[2],
                        feed_urls$location.bounding_box.minimum_latitude[2]),
                      ncol=2, byrow=TRUE)

box_poly_2 <- st_polygon(list(box_coords_2))

boxes <- st_sfc(box_poly_1, box_poly_2) |>
  st_as_sf()

for(i in 3:nrow(feed_urls)) {
  box_coords_next = matrix(c(feed_urls$location.bounding_box.minimum_longitude[i],
                          feed_urls$location.bounding_box.minimum_latitude[i],
                          feed_urls$location.bounding_box.minimum_longitude[i],
                          feed_urls$location.bounding_box.maximum_latitude[i],
                          feed_urls$location.bounding_box.maximum_longitude[i],
                          feed_urls$location.bounding_box.maximum_latitude[i],
                          feed_urls$location.bounding_box.maximum_longitude[i],
                          feed_urls$location.bounding_box.minimum_latitude[i],
                          feed_urls$location.bounding_box.minimum_longitude[i],
                          feed_urls$location.bounding_box.minimum_latitude[i]),
                        ncol=2, byrow=TRUE)
  
  box_poly_next <- st_polygon(list(box_coords_next)) |> st_sfc() |> st_as_sf()
  
  boxes <- rbind(boxes, box_poly_next)
}

feed_urls <- feed_urls |>
  mutate(id = seq(1:nrow(feed_urls)))

boxes <- boxes |>
  mutate(id = seq(1:nrow(boxes))) |>
  left_join(feed_urls) |>
  mutate(area = st_area(boxes)) |>
  st_set_crs("WGS84") |>
  st_filter(boundary)

#############################################
# Make a list of URLS, with an associated list of MSAs for each
# Only include MSAs for which we don't already have network data

#### Read in CBSA names
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

# Get MSA Boundaries for where we don't have gtfs data


# Check for the ones where we already have data
all_cbsa_folders <- cbsas$`CBSA Title` |>
  str_replace_all(' ', '-') |>
  str_replace_all(',', '')

cbsas <- cbsas |>
  mutate(folder_name = all_cbsa_folders)

have_cbsa_folders <- here("network-files") |>
  dir()

new_cbsas <- cbsas |>
  filter(folder_name %!in% have_cbsa_folders)

boundary <- core_based_statistical_areas(year = 2023) |>
  filter(NAME %in% new_cbsas$`CBSA Title`) |>
  st_transform("WGS84") 

################################

msas_1 <- boundary |>
  st_filter(boxes$x[1])

msa_folder_1 <- msas_1$NAME |>
  str_replace_all(' ', '-') |>
  str_replace_all(',', '')
  
msas_by_url_1 <- list(boxes$urls.direct_download[1],
                    msa_folder_1)

msas_2 <- boundary |>
  st_filter(boxes$x[2])

msa_folder_2 <- msas_2$NAME |>
  str_replace_all(' ', '-') |>
  str_replace_all(',', '')

msas_by_url_2 <- list(boxes$urls.direct_download[2],
                      msa_folder_2)

msas_by_url <- list(msas_by_url_1, msas_by_url_2)

for (i in 3:nrow(boxes)) {
  msas_next <- boundary |>
    st_filter(boxes$x[i])
  
  msa_folder_next <- msas_next$NAME |>
    str_replace_all(' ', '-') |>
    str_replace_all(',', '')
  
  msas_by_url_next <- list(boxes$urls.direct_download[i],
                        msa_folder_next)
  
  msas_by_url[[i]] <- msas_by_url_next
}

#############################
## Write a batch file
bat_lines <- paste0("curl -L -o gtfs1.zip ",
                      msas_by_url[[1]][[1]])

if(length(msas_by_url[[1]][[2]]) <6) {
  for (i in length(msas_by_url[[1]][[2]])) {
    bat_lines_next <- paste("copy gtfs1.zip ",
                            msas_by_url[[1]][[2]][i])
    bat_lines <- append(bat_lines, bat_lines_next)
  }
}


bat_lines <- append(bat_lines, "Del gtfs1.zip")

for (i in 2:length(msas_by_url)) {
  bat_lines_next <- paste0("curl -L -o gtfs",
                           i,
                           ".zip ",
                           msas_by_url[[i]][[1]])
  
  bat_lines <- append(bat_lines, bat_lines_next)
  
  # Don't include the ones with bbox that covers more than 5 cbsas.
  # These are large intercity systems like Amtrak that probably don't serve
  # all cities in the bounding box and may lead to time zone issues.
  if(length(msas_by_url[[i]][[2]]) < 6) {
    for (j in 1:length(msas_by_url[[i]][[2]])) {
      bat_lines_next <- paste0("copy gtfs",
                               i,
                               ".zip ",
                               msas_by_url[[i]][[2]][j])
      bat_lines <- append(bat_lines, bat_lines_next)
    }  
  }
  
  bat_lines <- append(bat_lines, paste0("Del gtfs",i,".zip"))
  
}

write_lines(bat_lines, file = here("network-files",
                               "get_gtfs.bat"),
            append = FALSE)

### Write a batch file to delete all the gtfs feeds
bat_lines <- "REM beginning of file"

folders <- here("network-files") |>
  dir()

for(i in 2:length(folders)) {
  zip_files <- here("network-files",
                    folders[i]) |>
    dir()
  
  zip_files <- zip_files[str_detect(zip_files, ".zip")]
  
  for(j in 1:length(zip_files)) {
    bat_lines <- append(bat_lines, paste0("Del ", 
                                          folders[i],
                                          "\\",
                                          zip_files[j]))
  }
}

write_lines(bat_lines, file = here("network-files",
                                   "delete_gtfs.bat"),
            append = FALSE)


