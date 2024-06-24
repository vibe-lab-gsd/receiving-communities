###

library(tidyverse)
library(here)
library(tidycensus)
library(sf)
library(readxl)

###########################################################################
## Read in 2021 LEHD data for all states (continguous + DC)
###################################################################
lower_states <- c("al", "az", "ar", "co", "ca", "ct", "de", "dc", "fl", "ga",
                  "id", "il", "in", "ia", "ks", "ky", "la", "me", "md", "ma", 
                  "mi", "mn", "mi", "ms", "mt", "ne", "nv", "nh", "nj", "nm", 
                  "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc", "sd",
                  "tn", "tx", "ut", "vt", "va", "wa", "wv" , "wi", "wy")

this_jobs <- read_csv("https://lehd.ces.census.gov/data/lodes/LODES8/al/wac/al_wac_S000_JT00_2018.csv.gz")

all_jobs <- this_jobs

for (i in 2:length(lower_states)) {
  this_url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/",
                     lower_states[i],
                     "/wac/",
                     lower_states[i],
                     "_wac_S000_JT00_2018.csv.gz")
  
  this_jobs <- read_csv(this_url)
  
  all_jobs <- rbind(all_jobs, this_jobs)
}

all_jobs <- all_jobs |>
  rename(total_emp = C000) |>
  mutate(basic_emp = CNS01+CNS02+CNS03+CNS04+CNS05+CNS06+CNS08+CNS09) |>
  rename(retail_emp = CNS07) |>
  mutate(service_emp = total_emp - basic_emp - retail_emp) |>
  select(w_geocode,
         total_emp,
         basic_emp,
         retail_emp,
         service_emp) 

write_csv(all_jobs, here("geography-data", "lehd_jobs.csv"))

##################################################################################
##  Read in CBSA crosswalk
################################################################################

# list1_2023.xlsx is from https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
# Accessed May 29, 2024

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
           `State Name` != "Puerto Rico")


################################################################################
## Get Centroid locations and number of households in each block
################################################################################

cbsas_by_state <- cbsas |>
  group_by(`CBSA Title`, `FIPS State Code`) |>
  summarise(n_counties = n())

cbsas_unique <- cbsas |>
  group_by(`CBSA Title`) |>
  summarise(n_counties = n())

## All states except Connecticut
no_ct_cbsas <- cbsas_by_state |>
  filter(`FIPS State Code` != "09")

for (i in 1:nrow(no_ct_cbsas)) {
  state <- no_ct_cbsas$`FIPS State Code`[i]
  
  counties <- cbsas$`FIPS County Code`[
    cbsas$`CBSA Title` == no_ct_cbsas$`CBSA Title`[i] &
      cbsas$`FIPS State Code` == no_ct_cbsas$`FIPS State Code`[i]]
  
  blocks <- get_decennial(geography = "block",
                          variables = "H10_001N",
                          state = state,
                          county = counties,
                          sumfile = "dhc",
                          geometry = TRUE) |>
    st_transform("WGS84") |>
    st_centroid()
  
  folder <- str_replace_all(no_ct_cbsas$`CBSA Title`[i] , ' ', '-')
  folder <- str_replace_all(folder, ',', '')
  folder <- str_replace_all(folder, '/', '-')
  
  filename <- paste0("blocks", no_ct_cbsas$`FIPS State Code`[i], ".geojson")
  
  dir.create(here("block-points",
                  folder))
  
  st_write(blocks, here("block-points",
                        folder,
                        filename), delete_dsn = TRUE)
}

# Connecticut
# See https://gist.github.com/arthurgailes/45f781c219320f3e9a962a2b3e9f55d2
ct_block <- get_decennial(geography = "block",
                          variables = "H10_001N",
                          state = "09", 
                          year = 2020,
                          sumfile = "dhc",
                          geometry = TRUE) |>
            st_transform("WGS84") |>
            st_centroid()

ct_cbsas <- cbsas_by_state |>
  filter(`FIPS State Code` == "09") 

ct_cbsa_geog <- tigris::core_based_statistical_areas(year = 2023) |>
  filter(NAME %in% ct_cbsas$`CBSA Title`) |>
  st_transform("WGS84")

for (i in 1:nrow(ct_cbsa_geog)) {
  folder <- str_replace_all(ct_cbsa_geog$NAME[i] , ' ', '-')
  folder <- str_replace_all(folder, ',', '')
  folder <- str_replace_all(folder, '/', '-')

  filename <- "blocks09.geojson"

  dir.create(here("block-points",
                  folder))

  blocks <- ct_block |>
    st_filter(ct_cbsa_geog[i])
  
  st_write(blocks, here("block-points",
                        folder,
                        filename), delete_dsn = TRUE)
}

msa_folders <- here("block-points") |>
  dir()

for (i in 1:length(msa_folders)) {
  these_pts <- here("block-points",
                   msa_folders[i]) |>
    dir()
  
  if(length(these_pts) > 1) {
    blocks_all <- here("block-points",
                       msa_folders[i],
                       these_pts[1]) |>
      st_read()
    
    here("block-points",
         msa_folders[i],
         these_pts[1]) |>
    file.remove()
    
    for(j in 2:length(these_pts)) {
      block_next <- here("block-points",
                         msa_folders[i],
                         these_pts[j]) |>
        st_read()
      
      blocks_all <- rbind(blocks_all, block_next)
      
      here("block-points",
           msa_folders[i],
           these_pts[j]) |>
        file.remove()
    }
    st_write(blocks_all,
             here("block-points",
                  msa_folders[i],
                  "blocks.geojson"))
  }
}

lehd_jobs <- here("geography-data",
                  "lehd_jobs.csv") |>
  read_csv() 

for (i in 1:length(msa_folders)) {
  these_pts <- here("block-points",
                    msa_folders[i]) |>
    list.files(full.names = TRUE) |>
    st_read() |>
    rename(n_HHs = value) |>
    left_join(lehd_jobs) |>
    rename(id = GEOID) |>
    select(id, n_jobs, population) |>
    replace_na(list(n_jobs = 0)) |>
    mutate(hbo_attr = 0.7 * tot_hhsE +
                      0.7 * basic_emp +
                      8.4 * retail_emp +
                      3.5 * service_emp,
           hbw_attr = 1.2 * total_emp,
           nhb_attr = 0.6 * tot_hhsE +
                      0.5 * basic_emp +
                      4.7 * retail_emp +
                      1.4 * service_emp) |>
    mutate(total_attr = hbo_attr +
                        hbw_attr +
                        nhb_attr)
  
  st_write(these_pts,
           here("block-points",
                msa_folders[i],
                "block-data.geojson"),
           delete_dsn = TRUE)
}
