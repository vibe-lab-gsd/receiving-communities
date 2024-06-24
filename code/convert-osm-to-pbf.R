
library(tidyverse)
library(here)

folders <- list.dirs(path = here("network-files"),
                     full.names = FALSE, recursive = FALSE)

lines <- c('@ECHO ON',
           '',
           'osmconvert streets.osm --out-pbf -o=streets_01.pbf',
           '')

for(i in 3:24) {
  next_lines <- c(paste0('copy osmconvert.exe ..\\',
                         folders[i]),
                  'Del streets.osm',
                  'Del osmconvert.exe',
                  '',
                  paste0('cd ..\\',
                         folders[i]),
                  'osmconvert streets.osm --out-pbf -o=streets_01.pbf',
                  '')
  
  lines <- append(lines, next_lines)
}

lines <- append(lines, c('Del streets.osm',
                         '',
                         'PAUSE'))

write_lines(lines, file = here("network-files",
                               folders[3],
                               "convert-osm-to-pbf.bat"),
            append = FALSE)
