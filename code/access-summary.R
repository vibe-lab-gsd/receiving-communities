library(tidyverse)
library(here)

point_folders <- here("block-points") |>
  dir()

access_summary <- tibble(place = "test",
                         avg_car_access = 0,
                         avg_no_car_access = 0,
                         total_jobs = 0,
                         total_popn = 0,
                         jobs_per_cap = 0,
                         no_car_share = 0) |>
  filter(total_jobs > 0)

for(i in 1:length(point_folders)) {
  check_folders <- here("block-points",
                        point_folders[i]) |>
    dir()
  
  if("access_calc.csv" %in% check_folders) {
    this_access <- here("block-points",
                        point_folders[i],
                        "access_calc.csv") |>
      read_csv()
    
    avg_car_access <- sum(this_access$car_access*this_access$population) /
      sum(this_access$population)
    
    avg_no_car_access <- sum(this_access$no_car_access*this_access$population) /
      sum(this_access$population)
    
    total_jobs <- sum(this_access$n_jobs)
    total_popn <- sum(this_access$population)
    
    jobs_per_cap <- total_jobs / total_popn
    
    jobs_pct_sq <- (this_access$n_jobs / total_jobs)^2
    
    job_hhi <- sum(jobs_pct_sq)
    
    no_car_share <- avg_no_car_access / avg_car_access
    
    this_summary <- tibble(place = point_folders[i],
                           avg_car_access = avg_car_access,
                           avg_no_car_access = avg_no_car_access,
                           total_jobs = total_jobs,
                           total_popn = total_popn,
                           jobs_per_cap = jobs_per_cap,
                           job_hhi = job_hhi,
                           no_car_share = no_car_share)
    
    access_summary <- rbind(access_summary, this_summary)
  }
}

ggplot(access_summary) +
  geom_point(aes(x = no_car_share,
                 y = total_popn)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(access_summary) +
  geom_point(aes(x = no_car_share,
             y = total_jobs)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(access_summary) +
  geom_point(aes(x = avg_no_car_access,
                 y = total_jobs)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(access_summary) +
  geom_point(aes(x = avg_car_access,
                 y = total_jobs)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(access_summary) +
  geom_point(aes(x = avg_car_access,
                 y = jobs_per_cap)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")


ggplot(access_summary) +
  geom_point(aes(x = jobs_per_cap,
                 y = no_car_share)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(access_summary) +
  geom_point(aes(x = job_hhi,
                 y = no_car_share)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

cor.test(log(access_summary$total_jobs), log(access_summary$avg_car_access))

cor.test(access_summary$total_jobs, access_summary$avg_no_car_access)

cor.test(access_summary$total_jobs, access_summary$no_car_share)

cor.test(access_summary$jobs_per_cap, access_summary$avg_car_access)

cor.test(access_summary$jobs_per_cap, access_summary$no_car_share)


## Some notes:

# Should we weight jobs based on trip generation? (this would take a while, but
# doing it via the cluster might be okay - could spend about a week on it)

# What is the relevant measure: Should the average be per household or per 
# person? Should it be a trip generation per household measure?

# For ranking - we can use average car access, average non-car access, and the 
# ratio of the two. Or use the residual for something that's highly correlated
# (total jobs? Car access?).

# If the ratio is a measure of car-dependency, does it correlate with
# Commute to work shares?

# Use a segregation index as a predictor? 
#   Three predictors:
#      * Land use (segregation index) - at a level higher than the block - 
#        Tract? Grid? Spatial correlation?
#      * Transit service (total VRM per area)
#      * Pedestrian network (intersection density or block length)