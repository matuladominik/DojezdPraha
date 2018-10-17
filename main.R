# Packages and functions --------------------------------------------------

# library(data.table)
library(tidyverse)
library(geosphere)
# library(igraph)
library(lubridate)
library(ggmap)


# Get data ----------------------------------------------------------------
#' The source of data: http://opendata.praha.eu


dir.create("_temp", showWarnings = FALSE)
fl <- "_temp/jrdata.zip"

# download.file("http://opendata.iprpraha.cz/DPP/JR/jrdata.zip", fl)

pth.dir <-  "_temp/jrdata"
# unzip(fl, exdir = pth.dir)

pth.fls <- dir(pth.dir, full.names = TRUE)


jr.info <- readr::read_delim(pth.fls[1], delim = ":", skip = 2, locale = locale("cs", encoding = "windows-1250"), col_names = c("co", "hodnota"))
jr.data <- sapply(pth.fls[-1], readr::read_csv, locale = locale("cs"))
names(jr.data) <- names(jr.data) %>% basename %>% str_remove("[.].*$")



# Filter data  ------------------------------------------------------------
#' Let's downdize the data just to the day we are interested in.
#' (Plus we'll add next day - just to complete trips startet before midnight..).

jr.data$calendar <- jr.data$calendar %>%
  mutate(start_date = ymd(start_date),
         end_date = ymd(end_date))

jr.data$calendar_dates <- jr.data$calendar_dates %>% mutate(date = ymd(date))

# pick dates
c.days <- jr.data$calendar_dates$date %>% sample(1)
c.days <- c.days + 0:1


# filter datasets
jr.data$calendar <- jr.data$calendar %>%
  filter(start_date <= c.days[1], end_date >= c.days[2])

l.available_services <- lapply(c.days,function(i_day) {
  
  i_wday <- lubridate::wday(i_day, week_start = 1, label = TRUE, abbr = FALSE,
                            locale = "English_United States.1252") %>% str_to_lower()
  
  kt <- jr.data$calendar[,i_wday] == 1
  jr.data$calendar$service_id[kt]
})
names(l.available_services) <- c.days
  

# Optionaly - remove services with an exception..
# jr.data$calendar_dates <- jr.data$calendar_dates %>% 
#   filter(date %in% c.days, exception_type == 2)
# l.available_services <- lapply(l.available_services, function(available_services) {
#   setdiff(available_services, jr.data$calendar_dates$service_id)
# })

# Remove unavailable services
jr.data$trips <- jr.data$trips %>% 
  filter(service_id %in% unlist(l.available_services))

l.available_trips <- lapply(l.available_services, function(available_services) {
  jr.data$trips %>% 
    filter(service_id %in% available_services) %>% 
    select(trip_id) %>% 
    unlist(use.names = FALSE) %>% 
    unique
})

jr.data$stop_times <- jr.data$stop_times %>% 
  filter(trip_id %in% unlist(l.available_trips))

available_stops <- jr.data$stop_times$stop_id %>% unique
jr.data$stops <- jr.data$stops %>% filter(stop_id %in% available_stops)


# also remove stop times without arrival/departure time specified
jr.data$stop_times <- jr.data$stop_times %>% 
  filter(!is.na(arrival_time) & !is.na(departure_time))


# Stop's id recode --------------------------------------------------------
#' To save some space lets recode stop's id to just id.

d.stops_id_dict <- jr.data$stops %>%
  mutate(stop_id_orig = stop_id, stop_id = row_number()) %>% 
  select(stop_id, stop_id_orig, stop_name, location_type, parent_station, wheelchair_boarding)

jr.data$stops <- jr.data$stops %>%
  rename(stop_id_orig = stop_id) %>% 
  left_join(select(d.stops_id_dict, stop_id, stop_id_orig), by = "stop_id_orig") %>% 
  select(stop_id, stop_lat, stop_lon)

jr.data$stop_times <- jr.data$stop_times %>% 
  rename(stop_id_orig = stop_id) %>% 
  left_join(select(d.stops_id_dict, stop_id, stop_id_orig), by = "stop_id_orig") %>% 
  select(trip_id, stop_id, arrival_time, departure_time, stop_sequence)

# also we'll make concrete departure/arrival times

temp_stop_times <- lapply(as.character(c.days), function(i_day) {
  
  available_trips <- l.available_trips[[i_day]]
  
  jr.data$stop_times %>% 
    filter(trip_id %in% available_trips) %>% 
    mutate(arrival_time = as_datetime(paste(i_day, arrival_time)),
           departure_time = as_datetime(paste(i_day, departure_time)))
})
jr.data$stop_times <- do.call("rbind", temp_stop_times)
rm(temp_stop_times)



# Count walking distances between stops -----------------------------------
#' Let's make it simpliest - just count the distance on Earth's surface and then multiply it 
#' by average human's walking speed. Then add some overhead penalty.
#' 
#' Optionally - use manhattan distance to reflect necesity of following roads etc. 

c.walk_overhead_penalty_secs <- 30 # penalty for changing means of transport, secs
c.avg_walking_speed <-  1.4 # source: wikipedia.org


d.stops_walking_dist <- geosphere::distm(jr.data$stops[,c("stop_lon", "stop_lat")]) %>%
  as.dist %>%
  broom::tidy(diagonal = FALSE, upper = FALSE) %>% 
  mutate(duration = distance/c.avg_walking_speed + c.walk_overhead_penalty_secs) %>% 
  select(-distance) %>% 
  rename(stop_id.d = item1, stop_id.a = item2)

# d.stops_walking_dist$trip_id <- 0  # 0 means use your legs :)
# d.stops_walking_dist$departure_time <- NA # and you can start whenever you want


# Create trips 
#' For each trip we will take into account possibility to use it for 1-k stops (starting on
#' whichever stop you want...)
#' 

stops_seq <- seq(max(jr.data$stop_times$stop_sequence))
stops_seq_pairs <- expand.grid(stops_seq, stops_seq) %>%
  rename(stop_seq_id.d = Var1, stop_seq_id.a = Var2) %>% 
  filter(stop_seq_id.d < stop_seq_id.a)

l.sub_paths <- stops_seq_pairs%>% 
  apply(1, function(comb){
    
    d.departure <- jr.data$stop_times %>% 
      filter(stop_sequence == comb["stop_seq_id.d"]) %>% 
      select(trip_id, stop_id, departure_time) %>% 
      rename(stop_id.d = stop_id)
    d.arrival <- jr.data$stop_times %>% 
      filter(stop_sequence == comb["stop_seq_id.a"]) %>% 
      select(trip_id, stop_id, arrival_time) %>% 
      rename(stop_id.a = stop_id)
    
    d.path <- inner_join(d.departure, d.arrival, by = c("trip_id")) %>% 
      filter(arrival_time >= departure_time & arrival_time < departure_time + ddays(1)) %>% 
      select(stop_id.d, stop_id.a, departure_time, arrival_time)
  })
d.sub_paths <- do.call("rbind", l.sub_paths)
rm(l.sub_paths)


# d.stops_dist <- rbind(d.sub_paths, d.stops_walking_dist[,colnames(d.sub_paths)])
# rm(d.stops_walking_dist, d.sub_paths)
gc()


# Computing distances to given stop (at given time) -----------------------

init_stop <-  2539
init_time <- lubridate::ymd_hms(paste(as.character(c.days[1]),"07:00:00"))
c.max_iter <- 1e2

# init - distance by walk/one vehicle 
d.best <- d.stops_walking_dist %>% 
  filter(stop_id.d == init_stop) %>%
  mutate(arrival_time = duration + init_time, 
         updated = (stop_id.a == init_stop)) %>% 
  rename(stop_id = stop_id.a) %>% 
  select(stop_id, arrival_time, updated)

d.best <- rbind(d.best,
                tibble(stop_id = init_stop,
                       arrival_time = init_time,
                       updated = TRUE))


iter.no <- 0
n.updated <- sum(d.best$updated)


while (iter.no < c.max_iter && n.updated > 0) {
  
  iter.no <- iter.no + 1
  cat('Iter no. ', iter.no, ', last upd.:',n.updated,' (',as.character(Sys.time()),')\n')
  
  # TODO!! -> pro i>0:
  # TODO!! -> move transfer overhead constant here (from walking time above)

  d.vysetruji <- d.best %>% 
    filter(updated == TRUE) %>% 
    rename(prev_arrival_time = arrival_time) %>% 
    select(stop_id, prev_arrival_time) 
  
  d.update <- d.sub_paths %>%
    inner_join(d.vysetruji, by = c("stop_id.d" = "stop_id")) %>% 
    filter(prev_arrival_time < departure_time) %>% 
    group_by(stop_id.a) %>% 
    summarise(new_arrival_time = min(arrival_time, na.rm = TRUE)) %>% 
    left_join(d.best, by = c("stop_id.a" = "stop_id")) %>% 
    filter(new_arrival_time < arrival_time | is.na(arrival_time)) %>% 
    select(stop_id.a, new_arrival_time)
  
  d.best <- d.best %>% 
    left_join(d.update, by = c("stop_id" = "stop_id.a")) %>% 
    mutate(updated = !is.na(new_arrival_time),
           arrival_time = pmin(new_arrival_time, arrival_time, na.rm = TRUE)) %>% 
    select(stop_id, arrival_time, updated) 
  
  n.updated <- sum(d.best$updated)
}





# Agregation --------------------------------------------------------------



# Visualisation -----------------------------------------------------------

d.best <- d.best %>% 
  left_join(d.stops_id_dict, by = "stop_id") %>% 
  left_join(jr.data$stops, by = "stop_id") %>% 
  mutate(min_duration = arrival_time - init_time)

d.best %>% 
  ggplot(aes(x = stop_lat, y = stop_lon, col = as.numeric(min_duration)/3600)) + 
  geom_point() + 
  theme_bw() + 
  scale_color_viridis_c()

d.best %>% 
  filter(min_duration < 3600) %>% 
  ggplot(aes(x = stop_lat, y = stop_lon, col = as.numeric(min_duration)/60)) + 
  geom_point() + 
  theme_bw() + 
  scale_color_viridis_c()



init_stop_spatial <- jr.data$stops %>% 
  filter(stop_id == init_stop) %>% 
  select(stop_lat, stop_lon) %>% 
  unlist


prague_map = ggmap::get_map(location = init_stop_spatial, maptype = "roadmap",
                           zoom = 11, color = "bw")

# install.packages("maps")

library(ggmap)

map_box <- ggmap::make_bbox(lon = jr.data$stops$stop_lon, lat = jr.data$stops$stop_lat, f = .01)

m <- ggmap::get_stamenmap(map_box, maptype = "toner-hybrid")

ggmap(m) + geom_point(data = jr.data$stops, aes(x = stop_lon, y = stop_lat, col = factor(location_type)))
