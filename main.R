# Packages and functions --------------------------------------------------

# library(data.table)
library(tidyverse)
library(geosphere)
library(igraph)



# Get data ----------------------------------------------------------------
#' The source of data: http://opendata.praha.eu


dir.create("_temp", showWarnings = FALSE)
fl <- "_temp/jrdata.zip"

download.file("http://opendata.iprpraha.cz/DPP/JR/jrdata.zip", fl)

pth.dir <-  "_temp/jrdata"
unzip(fl, exdir = pth.dir)

pth.fls <- dir(pth.dir, full.names = TRUE)


jr.info <- readr::read_delim(pth.fls[1], delim = ":", skip = 2, locale = locale("cs", encoding = "windows-1250"), col_names = c("co", "hodnota"))
jr.data <- sapply(pth.fls[-1], readr::read_csv, locale = locale("cs"))
names(jr.data) <- names(jr.data) %>% basename %>% str_remove("[.].*$")



# Stop's id recode --------------------------------------------------------
#' To save some space lets recode stop's id to just id..

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



# Count walking distances between stops -----------------------------------
#' Let's make it simpliest - just count the distance on Earth's surface and then multiply it 
#' by average human's walking speed. Then add some overhead penalty.

c.walk_overhead_penalty_secs <- 30 # penalty for changing means of transport, secs
c.avg_walking_speed <-  1.4 # source: wikipedia.org


d.stops_dist <- geosphere::distm(jr.data$stops[,c("stop_lon", "stop_lat")]) %>%
  as.dist %>%
  broom::tidy(diagonal = FALSE, upper = FALSE) %>% 
  mutate(duration = distance/c.avg_walking_speed + c.walk_overhead_penalty_secs) %>% 
  select(-distance) %>% 
  rename(stop_id.d = item1, stop_id.a = item2)

  
# Create stop-stop segments -----------------------------------------------

d.depart <- jr.data$stop_times %>%
  select(-arrival_time) %>% 
  rename(stop_id.d = stop_id)

d.arrival <- jr.data$stop_times %>% 
  mutate(waiting_time.a = departure_time - arrival_time, prev_stop_sequence = stop_sequence -1) %>% 
  select(trip_id, prev_stop_sequence, stop_id, arrival_time, waiting_time.a) %>% 
  rename(stop_id.a = stop_id)

d.segments <- d.depart %>%
  inner_join(d.arrival, by = c("trip_id" = "trip_id", "stop_sequence" = "prev_stop_sequence")) %>% 
  rename(segment_sequence = stop_sequence) %>% 
  select(trip_id, segment_sequence, stop_id.d, stop_id.a, departure_time, arrival_time, waiting_time.a) %>% 
  mutate(duration = arrival_time - departure_time)

rm(d.arrival, d.depart)



# Build a graph of stops --------------------------------------------------

# g <- igraph::graph_from_data_frame(d.segments[,c("stop_name.d", "stop_name.a")], directed = TRUE)
g <- igraph::graph_from_data_frame(d.segments[,c("stop_id.d", "stop_id.a")], directed = TRUE)


E(g)$weight <- d.segments.summary$min_duration[match(attr(E(g), "vnames"), d.segments.summary$edge_label)]
E(g)$departure_time
E(g)$trip_id

#' TODO:
#'  * přidej do grafu spojnice zastávek pěší chůzí (manhattan distance * odhad rychlosti + nějaká konstanta na režii)
#'  * přidej vlastnosti hran "departure_time, arrival_time, trip_id"
#'  * myšlenka:
#'     - pokud jsem ve vrcholu V a čase T, mohu vzít jen hrany v čase t+konst, pokud přestupuji a t, pokud jde o stejný trip_id
#' * graf by měl mít více hran mezi týmiž vrcholy --> !simplify



# this counts theoretic minimum; but
igraph::distances(g, v = "U179Z5", to = "U171Z1") 
igraph::shortest.paths(g, v = "U179Z5", to = "U171Z1")
# igraph::get.all.shortest.paths(g, from = "U179Z5", to = "U171Z1")



## TODO: přidej ztotožnění zastávek se stejným jménem (!přestupy)
## TODO: přidej vyhodnocení skutečné doby
## TODO: přidej vyhledávání do hloubky alternativních tras




# Computing distances to given stop (at given time) -----------------------



# Agregation --------------------------------------------------------------



# Visualisation -----------------------------------------------------------


