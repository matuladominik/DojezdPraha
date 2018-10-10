# Packages and functions --------------------------------------------------

# library(data.table)
library(tidyverse)
library(igraph)


# Get data ----------------------------------------------------------------
#' The source of data: http://opendata.praha.eu


dir.create("_temp")
fl <- "_temp/jrdata.zip"

download.file("http://opendata.iprpraha.cz/DPP/JR/jrdata.zip", fl)

pth.dir <-  "_temp/jrdata"
unzip(fl, exdir = pth.dir)

pth.fls <- dir(pth.dir, full.names = TRUE)


jr.info <- readr::read_delim(pth.fls[1], delim = ":", skip = 2, locale = locale("cs", encoding = "windows-1250"), col_names = c("co", "hodnota"))
jr.data <- sapply(pth.fls[-1], readr::read_csv, locale = locale("cs"))
names(jr.data) <- names(jr.data) %>% basename %>% str_remove("[.].*$")


# agency -- kdo obsluhuje
# calendar -- ve ktere dny jede
# calendar_dates -- dny ve vyberu
# linka (konecna-konecna, id, kdo obsluhuje, cislo, prip. barva)
# shapes -- 3332 tvarů na povrhcu zeme
# stop_times -- zastavkove casy (pro kazdou zastavku kazdou linku příjezdy a odjezdy)



# Create stop-stop segments -----------------------------------------------

d.depart <- jr.data$stop_times %>%
  select(trip_id, stop_sequence, stop_id, departure_time) %>% 
  rename(stop_id.d = stop_id)

d.arrival <- jr.data$stop_times %>% 
  mutate(waiting_time.a = departure_time - arrival_time, prev_stop_sequence = stop_sequence -1) %>% 
  select(trip_id, prev_stop_sequence, stop_id, arrival_time, waiting_time.a) %>% 
  rename(stop_id.a = stop_id)

d.segments <- d.depart %>%
  inner_join(d.arrival, by = c("trip_id" = "trip_id", "stop_sequence" = "prev_stop_sequence")) %>% 
  rename(segment_sequence = stop_sequence) %>% 
  select(trip_id, segment_sequence, stop_id.d, stop_id.a, departure_time, arrival_time, waiting_time.a)

d.segments <- d.segments %>% mutate(duration_time = arrival_time - departure_time)
  
# test if there is multiple durations between two stops (maybe using different means of transport...)


# Docasne omezeni ---------------------------------------------------------

# warning("Prozatim omezim na cast, pak odmazat!") # TODO
# d.segments <- head(d.segments, 10)
# 
# 
# d.segments <- jr.data$stops %>%
#   select(stop_id, stop_name) %>%
#   rename(stop_name.d = stop_name) %>%
#   right_join(d.segments, by = c("stop_id" = "stop_id.d"))
# d.segments <- jr.data$stops %>% 
#   select(stop_id, stop_name) %>% 
#   rename(stop_name.a = stop_name) %>% 
#   right_join(d.segments, by = c("stop_id" = "stop_id.a"))


# Build a graph of stops --------------------------------------------------

# g <- igraph::graph_from_data_frame(d.segments[,c("stop_name.d", "stop_name.a")], directed = TRUE)
g <- igraph::graph_from_data_frame(d.segments[,c("stop_id.d", "stop_id.a")], directed = TRUE)

d.segments.summary <- d.segments %>% group_by(stop_id.d, stop_id.a) %>%
  summarise(min_duration = min(duration_time, na.rm = TRUE),
            avg_duration = mean(duration_time, na.rm = TRUE),
            max_duration = max(duration_time, na.rm = TRUE),
            n_trips = n()) %>% 
  mutate(edge_label = str_c(stop_id.d,"|",stop_id.a))


E(g)$weight <- d.segments.summary$min_duration[match(attr(E(g), "vnames"), d.segments.summary$edge_label)]


# this counts theoretic minimum; but
igraph::distances(g, v = "U179Z5", to = "U171Z1") 
igraph::shortest.paths(g, v = "U179Z5", to = "U171Z", )
# igraph::get.all.shortest.paths(g, from = "U179Z5", to = "U171Z1")



## TODO: přidej ztotožnění zastávek se stejným jménem (!přestupy)
## TODO: přidej vyhodnocení skutečné doby
## TODO: přidej vyhledávání do hloubky alternativních tras




# Computing distances to given stop (at given time) -----------------------



# Agregation --------------------------------------------------------------



# Visualisation -----------------------------------------------------------


