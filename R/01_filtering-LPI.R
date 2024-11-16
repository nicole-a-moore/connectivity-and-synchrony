## LPI data cleaning script
library(tidyverse)
theme_set(theme_bw())
select <- dplyr::select

## read in LPI data
lpi <- read.csv("data-raw/lpi-data/LivingPlanetIndex_2024_PublicData/LPD_2024_public.csv")

## filter to terrestrial species 
lpi <- lpi[lpi$System == "Terrestrial",]

## filter to populations in North America
lpi <- lpi[lpi$Region %in% c("North America"),]

## filter to species with at least two populations
lpi <- lpi %>%
  group_by(Binomial) %>%
  mutate(num_pops = n()) %>%
  filter(num_pops >= 2) %>%
  select(num_pops, Binomial, everything()) %>%
  ungroup()

## how many species?
length(unique(lpi$Binomial)) #348

## how many populations in each?
lpi %>%
  select(Binomial, num_pops) %>%
  distinct() %>% 
  ggplot(aes(x = num_pops)) + geom_histogram()

## how are they distributed geographically?
countries <- map_data("world")

na = filter(countries, region %in% c("Canada", "Mexico", "USA"))

ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(x = "", y = "") +
  geom_point(data = lpi, 
             aes(x = Longitude, y = Latitude, colour = Binomial),
             inherit.aes = FALSE, 
             size = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) 

## calculate the distance between each population within species 

## first, label each population with a unique identifier 
lpi <- lpi %>%
  group_by(Binomial) %>%
  mutate(pop_id = paste(Binomial, 1:n(), sep = "-")) %>%
  select(pop_id, everything())

## then, make matrix that crosses coordinates of each pair of populations 
## split into separate dfs by species 
split <- lpi %>%
  group_by(Binomial) %>%
  group_split()

## select just the relevant columns
split <- map(.x = split,
             .f = ~select(.x, pop_id, Longitude, Latitude))

## for each species, calculate dists between populations 
dists = map(.x = split,
            .f = ~as.data.frame(geosphere::distm(.x[,which(colnames(.x) %in% c("Longitude", "Latitude"))], 
                                                 .x[,which(colnames(.x) %in% c("Longitude", "Latitude"))], 
                                                 fun = geosphere::distHaversine)) %>%
              `colnames<-`(.x$pop_id) %>%
              mutate(pop_1 = .x$pop_id) %>%
              gather(., key = "pop_2", value = "dist", 1:(ncol(.)-1)) %>%
              distinct() %>%
              filter(dist != 0))

## combine all
dists <- bind_rows(dists) %>%
  mutate(Binomial = str_split_fixed(.$pop_1, "\\-", 2)[,1]) %>%
  group_by(Binomial, dist) %>%
  filter(row_number()==1) %>%
  ungroup()
  #get rid of non-unique pairs

## convert distance to km
dists$dist = dists$dist/1000

## plot 
dists %>%
  select(dist) %>%
  distinct() %>%
  ggplot(aes(x = dist)) + geom_histogram()

## plot two on a map to see how far 
two_pops = filter(lpi, pop_id %in% c("Canis_lupus-19", "Canis_lupus-15"))

ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(x = "", y = "") +
  geom_point(data = two_pops, 
             aes(x = Longitude, y = Latitude, colour = Binomial),
             inherit.aes = FALSE, 
             size = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) 

## come up with a threshold based on the kind of species?
## leave for now... 

dists <- dists %>%
  group_by(Binomial) %>%
  filter(n() >= 2)

length(unique(dists$Binomial)) ## 271 species remain 

## plot these only 
some_pops = filter(lpi, pop_id %in% c(dists$pop_1, dists$pop_2))

ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(x = "", y = "") +
  geom_point(data = some_pops, 
             aes(x = Longitude, y = Latitude, colour = Binomial),
             inherit.aes = FALSE, 
             size = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) 

## draw lines between each population that will be compared 
split <- bind_rows(split)

leftjoin1 <- split %>%
  rename("Latitude1" = Latitude, "Longitude1" = Longitude)
leftjoin2 <- split %>%
  rename("Latitude2" = Latitude, "Longitude2" = Longitude)

dists_points <- left_join(dists, leftjoin1, by = c("pop_1" = "pop_id"))
dists_points <- left_join(dists_points, leftjoin2, by = c("pop_2" = "pop_id"))

ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(y = "Latitude", x = "Longitude") +
  geom_point(data = some_pops, 
             aes(x = Longitude, y = Latitude, colour = Binomial),
             inherit.aes = FALSE, 
             size = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) +
  geom_segment(data = dists_points,
               aes(x = Longitude1, y = Latitude1, xend = Longitude2, yend = Latitude2, colour = Binomial), 
                   inherit.aes = FALSE, linewidth = .25) 

## save plot
ggsave(width = 6, height = 4, path = "figures", filename = "map-of-populations.png")

## save dataset 
write.csv(some_pops, "data-processed/lpi-subset.csv", row.names = FALSE)
write.csv(dists_points, "data-processed/pop-distance-key.csv", row.names = FALSE)


## next:
## filter to ts of populations with overlapping obs times
dists_points <- read.csv("data-processed/pop-distance-key.csv")
some_pops <- read.csv("data-processed/lpi-subset.csv")

## make column for first and last year with data 
reorg <- some_pops %>%
  gather(key = "year", value = "pop_size", contains("X")) %>%
  mutate(year = as.numeric(as.character(str_split_fixed(year, "X", 2)[,2])), 
         pop_size = as.numeric(as.character(pop_size))) %>%
  filter(!is.null(pop_size), !is.na(pop_size)) %>%
  group_by(pop_id) %>%
  mutate(first_year = min(year),
         last_year = max(year)) %>%
    select(first_year, last_year, pop_size, year, pop_id, everything())

## filter out populations with less than 7 observations through time 
reorg <- reorg %>%
  group_by(pop_id) %>%
  filter(n() > 7)

## get rid of cases where first year of pop 1 > last year of pop 2, first year of pop 2 < last year of pop 1
years1 <- select(reorg, pop_id, first_year, last_year) %>%
  rename(first_year1 = "first_year", last_year1 = "last_year") %>%
  distinct()
years2 <- select(reorg, pop_id, first_year, last_year) %>%
  rename(first_year2 = "first_year", last_year2 = "last_year") %>%
  distinct()

dists_points <- left_join(dists_points, years1, by = c("pop_1" = "pop_id")) %>%
  left_join(., years2, by = c("pop_2" = "pop_id")) 

dists_points <- dists_points %>%
  filter(!first_year1 > last_year2 & !first_year2 > last_year1) 

## filter populations that were removed out of lpi
some_pops <- filter(some_pops, pop_id %in% c(dists_points$pop_1, dists_points$pop_2))
reorg <- filter(reorg, pop_id %in% c(dists_points$pop_1, dists_points$pop_2))

## how many species now?
length(unique(some_pops$Binomial)) #231

## replot
ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(y = "Latitude", x = "Longitude") +
  geom_point(data = some_pops, 
             aes(x = Longitude, y = Latitude, colour = Binomial),
             inherit.aes = FALSE, 
             size = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) +
  geom_segment(data = dists_points,
               aes(x = Longitude1, y = Latitude1, xend = Longitude2, yend = Latitude2, colour = Binomial), 
               inherit.aes = FALSE, linewidth = .05) 


## save dataset 
write.csv(some_pops, "data-processed/lpi-subset.csv", row.names = FALSE)
write.csv(dists_points, "data-processed/pop-distance-key.csv", row.names = FALSE)
write.csv(reorg, "data-processed/lpi-subset-long.csv", row.names = FALSE)

