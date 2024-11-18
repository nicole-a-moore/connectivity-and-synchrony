## calcualte the synchrony between population time series 
library(tidyverse)

## read in population time series data + cost distance estimates 
lpi <- read.csv("data-processed/lpi-subset-with-cost.csv")
dists <- read.csv("data-processed/pop-distance-key-with-cost.csv")
lpi_long <- read.csv("data-processed/lpi-subset-long-with-cost.csv")

## loop through pairs
i = 1
while(i <= nrow(dists)) {
  
  ## get pair 
  pair <- dists[i,]
  
  ## get time series for each population 
  ts1 <- lpi[which(lpi$pop_id == pair$pop_1),] %>%
    select(contains("X")) %>%
    gather(key = "year", value = "pop1_size", contains("X")) %>%
    mutate(year = str_split_fixed(year, "X", 2)[,2]) 
  
  ts2 <- lpi[which(lpi$pop_id == pair$pop_2),] %>%
    select(contains("X")) %>%
    gather(key = "year", value = "pop2_size", contains("X")) %>%
    mutate(year = str_split_fixed(year, "X", 2)[,2]) 
  
  ## join them 
  ts <- left_join(ts1, ts2, by = "year") 
  
  ## get rid of missing observations 
  ts <- ts[ts$pop1_size != "NULL" & ts$pop2_size != "NULL", ]
  
  ## convert to numeric 
  ts$pop1_size = as.numeric(as.character(ts$pop1_size))
  ts$pop2_size = as.numeric(as.character(ts$pop2_size))
  ts$year = as.numeric(as.character(ts$year))
  
  ## plot them 
  # ts %>%
  #   ggplot(aes(x = year, y = pop1_size)) +
  #   geom_point(colour = "red") + 
  #   geom_point(aes(y = pop2_size), colour = "blue")
  
  ## measure Pearson correlation
  correlation <- cor(ts$pop1_size, ts$pop2_size, method = 'pearson')
  
  ## save
  if(i == 1) {
    results <- data.frame(pop_1 = pair$pop_1, 
                          pop_2 = pair$pop_2,
                          pearson_corr = correlation, 
                          n_obs = nrow(ts))
  }
  else {
    results = rbind(results, 
                    data.frame(pop_1 = pair$pop_1, 
                               pop_2 = pair$pop_2,
                               pearson_corr = correlation, 
                               n_obs = nrow(ts)))
  }

  print(paste0("done population set # ", i))
  i = i + 1
}

## left join
results <- left_join(dists, results)

## save 
write.csv(results, "data-processed/synchrony-results.csv", row.names = FALSE)

results <- read.csv("data-processed/synchrony-results.csv")

## plot
results %>%
  filter(dist < 600) %>%
  filter(n_obs > 10) %>%
  ggplot(aes(x = dist, y = pearson_corr, colour = cost)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = dist, y = pearson_corr),
              colour = "black") +
  theme() +
  scale_colour_viridis_b()

results %>%
  filter(dist < 600) %>%
  filter(n_obs > 10) %>%
  ggplot(aes(x = dist, y = cost, colour = dist)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = dist, y = cost), colour = "black") +
  theme() +
  scale_colour_viridis_b()

results %>%
  filter(dist < 600) %>%
  filter(n_obs > 10) %>%
  ggplot(aes(x = cost, y = pearson_corr, colour = dist)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = cost, y = pearson_corr), colour = 'black') +
  theme() +
  scale_colour_viridis_b()

## plot two with high synchrony
long <- read.csv("data-processed/lpi-subset-long.csv")

which(results$pearson_corr == max(results$pearson_corr, na.rm = T))

results$pop_1[94]
results$pop_2[94]

long %>%
  filter(pop_id %in% c("Hirundo_rustica-11", "Hirundo_rustica-6")) %>%
  ggplot(aes(x = year, y = pop_size, colour = pop_id)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Population size") +
  theme(legend.position = "none")
  
ggsave(path = "figures", filename = "pop-time-series_high-synchrony.png", width = 4, height = 2)

long %>%
  filter(pop_id %in% c("Numenius_americanus-3","Numenius_americanus-1")) %>%
  ggplot(aes(x = year, y = pop_size, colour = pop_id)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Population size") +
  theme(legend.position = "none")

ggsave(path = "figures", filename = "pop-time-series_low-synchrony.png", width = 4, height = 2)

## and low synchrony


