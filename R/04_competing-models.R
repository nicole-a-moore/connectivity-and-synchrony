## competing models 
library(tidyverse)

## read in the data 
data <- read.csv("data-processed/synchrony-results.csv")
lpi <- read.csv("data-processed/lpi-subset-with-cost.csv") 
coords <- read.csv("data-processed/pop-distance-key-with-cost.csv")

key = select(lpi, Class, Binomial) %>%
  unique()

## add group variable
data <- left_join(data, key)

## exclude populations whose time series have less than 5 observations
data <- data %>%
  filter(n_obs >= 5) 

## divide cost distance by 1000 to convert to km
data$cost = data$cost/1000

## plot the data
data %>%
  ggplot(aes(x = dist, y = pearson_corr, colour = cost)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = dist, y = pearson_corr), colour = "black") +
  theme() +
  scale_colour_viridis_b() +
  facet_wrap(~Class)

data %>%
  ggplot(aes(x = cost, y = pearson_corr, colour = dist)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = cost, y = pearson_corr), colour = "black") +
  theme() +
  scale_colour_viridis_b() +
  facet_wrap(~Class)

data %>%
  ggplot(aes(x = cost, y = pearson_corr, colour = dist)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = cost, y = pearson_corr, group = Binomial), 
              se = FALSE, size = 0.25, colour = "black") +
  theme() +
  scale_colour_viridis_b() +
  facet_wrap(~Class)

data %>%
  ggplot(aes(x = n_obs, y = pearson_corr, colour = dist)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = n_obs, y = pearson_corr, group = Class), colour = "black") +
  theme() +
  scale_colour_viridis_b() +
  facet_wrap(~Class)

data %>%
  ggplot(aes(x = dist, y = cost, colour = pearson_corr)) +
  geom_point() +
  geom_smooth(method = "lm", inherit.aes = FALSE,
              aes(x = dist, y = cost), colour = "black") +
  theme() +
  labs(x = "Linear distance between populations (km)",
       y = "Distance of least resistance path between populations (km)",
       colour = "Pearson\ncorrelation\ncoefficient")

ggsave(path = "figures", filename = "distance-vs-cost-distance.png", 
       width = 7, height = 4.5)

## fit model with distance as explanatory variable
mod_dist <- lm(data = data,
               pearson_corr ~ dist)

summary(mod_dist)

## fit model with cost as explanatory variable
mod_cost <- lm(data = data,
               pearson_corr ~ cost)

summary(mod_cost)

## compete the models using aic 
aic <- AIC(mod_dist, mod_cost)

aic$delta_AIC = aic$AIC - min(aic$AIC)

aic
## cost model is better!!!!! 

mod_dist_cost$coefficients
## synchrony decreases with least cost distance between the populations 

data %>%
  ggplot(aes(x = Class)) +
  geom_bar()
## almost all pops are are birds / mammals 


## try random effect of species - within species, do the patterns hold?
library(nlme)

lme_dist <- lme(data = data,
           pearson_corr ~ dist,
           random = ~1|Binomial)
summary(lme_dist)    
lme_cost <- lme(data = data,
                pearson_corr ~ cost,
                random = ~1|Binomial)
summary(lme_cost)    

    
## compete the models 
aic_lme <- AIC(lme_cost, lme_dist)
aic_lme$delta_AIC <- aic_lme$AIC - min(aic_lme$AIC)
aic_lme

## plot the predicts
newdata_dist <- data.frame(expand.grid(dist = seq(from = min(data$dist), to = max(data$dist), by = 0.1)))


pred <- AICcmodavg::predictSE.lme(lme_dist, newdata_dist, se.fit = TRUE, level = 0,
                                  print.matrix = FALSE)
newdata_dist$pred <- pred$fit
newdata_dist$pred_se <- pred$se.fit


newdata_cost <- data.frame(expand.grid(cost = seq(from = min(data$cost), to = max(data$cost), by = 5)))
pred <- AICcmodavg::predictSE.lme(lme_cost, newdata_cost, se.fit = TRUE, level = 0,
                                  print.matrix = FALSE)
newdata_cost$pred <- pred$fit
newdata_cost$pred_se <- pred$se.fit

## cost vs. correlation
data %>%
  ggplot(aes(x = cost, y = pearson_corr)) +
  geom_point() +
  theme() +
  geom_line(data = newdata_cost, 
            aes(x = cost, y = pred,),
            inherit.aes = FALSE) +
  geom_ribbon(data = newdata_cost, 
              aes(x = cost,
                  ymin = (pred-1.96*pred_se),
                  ymax = (pred+1.96*pred_se)),
              fill = "darkgrey",
              alpha = 0.25,
              colour = NA,
              inherit.aes = F) +
  labs(x = "Distance of least resistance path between populations (km)",
       y = "Pearson correlation coefficient",
       colour = "Taxonomic\ngroup")


ggsave(path = "figures", filename = "synchrony-vs-cost-distance_no-colour.png", 
       width = 6, height = 4)

data %>%
  ggplot(aes(x = cost, y = pearson_corr, colour = dist)) +
  geom_point() +
  theme() +
  geom_line(data = newdata_cost, 
            aes(x = cost, y = pred,),
            inherit.aes = FALSE) +
  geom_ribbon(data = newdata_cost, 
              aes(x = cost,
                  ymin = (pred-1.96*pred_se),
                  ymax = (pred+1.96*pred_se)),
              fill = "darkgrey",
              alpha = 0.25,
              colour = NA,
              inherit.aes = F) +
  labs(x = "Distance of least resistance path between populations (km)",
       y = "Pearson correlation coefficient",
       colour = "Linear\ndistance\nbetween\npopulations (km)")

ggsave(path = "figures", filename = "synchrony-vs-cost-distance_colour.png", 
       width = 7, height = 4)


## distance vs. correlation
data %>%
  ggplot(aes(x = dist, y = pearson_corr, colour = Class)) +
  geom_point() +
  theme() +
  geom_line(data = newdata_dist, 
            aes(x = dist, y = pred),
            inherit.aes = FALSE) +
  geom_ribbon(data = newdata_dist, 
              aes(x = dist,
                  ymin = (pred-1.96*pred_se),
                  ymax = (pred+1.96*pred_se)),
              fill = "darkgrey",
              alpha = 0.25,
              colour = NA,
              inherit.aes = F) +
  labs(x = "Linear distance between populations (km)",
       y = "Pearson correlation coefficient",
       colour = "Taxonomic\ngroup")

ggsave(path = "figures", filename = "synchrony-vs-distance.png", 
       width = 6, height = 4)



## plot map of final populations
ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(y = "Latitude", x = "Longitude") +
  geom_point(data = lpi, 
             aes(x = Longitude, y = Latitude, colour = Binomial),
             inherit.aes = FALSE, 
             size = 0.75) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) +
  geom_segment(data = data,
               aes(x = Longitude1, y = Latitude1, xend = Longitude2, yend = Latitude2, colour = Binomial), 
               inherit.aes = FALSE, linewidth = .4) 

ggsave(width = 5, height = 3, path = "figures", filename = "map-of-populations_final.png")


## predict:
## cost distance explains variation in synchrony better than linear distance between populations 
## cost distance + accumulated cost explain variation in synchronous between populations better than cost distance alone 

## plot examples








## next: 
## add n_obs to model?
## calculate accumulated cost? 
## update map
## make figures for presentation



accCost(matrix, fromCoords)
## do for each coord of pop_1, extract value at coord of pop_2

all.equal(rownames(comm), rownames(metadata))

metadata <- metadata[rownames(comm), ]




