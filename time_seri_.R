#### load packages
library(readr)
library(sp)
library(raster)
library(knitr)
library(rgdal)
library(rgeos)
library(foreach) 
library(doParallel)
library(tidyverse)
library(caret)
library(ggplot2)
library(automap)
library(gstat)

TAS090 <- read_delim("N:/Jose/Buoy data/TAS090.txt", 
                     "\t", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)

##### subset data for the experiment

TAS090 = TAS090[1:1000,]

# ggplot(TAS090, aes(x = TAS090$X1, y = TAS090$X2)) + geom_line()

TAS090$X1 = TAS090$X1
TAS090$X0 = 1

#### split sets 

set.seed(99)
test_set = TAS090[sample(nrow(TAS090), 100),]
training_set = TAS090[-sample(nrow(TAS090), 100),]

coordinates(training_set) = c('X1', 'X0')

coordinates(test_set) = c('X1', 'X0')

# create grd 

grd = data.frame(X1 = seq(training_set@bbox[1,1], training_set@bbox[1,2], by = 0.01),
                 X0 = 1)

coordinates(grd) = c('X1', 'X0')

# gs = gstat(formula = X2~1, locations = training_set)

# idw <- interpolate(r, gs)

start_time <- Sys.time()

test_kri <- autoKrige(X2~1, training_set, grd, maxdist= 10)

# kr.cv = autoKrige.cv(X2~1, training_set, model = c("Ste"), nfold = 10)

end_time <- Sys.time()
end_time - start_time

test_map = test_kri$krige_output
gridded(test_map) = TRUE
test_map_ras = raster(test_map)

# error_interpo = postResample(pred = raster::extract(test_map_ras, test_set), obs = test_set$X2)

interpo_df = data.frame(times_coord = test_set@coords, X2 = raster::extract(test_map_ras, test_set), label = "interpolated")
combine_df = rbind(data.frame(times_coord = training_set@coords, X2 = training_set$X2, label = "Original"), interpo_df)


############### extrapolation: time series forecasting using xgboost #############

# dates = as.Date(as.character(TAS090$X1), "%Y%m%d") ### not ensure what the format is for the original time stamps

TAS090$whole  = floor(TAS090$X1)
TAS090$dec = TAS090$X1 - floor(TAS090$X1)

# set training method 
fit_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# xgb_grid = expand.grid(nrounds = 200, max_depth = c(5, 10, 15), eta = 0.4, gamma = 0, colsample_bytree = c(0.5, 0.8), min_child_weight = 1, subsample = 1)

set.seed(29)
start_time <- Sys.time() # measure running time

fit_xg <- train(
  
  x = TAS090 %>% select(-c(X1, X2, X0)),
  y = TAS090$X2,
  
  method = "xgbTree",
  trControl = fit_control
)

end_time <- Sys.time() # measure running time
time_xg = end_time - start_time # 

############### create unseen set for testing ################

unseen = read_delim("N:/Jose/Buoy data/TAS090.txt", 
           "\t", escape_double = FALSE, col_names = FALSE, 
           trim_ws = TRUE)[1001:1100,]

unseen$whole  = floor(unseen$X1)
unseen$dec = unseen$X1 - floor(unseen$X1)
unseen_test = unseen %>% select(-c(X1, X2))

pred_xg <- predict(fit_xg, unseen_test)
error_xg = postResample(pred = pred_xg, obs = unseen$X2)

combine_df_v2 = rbind(combine_df, data.frame(times_coord.X1 = unseen$X1, X2 = pred_xg, label = "extrapolated", times_coord.X0 = 1))
ggplot(data = combine_df_v2, aes(x = times_coord.X1, y = X2, colour = label)) + geom_point()

ggplot(data = rbind(data.frame(X1=unseen$X1,X2=pred_xg,label = 'Extrapolated'),data.frame(X1=unseen$X1,X2=unseen$X2,label='Original')), 
       aes(x = X1, y = X2, colour = label)) + geom_line()
