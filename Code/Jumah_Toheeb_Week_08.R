#Week 08 - MC1-M231 - Programming, Data Science and Statistics 3

#Objective: Exploring a climate data set of 36 stations in France

#####################
######Exercise 1####
#####################

#loading the necessary library needed for analysis
library(tidyverse)
library(broom)

#Loading the dataset
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")

#Inspecting the structure of the dataset
str(clim)

#Exploring the content of the dataset using view function from tidyverse
view(clim)

#Fixing the issues with altitude and p_mean precipitation
clim$altitude <- as.numeric(gsub(",","",clim$altitude))
clim$p_mean <- as.numeric(gsub(",","",clim$p_mean))

#Inspecting the new structure of the dataset
str(clim)

#Rechecking the content of the dataset using view function from tidyverse, to confirm the comma have been removed
view(clim)

#Checking for the presence of NA
anyNA(clim)

##########################################################################
##Plotting the Map of France of France to Understand the Station Location#
##########################################################################

#Installing the necessary packages
install.packages('maps')

#Loading the necessary libraries to plot the map
library(maps)        # For France map data
library(ggplot2)  

#The "maps" library was use to get the map of the France, based on the 52 divisions that from the 13 administrative states. The ggplots library was used to plot the locations of the various stations on the France map. 

#Extracting France map data
france_map <- map_data("world", region = "France")

france_states <- map_data("france")

#Plotting the France map with the climate stations using ggplot
ggplot() +
  # Plot the France map
  geom_polygon(
    data = france_states,
    aes(x = long, y = lat, group = group),
    color = "grey10",
    fill = "#fff7bc"
  ) +
  
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    color = "blue",
    alpha = 0.7,
    size = 2
  ) +
  
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Climate Stations") 

########################
###Fitting the Model####
########################

#Excluding the two high mountain extremes (35 and 36)
new_clim <- clim[1:34,]

#Exploring the content of the new dataset using view function from tidyverse
view(new_clim)

# Fit the multiple linear regression model
clim_model <- lm(t_mean ~ altitude + lat + lon, data = new_clim)

#Display the summary of the model
summary(clim_model)

#Extracting the coefficients of the model
tidy(clim_model) %>% select(term, estimate)

#The multiple linear regression model estimates the mean temperature (t_mean) in Celsius with an intercept of 37.27°C, indicating the expected temperature when altitude, latitude, and longitude are zero. The coefficient for altitude is -0.0064, suggesting that each unit increase in altitude decreases temperature by approximately 0.0064°C, while latitude has a more substantial negative effect of -0.534, indicating a decrease of 0.534°C per unit increase. Longitude's coefficient is 0.0321 but is not statistically significant. The estimated coefficients for altitude and latitude are statistically significant. The model was also able to explains 83.29% of the variance in the mean temperature (Multiple R-squared) and 81.62% when adjusted for predictors (Adjusted R-squared), indicating a strong fit.


##########################
####Excercise 2###########
##########################

#Fitting a new model while excluding the non-significant variables
clim_model2 <- lm(t_mean ~ altitude + lat, data = new_clim)

#Display the summary of the second model without non-significant variables
summary(clim_model2)

#Extracting the coefficients of the second model without non-significant variables
tidy(clim_model2) %>% select(term, estimate)

#The multiple linear regression results indicate that the mean temperature (t_mean) is significantly influenced by both altitude and latitude. The intercept of 37.91 suggests that when both altitude and latitude are zero, the expected mean temperature is 37.91°C. The coefficient for altitude is -0.0063, meaning that for each unit increase in altitude, the mean temperature decreases by approximately 0.0063°C. The latitude coefficient is -0.5465, indicating that for each unit increase in latitude, the mean temperature decreases by about 0.5465°C. Both predictors are statistically significant, with p-values less than 0.001. The model explains 82.92% of the variance in mean temperature (Multiple R-squared) and 81.82% when adjusted for the number of predictors (Adjusted R-squared), demonstrating a strong fit to the data.

#Creating a dataframe to predict the temperature of Mont-Ventoux and Pic-du-midi using the data that was provided as their altitude and latitude in the dataset
stations_to_predict <- data.frame(
  altitude = c(1212, 2860),
  lat = c(44.16, 42.93)
)

#Predicting the mean temperature for the new stations
predictions <- predict(clim_model2, newdata = stations_to_predict, interval = "confidence")

#Printing the predictions with its confidence intervals
predictions

#The output of the predictions shows that, predicted mean temperature for Mont-Ventoux (altitude: 1212, latitude: 44.16) is 6.19 degrees Celsius with a 95% confidence interval between 4.33 and 8.04 degrees Celsius. And for Pic-du-midi (altitude: 2860, latitude: 42.93), the predicted mean temperature is -3.46 degrees Celsius with a 95% confidence interval between -8.13 and 1.21 degrees Celsius

#####################
### Exercise 3#######
#####################

#Evaluating the Model using 3D scatter plot

#Installing the  necessary Package to plot the 3D scatter plot
install.packages("scatterplot3d")

#Loading the scatterplot3d library
library(scatterplot3d)

#Plotting the 3D scatter plot
scatterplot3d(clim$altitude, clim$lat, clim$t_mean,
              main = "3D Scatter Plot of Temperature",
              xlab = "Altitude", ylab = "Latitude", zlab = "Mean Temperature",
              pch = 19, color = "blue")

scatter_3d <- with(clim, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(clim_model2)

#The 3D scatter plot helps to visualize the relationship between three variables of latitude, altitude, and mean temperature. The latitude point ranges from roughly 42 to 52, with higher latitudes clustered around 48, the altitude ranges from 0 to 1500, with most data points concentrated below 1000 while the mean Temperature ranges from 0 to 20. The plot shows that higher mean temperature is associated with lower altitudes and latitudes and vice versa. The scatter plot shows a general trend where  as the altitudes and latitudes increases the mean temperatures decreases.

################################################
#Evaluating the Models using the summary output#
################################################

#The two multiple linear regression model estimates the mean temperature (t_mean) using altitude, latitude and longitude as predictors, with a difference from the first model that included longitude, the second model did not include longitude because its coefficients is not statistically significant. The intercept of the second model prediction for the mean temperature is 37.91 which is significantly higher than the first model of 37.27°C. The coefficient of altitude for every meters (m) or feet (ft) above sea level and latitude in both models decrease the temperature, for every unit increase in altitude and latitude. This established that the higher the altitude the colder the temperature becomes and also for every distance we travel away from the Equator at 90° towards both the North and South pole, the colder the temperature becomes.