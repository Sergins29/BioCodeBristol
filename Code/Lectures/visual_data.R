library(tidyverse)
library(vroom)
library(devtools)
library(multcomp)
data("iris")
iris
iris["Sepal.Width"]
#Jittered data = artificially added variance to the data on x-axis to see points more easily
ggplot(data = iris, aes(x = Species, y = Sepal.Width)) + geom_jitter(aes(col = Species)) + theme_bw()
ggplot(data = iris, aes(x = Sepal.Width, fill = Species)) + geom_histogram(binwidth = .1, alpha = .5, position = "identity")
mod_iris <- glm(Sepal.Width ~ Species,
                data = iris,
                family = "gaussian")
class(mod_iris)
plot(mod_iris)
summary(mod_iris)
3.428 + -0.658
3.428 + -0.454

summary(glht(mod_iris, mcp(Species = "Tukey")))

#load in population data

pop_1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
pop_2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv")

#add population column to both sets of data
pop_1$Population=1
pop_2$Population =2

#merge into big df
pops <- merge(pop_1,pop_2, all = TRUE, na.rm = TRUE)

#pivot df to long format
pops_long <- pops %>%
  
  pivot_longer(cols = -c(species,
                         primary_threat,
                         secondary_threat,
                         tertiary_threat,
                         Population),
               names_to = "Date",
               values_to = "Pop_count")

pops_long
#fix date column and set values as class "Date"
new_data <- gsub(".*_", "", pops_long$Date)
pops_long$Date <- as.Date(new_data)
pops_long
class(pops_long$Date)
#filter by only one species
single_spp <- pops_long %>%
  filter(species == "Trichocolea tomentella")

#remove rows with no pop data
single_spp <- single_spp %>% drop_na(Pop_count)

#plot showing decline of species population throughout 15 years
p1 <- ggplot(single_spp, aes(x=Date, y=Pop_count)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Pop count") +
  xlab("Year")
  
p1 + geom_smooth(method = "loess")

#new column (standardised time) which is the difference between starting date
#of time series and each other date in weeks
single_spp <- single_spp %>%
  mutate(standardised_time = as.numeric(difftime(Date,
                                                 min(Date),
                                                 units = "weeks")))
print(single_spp[,c("Pop_count", "Date", "standardised_time")])

#fit this column in our glm (before the ~ is our y (dependent) and after the ~ is our x (independent))
mod1 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = "gaussian")

#find the predicted y values for each x value from the model and add them to a column in df
single_spp$pred_gaussian <- predict(mod1,
                                    type="response",
                                    )

#find the residuals
single_spp$resid_gaussian <- resid(mod1)

#plot data
p2 <- ggplot(single_spp, aes(x = standardised_time,
                             y = Pop_count)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Pop_count") +
  xlab("standardised_time")

#add in line of predicted values from model
p2 <- p2 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian),
                     col = "dodgerblue",
                     size = 1)

#Can also add vertical blue lines to show residual error
#without prompts, ggplot assumes we want start of lines from x and y valuess
#just need to specify end points:
p2 <- p2 + 
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian),
                   col = "lightblue")

#finally, we add a title and print the plot:
p2 <- p2 + ggtitle("Fitted Model (Gaussian with Identity Link)")

#Now let's check the residuals:
p3 <- ggplot(single_spp, aes(x = resid_gaussian)) +
  geom_histogram(fill="goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals (gaussian with identity link)")

#Plot the predicted values vs the residuals:
p4 <- ggplot(single_spp, aes(x=pred_gaussian, y=resid_gaussian)) + 
  geom_point() +
  theme_minimal() +
  ylab("Residuals") +
  xlab("Predicted Gaussian") +
  ggtitle("Predicted vs Residual (Gaussian with Identity Link)")

p4 + geom_smooth(fill = "lightblue",
                 col = "dodgerblue")

#plot the qq plot for residuals from the model assuming a normal dist
#and add straight line the points fall along
qqnorm(single_spp$resid_gaussian); qqline(single_spp$resid_gaussian)

#Lets find better alternative models to fit this data:


#fit glm with poisson distribution
mod2 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = "poisson")
#Other option due to skewness seen in qqnorm: gaussian glm with log link
mod3 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "log"))

#Or, a gaussian model with an inverse link
mod4 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "inverse"))

#Can now compare these 4 models to the data using "Corrected Akaike information criterion (AICc)".

library(gamlr)

#ccompare models
AIC_mods <- data.frame(model = c("mod1", "mod2", "mod3", "mod4"),
                       AICc = c(AICc(mod1), AICc(mod2), AICc(mod3), AICc(mod4)))

#rank models by AIC using order function
AIC_mods[order(AIC_mods$AICc),]

#Knowing mod3 fits better than the other ones, we now must find out if it actually fits well or just less badly
single_spp$pred_gaussian <- predict(mod2,
                                    type="response",
)

#find the residuals
single_spp$resid_gaussian <- resid(mod2)

#plot data
p2 <- ggplot(single_spp, aes(x = standardised_time,
                             y = Pop_count)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Pop_count") +
  xlab("standardised_time")

#add in line of predicted values from model
p2 <- p2 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian),
                     col = "dodgerblue",
                     size = 1)

#Can also add vertical blue lines to show residual error
#without prompts, ggplot assumes we want start of lines from x and y valuess
#just need to specify end points:
p2 <- p2 + 
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian),
               col = "lightblue")

#finally, we add a title and print the plot:
p2 <- p2 + ggtitle("Fitted Model (Gaussian with log Link)")

p2
plot(mod3)

