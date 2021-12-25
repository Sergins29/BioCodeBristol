data("iris")
library(tidyverse)
library(multcomp)
library(vroom)
library(gamlr)

m#to visualize how normally distributed the sets of data (species in this case) are and 
#to visualise how overlapping they are, thus how likely they are to have statistically different means
ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = .1, alpha = .5, position = "identity")
iris

ggplot(iris, aes(x = Species, y = Sepal.Width, col = Species)) +
  geom_jitter()

#to test whether there is significant difference in the mean sepal widths of three species:
#code a glm:
#glm of sepal width as a function of species
mod_iris <- glm(Sepal.Width ~ Species,
                data = iris,
                family = "gaussian")
 
#continuous response (y) variable (sepal width) and categorial predictor (x) variable (species)

#to assess the fit of a model:
class(mod_iris)
plot(mod_iris)


#deviance residueals: close to zero mean neither good nor bad. Higher = bad fit, lower = good fit.
#null deviance: how well response variable is predicted by model that includes only intercept 
#(low null deviance: data modelled well using only intercept, should consider using few features for modeling data)
#residual deviance:low residual deviance implies model trained is appropriate
#so residual deviance = null deviance with the addition of other independent variables
#estimate shows how negatively or positively the x variables affect y. Pr shows the significance of the coefficients (below 0.05 is significant)

summary(mod_iris)

#R takes the first value of our predictor variable and uses it as comparison point (the intercept)
#so in this case, the intercept is species setosa
#other two values are coefficeients relative to mean of intercept 
#to find mean of other x values just have to add the coefficient to the intercept estimate

#not enough for significant proof

#multiple comparisons test: compare differences between all categories in predictor variable:
summary(glht(mod_iris, mcp(Species = "Tukey")))

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
               values_to = "Pop_count",
               values_drop_na = TRUE)
print(pops_long)

new_data <- gsub(".*_", "", pops_long$Date)
pops_long$Date <- as.Date(new_data)
pops_long

single_spp <- pops_long %>%
  filter(species == "Trichocolea tomentella")
single_spp

p1 <- ggplot(single_spp, aes(x = Date, y = Pop_count)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("Year")
p1 + geom_smooth(method="loess")

#easier to interpret date as numeric vector for predictor variable:
single_spp <- single_spp %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(Date),
                                                 min(as.Date(Date)),
                                                 units = "weeks")))
print(single_spp[,c("Pop_count", "Date", "standardised_time")])

#now, fit a glm:
mod1 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = "gaussian")
single_spp$pred_gaussian <- predict(mod1,
                                    type = "response")
single_spp$resid_gaussian <- resid(mod1)

p2 <- ggplot(single_spp, aes(x = standardised_time,
                             y = Pop_count)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Pop_count") +
  xlab("standardised_time")

#to add in predicted value line:
p2 <- p2 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian),
                     col = "dodgerblue",
                     size = 1)
#to add vertical lines showing residual errors (how far the observed points are from pred values):
#must also specify the x and y ends, as r would otherwise assume we want the start to be taken from x and y values we are plotting
p2 <- p2 + 
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian),
               col = "lightblue")

#add title:
p2 <- p2 + ggtitle("Fitted model (gaussian with identity link)")

p2

#to check residuals around the fitted values:
p3 <- ggplot(single_spp, aes(x = resid_gaussian)) +
  geom_histogram(fill = "goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals (gaussian with identity link)")
p3
#residuals aren't normally ditributed, concerning

p4 <- ggplot(single_spp, aes(x = pred_gaussian,
                             y = resid_gaussian)) +
  geom_point() +
  theme_minimal() +
  ylab("Residuals") +
  xlab("Predicted values") +
  geom_smooth(fill = "lightblue", col = "dodgerblue") +
  ggtitle("Predicted vs residuals (gaussian with identity link")
p4

#QQ plots are useful to interpret how well model fits data
#if the two distributions being compared are similar, the points will approximatelt lie on the line
qqnorm(single_spp$resid_gaussian); qqline(single_spp$resid_gaussian)
#points here deviate from expected line, not good

#for a better fit, we need to minimise the residual error in the above plots, less residual error = fits data better
#since the data is counts (pop count at each standardised time) poisson error distribution may be good:
mod2 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = "poisson")
#due to skewness, a log-link gaussian may work too:
mod3 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "log"))
#finally, can try a gaussian model with an inverse link:
mod4 <- glm(Pop_count ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "inverse"))
#normal AIC good to compare models with same distribution but different predictor variables for example
#in this case, distributions are different so we use corrected AIC (AICc)

AIC_mods <- data.frame(model = c("mod1", "mod2", "mod3", "mod4"),
                       AICc = c(AICc(mod1), AICc(mod2), AICc(mod3), AICc(mod4)))
AIC_mods[order(AIC_mods$AICc),]
#if their scores are within 2 AIC of each other, cnat be sure its the best mode. due to mod3 being 8 apart from second best fitting, we can assume mod3 is best
#now have to check whether mod3 actually fits well or the others just fit worse
single_spp$pred_gaussian_log <- predict(mod3,
                                    type = "response")
single_spp$resid_gaussian_log <- resid(mod3)

p5 <- ggplot(single_spp, aes(x = standardised_time,
                             y = Pop_count)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Pop_count") +
  xlab("standardised_time")

#to add in predicted value line:
p5 <- p5 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian_log),
                     col = "dodgerblue",
                     size = 1)
#to add vertical lines showing residual errors (how far the observed points are from pred values):
#must also specify the x and y ends, as r would otherwise assume we want the start to be taken from x and y values we are plotting
p5 <- p5 + 
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian_log),
               col = "lightblue")
p5 <- p5 + ggtitle("Fitted model (gaussian with log link)")
p5

plot(mod3)
#if any values are outside the 0.5 or 1 lines in plot 4, some data are exerting an overly strong effect on the predicted trends, should probs remove them
#gaussian not specifically formulated for count data
#assumes that error values continous, but we know thats not the case. Can however get away with it when mean dependant variable is high (>50)

summary(mod3)
#a decrease of about 0.003 individuals per week over time series
#can plot output of this model:
p6 <- ggplot(single_spp, aes(x = standardised_time, y = Pop_count)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("Standardised time")

p6 <- p6 + geom_smooth(data = single_spp,
                       method = "glm",
                       method.args = list(family = gaussian(link="log")),
                       formula = y ~ x,
                       col = "dodgerblue",
                       fill = "lightblue")
p6
