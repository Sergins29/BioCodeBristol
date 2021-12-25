#fixed effect: things you measure 
#random effect: things that may change but you can't measure
#sometimes, random effects can be used as fixed effects:
#example: collect data on species abundance over 10 years in 6 sites
#The sites can be a random effect, where we measure the abundance over time
#and add more sites for more data, or the sites can be a fixed effect where we check
#the difference of abundance in each site

library("devtools")
library("tidyverse")
library("vroom")
library("nloptr")
library("glmmTMB")



pop_1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
pop_2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv")


pops_long <- full_join(pop_1, pop_2) %>%
  
  pivot_longer(cols = -c(species,
                         primary_threat,
                         secondary_threat,
                         tertiary_threat),
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = T,
               values_to = "abundance")

pops_long

#add number of threats per species
pops_long <- pops_long %>%
  rowwise %>%
  mutate(n.threats = length(which(!is.na(c(primary_threat,
                                           secondary_threat,
                                           tertiary_threat)))))

pops_long <- pops_long %>% 
  group_by(species, population) %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(date),
                                                 min(as.Date(date)),
                                                 units = "weeks")))

print(pops_long[,c("species", "population", "abundance", "standardised_time")], 10)

#visualize data
ggplot(pops_long, aes(x = standardised_time,
                      y = abundance)) +
  #add the points
  #Can use group function to specify that we want to treat as separate data for the sake of plotting
  #interaction() allows us to specify multiple levels to these data

  geom_line(aes(group = interaction(species, population))) +
  facet_wrap(~n.threats) +
  theme_minimal() +
  #fit linear regression to data
  geom_smooth(method = "lm")

#if the grouping of species and population occurs in ggplot rather than geom_line, this happens:
ggplot(pops_long, aes(x = standardised_time,
                     y = abundance,
                     group = interaction(species, population))) + 
  geom_line() + 
  facet_wrap(~n.threats) +
  theme_minimal() +
  geom_smooth(method = "lm")

#set a random intercept model
m_mod1 <- glmmTMB(abundance ~
                    standardised_time +
                    n.threats +
                    standardised_time:n.threats +
                    (1|species),
                  data = pops_long,
                  family = "gaussian")

summary(m_mod1)

random_eff_var = 9602 / (9602+4447)
random_eff_var

#set a nested model where we nest population inside species
m_mod2 <- glmmTMB(abundance ~ standardised_time + 
                    n.threats +
                    standardised_time:n.threats +
                    (1|species/population),
                  data = pops_long,
                  family = "gaussian",
                  control=glmmTMBControl(optimizer = optim, optArgs=list(method="BFGS")))


summary(m_mod2)

(8.988e+03 + 5.554e-93) / (8.988e+03 + 5.554e-93 + 3.755e+03)

#Lastly, can also specify crossed random effects as follows:
m_mod3 <- glmmTMB(abundance ~ standardised_time +
                    n.threats +
                    (1|population:species) +
                    (1|species),
                  data = pops_long,
                  family = "gaussian",
                  control=glmmTMBControl(optimizer = optim, optArgs=list(method="BFGS")))

summary(m_mod3) 

#however, in this case, nested model design is better as there isn't any variable which would 
#suggest a crossed design


#DHARMa package helps assess the model fit more robustly, simulates residuals which incorporate uncertainty in model
library(DHARMa)

m_mod2_sim <- simulateResiduals(m_mod2, n = 1000)
plot(m_mod2_sim)

#tests show the model does not fit the data very well
#firstly, since some species go extinct we have many zeros in the abundance column
#should remove some of these zeros for better predictions
#to remove all zeros except the first one (to show when they went extinct):
single_zero <- function(x, group_info){
  if(nrow(x)>0){
    x <- x[ order(x$standardised_time) ,]
    if( min(x$abundance) == 0){
      return( x[1:min(which(x$abundance == 0)),])
    }else{return(as_tibble(x))}
  }
}

species_single0 <- pops_long %>%
  group_by(species, population) %>%
  #group_map allows us to apply function to these groups 
  group_map(single_zero, .keep = T) %>%
  #.keep = T keeps the grouping variables
  #now, we bind it all together to a tibble (otherwise returns a list of group data)
  bind_rows()

#Must find which count data distribution type (poisson, ggeneralised poisson, binomial, etc) fits best
#to do this:
library(fitdistrplus)
fit_pois <- fitdist(species_single0$abundance,
                    distr = "pois")
plot(fit_pois)
#red area shows expected distribution if of type poisson, black area shows actual distribution
#hence does not fit

#look at summary of statistics
gofstat(fit_pois)
#p_value shows significant difference between observed dist and expected dist, meaning poisson is a bad fit

#now can try binomial fit:
fit_nbinom <- fitdist(species_single0$abundance,
                      dist = "nbinom")

plot(fit_nbinom)
#looks a lot better, can check p value:
gofstat(fit_nbinom)
#p_value still pretty low (p<0.05) meaning significant difference
#however, suggests its better than poisson

#Scaling data:
#when continuous predictor variables are on very different scales, we scale data
#threats between 0 and 3, standardised time between 0 and 991 (big difference)
#fit model with scaled params:

ps_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1 | species/population),
                  data = species_single0,
                  family = "poisson")

summary(ps_mod)

#can use r-squared calculations to roughly estimate goodness of fit:
r2.corr.mer <- function(m) {
  lmfit <- lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}
r2.corr.mer(ps_mod)

#other way of calculating R2:
MuMIn::r.squaredGLMM(ps_mod)

#can also use a function to look at overdispersion in the model:
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(ps_mod)
#when the chisq value is bigger than the rdf value, data considered overdispersed (when ratio is >1)
#in this case, data is clearly overdispersed

#can update() model structure to foit some other potential models without typing out full model everyt ime:
nb1_mod <- update(ps_mod, family = "nbinom1")
nb2_mod <- update(ps_mod, family="nbinom2")

#looking at our plotted data, number of threats may be the key driver of the number of zeros
#this makes sense due to more threats = more likely to go extinct
#Can thus specify threats as potential driver of zeros in our data for a zero inflated version of NB distributions:
Zi_nb1_mod <- update(nb1_mod, ziformula = ~n.threats)
Zi_nb2_mod <- update(nb2_mod, ziformula = ~n.threats)

#can also look at zero inflated version of poisson model:
Zi_ps_mod <- update(ps_mod, ziformula = ~n.threats)

#can now use anova analysis to compare the model fits of these three models
anova(ps_mod,
      Zi_ps_mod,
      Zi_nb1_mod,
      Zi_nb2_mod,
      nb1_mod,
      nb2_mod)
#check for lowest AIC values and good r-squared values (closer to 1), r2 can be calculated using function from earlier:
r2.corr.mer(Zi_nb1_mod)
#so r2 is good and AIC is low, making Zi_nb1_mod best fit
#MuMIn function used earlier to calculate r2 doesn't work with zero inflated version yet

#might consider correlations structures in data
#time series data: value of any given time point relies on value of previous time point
#might thus consider autocorrelation:
Zi_nb1_ar1_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1|species/population) + ar1(factor(scale(standardised_time))-1|species/population), 
                          data = species_single0,
                          ziformula=~n.threats,
                          family="nbinom1",
                          ##the control parameter specifying the optimizer to use:
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
#ar1 needs to be introduced as a factor (for some unknown reason)   

#to fix model convergence problem:
vignette("troubleshooting")
#We can look at the fixed effects of the model and assess what the problem is by using this with the troubleshooting
fixef(Zi_nb1_ar1_mod)

#Looking at the troubleshooting, reasons for warning are:
#1 = when a model is overparameterized (i.e. the data does not contain enough information to estimate the parameters reliably)

#2 = when a random-effect variance is estimated to be zero, or random-effect terms are estimated to be perfectly correlated (“singular fit”: often caused by having too many levels of the random-effect grouping variable)

#3 = when zero-inflation is estimated to be near zero (a strongly negative zero-inflation parameter)
# In our case, zero inflation parameters are not near zero

#when dispersion is estimated to be near zero
#In our case, no dispersion parameter

#when complete separation occurs in a binomial model: some categories in the model contain proportions that are either all 0 or all 1
#In our case, we aren't fitting a binomial model

#this leaves us with options 1 or 2

#look at model:
Zi_nb1_mod

#random effect of species quite small, so could be removed from model with autocorrelation (this would solve if the problem is n.2):
Zi_nb1_ar1_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1|population) + ar1(factor(scale(standardised_time)) -1|population),
                          data = species_single0,
                          ziformula = ~n.threats,
                          family = "nbinom1",
                          control = glmmTMBControl(optimizer = optim, optArgs=list(method="BFGS")))

#we still get a warning, meaning the model is probably over parametised (problem 1 in troubleshooting)
Zi_nb1_mod_sim <- simulateResiduals(Zi_nb1_mod, n=1000)
plot(Zi_nb1_mod_sim)
#QQ plot looks normal, with no significant values for any statistical tests (not shown here for some reason)
#predictions vs residuals should have straight red lines following the dotted ones, but it doesn't
#Could ideally try other models

#Other DHARMa tests:

#tests to see where there are outliers, in our case not significant so we dont need to worry
testOutliers(Zi_nb1_mod_sim,
             plot = TRUE)

#tests if simulated dispersion equal to observed dispersion (again not significant so no worry)
testDispersion(Zi_nb1_mod_sim,
               plot = TRUE)

#Can also compare dist of expected zeros in data against observed zeros
testZeroInflation(Zi_nb1_mod_sim,
                  plot = TRUE)
#In this case, deviation may be significant and our inflation parameter (n.threats) may have to be tweaked

#see if there is temporal autocorrelation in residuals, again not significant so the autocorrelation model wasn't needed earlier
testTemporalAutocorrelation(Zi_nb1_mod_sim,
                            time = species_single0$standarised_time,
                            plot = TRUE)

#Final check on how well model works, do this by comparing model predictions to observed values
species_single0$predicted <- predict(Zi_nb1_mod,
                                     data = species_single0,
                                     type = "response")

ggplot(species_single0, aes(x = abundance,
                            y = predicted)) +
  geom_point(col="grey") +
  geom_abline(slope = 1) +
  theme_minimal() +
  xlab("Observed") +
  ylab("Predicted")

#Model does a pretty good job of points being close to 1:1 line

summary(Zi_nb1_mod)
#Our model says:
##On average, as time increases abundance declines
##As the number of threats increase, the average population size decreases
##The interaction states that as those species with more threats decline faster through time than those with fewer threats

#To visualize the model all together (adapted from glmmTMB paper appendix):

#make new dataset
new_data <- unique(species_single0[,c("n.threats",
                                      "standardised_time",
                                      "species",
                                      "population")])

#scale relevant column
new_data$n.threats<-scale(new_data$n.threats)
new_data$standardised_time<-scale(new_data$standardised_time)

#set random effects of model to zero
X_cond <- model.matrix(lme4::nobars(formula(Zi_nb1_mod)[-2]), new_data)
beta_cond <- fixef(Zi_nb1_mod)$cond
pred_cond <- X_cond %*% beta_cond
ziformula <- Zi_nb1_mod$modelInfo$allForm$ziformula
X_zi <- model.matrix(lme4::nobars(ziformula), new_data)
beta_zi <- fixef(Zi_nb1_mod)$zi
pred_zi <- X_zi %*% beta_zi

#tranform point estimates of unconditional counts to response scale and multiply
pred_ucount = exp(pred_cond)*(1-plogis(pred_zi))

#load MASS library
library(MASS)

#set random number generator seed
set.seed(101)

#Use posterior predictive simulations to generate upper and lower confidence intervals and median predicted counts

##conditional
pred_condpar_psim = mvrnorm(1000,mu=beta_cond,Sigma=vcov(Zi_nb1_mod)$cond)
pred_cond_psim = X_cond %*% t(pred_condpar_psim)

##zero inflation parameter
pred_zipar_psim = mvrnorm(1000,mu=beta_zi,Sigma=vcov(Zi_nb1_mod)$zi)
pred_zi_psim = X_zi %*% t(pred_zipar_psim)

##transform them
pred_ucount_psim = exp(pred_cond_psim)*(1-plogis(pred_zi_psim))

##calculate 95% CIs
ci_ucount = t(apply(pred_ucount_psim,1,quantile,c(0.025,0.975)))
ci_ucount = data.frame(ci_ucount)

##rename
names(ci_ucount) = c("ucount_low","ucount_high")

##put into a data frame
pred_ucount = data.frame(new_data, 
                         pred_ucount, 
                         ci_ucount)

##we need to reverse the scaling of our predictor variables for our plots to make sense
##the scale() function stores attributes of the scaling in the vectors of scaled data 
## try running new_data$n.threats and looking at the bottom values
##write a function to do this:
unscale <- function(x){
  x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
}

##unscale the variables
pred_ucount$n.threats_unscaled <- unscale(pred_ucount$n.threats)
pred_ucount$standardised_time_unscaled <- unscale(pred_ucount$standardised_time)

##load the viridis package (colourblind friendly palletes)
library(viridis)

##plot out the predicted median values for abundance
## in response to time (x-axis)
##and grouped by the number of threats
ggplot(pred_ucount, aes(x = standardised_time_unscaled, 
                        y = pred_ucount, 
                        group = n.threats_unscaled, 
                        col = n.threats_unscaled))+ 
  ##median lines for each number of threats
  geom_line() +
  ##add in a geom_ribbon to show the 95% CI
  geom_ribbon(aes(ymin = ucount_low,
                  ymax = ucount_high), 
              alpha = 0.1, 
              col = "grey", 
              linetype=0) +
  ##minimal theme
  theme_minimal() +
  ##set x and y axes labels
  ylab("Predicted\nabundance") + xlab("Time\n(weeks)") +
  ##viridis colour pallette for continuous data
  scale_colour_viridis_c() +
  ##move legend to the top
  theme(legend.position = "top") +
  ##rename the legend
  labs(colour="Number of threats")
