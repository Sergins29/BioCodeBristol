library(vroom)
library(tidyverse)
remotes::install_github("nset-ornl/wbstats")
library(wbstats)
library(countrycode)

#load covid data in
data <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")

#change names of first two headings
names(data)[1:2] <- c("Province.State", "Country.Region")

#format data to long format
covid_long <- data %>%
  
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")

#load in data from wbstats regarding population
pop_data <- wb_data(indicator = "SP.POP.TOTL",
                    start_date = 2002,
                    end_date = 2020)

pop_data <- as_tibble(pop_data)

#filter the data to include data just from year 2020
pop_2020 <- pop_data %>%
  
  filter(date == 2020)

pop_2020

covid_long

head(unique(pop_2020$country), 10); tail(unique(pop_2020$country), 10)

#the covid data splits countries by province while the pop data doesn't
data %>% filter(Country.Region == "Australia")
pop_2020 %>% filter(country == "Australia")

#must thus first join the provinces together in covid data
covid_country <- covid_long %>%
  #number of deaths in each country at each date
  group_by(Country.Region, Date) %>%
  #sum the death column in these groups
  summarise(Deaths = sum(Deaths))

covid_country

#every country in R has a "country code" which can be used to simplify data throughout different datasets
covid_country$code <- countrycode(covid_country$Country.Region,
                                  origin = "country.name" , 
                                  destination = "iso3c")

#left_join() returns all rows from x and all columns from x and y
#to join the two dataframes, we shall use the country codes since we have them in both frames
#Will also use select() function to pick up only the wanted columns 
#these columns are: iso3c codes and value columns
names(pop_2020)[5] <- "value"

covid_w_pop <- left_join(covid_country,
                         pop_2020 %>% select(iso3c, value),
                         #by selects the variable that we join the dataframes by
                         by = c("code" = "iso3c"))

covid_w_pop

#to figure out in what position certain column names are:
which(names(covid_w_pop) == "value")
#to change the name without specifying the column name:
names(covid_w_pop)[which(names(covid_w_pop) == "value")] <- "Population"

#visual checks by filter:
covid_w_pop %>% filter(Country.Region=="Afghanistan" & Date == "1/22/20") 
pop_2020 %>% filter(country == "Afghanistan")
#population values check, thus the join worked
max(covid_w_pop$Date)

recent <- covid_country %>% 
  
  filter(Date == max(covid_country$Date))

sum(recent$Deaths)

#make new dataframe of global deaths and use group by and summarise
#this is to find the global deaths per day:
global_deaths <- covid_country %>%
  group_by(Date) %>%
  summarise("Global.deaths" = sum(Deaths))

global_deaths

#to check for any NA values:
which(is.na(global_deaths$Global.deaths))
#integer(0) means there are no NA values

#to remove any NAs in the data frame:
global_deaths <- covid_country %>%
  group_by(Date) %>%
  summarise("Global.deaths" = sum(Deaths, na.rm=T))

global_deaths

#Deaths per million:
covid_w_pop$DPM <- covid_w_pop$Deaths/(covid_w_pop$Population / 1000000)
#test that it works:
covid_w_pop %>% filter(Date == max(covid_w_pop$Date))

#This won't work as R will treat the dates as a random number:
ggplot(data = global_deaths, aes(x = Date, y = Global.deaths)) +
  geom_point()

#to order the dates in the plot:
global_deaths$Date.corrected <- as.Date(global_deaths$Date, format = "%m/%d/%y")

ggplot(data = global_deaths, aes(x = Date.corrected, y = Global.deaths)) + geom_point() 

#can also plot as a line rather than points:
ggplot(data = global_deaths, aes(x = Date.corrected, y = Global.deaths)) + geom_line() 
#or can plot both by just adding another +:
ggplot(data = global_deaths, aes(x = Date.corrected, y = Global.deaths)) + 
  geom_point(col = "darkgrey") +
  geom_line(col = "red")

#many other geom options online

#can split ggplot arguments across multiple statements:
p1 <- ggplot(data = global_deaths, aes(x = Date.corrected, y = Global.deaths))
p1 <- p1 + geom_line()
p1
#can also test the look of a plot without actually overriding it:
p1 +geom_point()

covid_w_pop$Date.corrected <- as.Date(covid_w_pop$Date, format = "%m/%d/%y")

by_country <- ggplot(data = covid_w_pop, aes(x = Date.corrected, y = Deaths))
by_country + geom_point(aes(col = Country.Region))
#the problem with this is the legend is too big (all countries in the legend)
#can change the theme to fix this by removing the legend:
by_country + geom_point(aes(col = Country.Region)) + theme(legend.position = "none")
#very messy since we are plotting all countries

#can fix this by selecting countries through vectors:
selec_countries <- c("United Kingdom", "China", "US", "Italy", "Germany", "France")

sel_country_plot <- ggplot(data = covid_w_pop %>%
                             filter(Country.Region %in% selec_countries),
                           aes(x = Date.corrected, y = Deaths))
sel_country_plot + geom_line(aes(col = Country.Region))
#the aes argument is needed to specify groups of data rather than just one line
#otherwise this happens:
sel_country_plot + geom_line()

#more subdued aesthetics (different line or shape types):
sel_country_plot + geom_line(aes(linetype = Country.Region))
#for point plots, can use shape as aesthetic:
sel_country_plot + geom_point(aes(shape = Country.Region))

#"facet()" spreads data from different groups across sub-plots:
sel_country_plot + geom_line() + facet_wrap(. ~ Country.Region)
#same but with colour:
sel_country_plot + geom_line(aes(col = Country.Region)) + facet_wrap(. ~ Country.Region)

#To save the plots:
pdf("Plots/Deaths by Country.pdf", width = 6, height = 4)

  sel_country_plot +
    geom_line(aes(col = Country.Region)) +
    facet_wrap(. ~ Country.Region)

  #Stop the pdf function and finish the .pdf file
  dev.off()
