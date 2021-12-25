library(tidyverse)
library(vroom)
library(lubridate)
library(wbstats)
library(countrycode)

covid_dat <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")

class(covid_dat)

names(covid_dat)[1:2] <- c("Province.State", "Country.Region")

covid_long <- covid_dat %>%
  
  pivot_longer(cols = -c(Province.State,
                         Country.Region,
                         Lat,
                         Long))
covid_long

covid_long <- covid_dat %>%
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")

covid_long

#fix formatting of dates to have them all in day month year (big Y for 4 numb format)
covid_long$Date <- mdy(covid_long$Date)
covid_long$Date <- format(covid_long$Date, "%d-%m-%Y")

unique(covid_long$Date)

#pop data from wbstats:
pop_data <- wb_data(indicator = "SP.POP.TOTL",
                    start_date = 2002,
                    end_date = 2020)

#convert to tibble
pop_data <- as_tibble(pop_data)

pop_data

#to find latest date:
max(pop_data$date)


#filter data to include data from 2020 only:
pop_2020 <- pop_data %>%
  filter(date == 2020)

covid_long

#pop data not split by region like covid data:
pop_2020 %>% filter(country == "Australia")

#5 main tidyverse themes:
#mutate(): adds new variables as functions of existing variables
#select(): picks variables based on name
#filter(): picks cases based on their values
#summarise(): reduces multiple values down to single summary
#arrange(): changes ordering of the rows
#these can be combined using group_by() to specify groups within data to apply these to

#can use summarise to split covid data to have total deaths per country as a whole not split up
#can then pair this with group_by to specify groups we want to summarise

#this is calculating deaths in each country at each date:
covid_country <- covid_long %>%
  group_by(Country.Region, Date) %>%
  summarise(Deaths = sum(Deaths))

covid_country

#to view differences:
covid_country %>% filter(Country.Region == "Australia")
covid_long %>% filter(Country.Region == "Australia")

#use WB data to add iso3c column, 3 letter abbreviation for countries:
covid_country$code <- countrycode(covid_country$Country.Region,
                                  origin = "country.name",
                                  destination = "iso3c")

#to view specific row:
head(covid_country, 1)

#compare to pop data:
pop_2020 %>% filter(iso3c == "AFG")

#to join DFs, 3 functions:
#full_join(): keeps all columns & rows
#left_join(): returns all rows from x and columns from x & y
#right_join(): returns all rows from y and columns from x & y

#can use select value as well to remove some columns from WB data

#first, rename column so it works with data
names(pop_2020)[5] <- "value"

head(pop_2020 %>% select(iso3c, value))

#so to join, use by = to set from what the two DFs compare values. 
#Could also do by date but would have to correct it to be same
covid_w_pop <- left_join(covid_country,
                         pop_2020 %>% select(iso3c, value),
                         by = c("code" = "iso3c"))

covid_w_pop

#to find position of certain column:
which(names(covid_w_pop) == "value")
#can use this later to not specify position of column for other functions. 
#for example, change value column name:

names(covid_w_pop)[which(names(covid_w_pop) == "value")] <- "Population"

#interesting stats to find: n° of deaths, deaths per day and deaths per million people
#total global deaths:
most_recent <- covid_w_pop %>% filter(Date == max(covid_w_pop$Date))
sum(most_recent$Deaths)

#number of deaths per day
#to remove nas in case there are, can use na.rm=T 

global_deaths_day <- covid_country %>%
  group_by(Date) %>%
  summarise("Global.deaths" = sum(Deaths, na.rm=T))

global_deaths_day

#now check na values:
which(is.na(global_deaths_day$Global.deaths))
#knowing its 0 can proceed.

#calculate deaths per million for all countries per day:

covid_w_pop$Deaths.p.m <- (covid_w_pop$Deaths / covid_w_pop$Population) * 1000000
covid_w_pop

#visualize data with ggplot:

#aesthetic arguments are passed using aes():
ggplot(data = global_deaths_day, aes(x = Date, y = Global.deaths))
#we need to add what kind of plot we want to make or we will only make a blank plot:
ggplot(data = global_deaths_day, aes(x = Date, y = Global.deaths)) +
  geom_point()

#plot is horrible due to it seeing global deaths as a dbl (numeric) and date as a chr (character vector):
global_deaths_day
#Also, the plot doesnt know the dates are in a date format, thus the order is all messed up
#can use lubridate package as earlier:
global_deaths_day$Date.corrected <- as_date(global_deaths_day$Date,
                                            format = "%d-%m-%y")

#now lets try plotting again:
ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths)) +
  geom_point()

#can also plot using a line with geom_line()
#can add colour too:
ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths)) +
  geom_point(col = "darkgrey") +
  geom_line(col = "red")

#can also save a plot as a variable and add lines/points later:
p1 <- ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths))
p1
p1 <- p1 + geom_point()
p1

#more complex ggplot using more data:
#plot data by country to visualize how virus spreads over time in different places across world
covid_w_pop$Date.corrected <- as_date(covid_w_pop$Date,
                                      format = "%d-%m-%y")

by_country <- ggplot(data = covid_w_pop, aes(x = Date.corrected, y = Deaths))

#then, add points:
by_country + geom_point(aes(col = Country.Region))
by_country
#the legend is too big, so we have to remove it:
by_country + geom_point(aes(col = Country.Region)) + theme(legend.position = "none")

#very messy, lets just look at a few countries:
selec_countries <- c("United Kingdom", "China", "US", "Italy", "France", "Germany")

sel_country_plot <- ggplot(data = covid_w_pop %>%
                             filter(Country.Region %in% selec_countries),
                           aes(x = Date.corrected, y = Deaths))
sel_country_plot + geom_line(aes(col=Country.Region))

#if we didnt specify that there are groups of data (col = country) then this would happen:
sel_country_plot + geom_line()

#can also use other types of aesthetics:
sel_country_plot + geom_line(aes(linetype = Country.Region))
sel_country_plot + geom_point(aes(shape = Country.Region))                             

#Can also use faceting: spread data to different plots. functions: facet_wrap and facet_grid
#~ means "as a function of"
#if you only have one group to facet by rather than 2, you use a . instead of a char:
sel_country_plot + geom_line(aes(col = Country.Region)) + facet_wrap(. ~ Country.Region)
sel_country_plot + geom_line() + facet_grid(. ~ Country.Region)

#Can save plots to computer too:
#specify directory of pdf with width and height:
pdf("Plots/deaths_by_country.pdf", width = 6, height = 4)
sel_country_plot +
  geom_line(aes(col = Country.Region)) +
  facet_wrap(. ~ Country.Region)
dev.off()
