library(vroom)
library(tidyverse)
library(devtools)
library(wbstats)
library(countrycode)

tokyo <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%205/Tokyo%202021%20medals.csv")
##add in position number
tokyo$Position <- 1:nrow(tokyo)

##pull GDP data
GDP <- wb_data(indicator = "NY.GDP.MKTP.CD", 
               start_date = 2019, 
               end_date = 2019)

tokyo$code <- countrycode(tokyo$Country, 
                          origin = "country.name", 
                          destination = "iso3c")

##correct china
tokyo$code[2] <-  "CHN"

tokyo <- left_join(tokyo,
                   GDP %>% select(iso3c, NY.GDP.MKTP.CD),
                   by = c("code" = "iso3c"))

tokyo <- rename(tokyo, gdp = NY.GDP.MKTP.CD)


tokyo
ggplot(tokyo, aes(x=gdp, y=Position)) +
  geom_point() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  theme_bw() +
  ggtitle("Logged data")


mod1 <- glm(Position ~ gdp,
            data = tokyo)

plot(mod1)

mod2 <- glm(log10(Position) ~ log10(gdp), data = tokyo)
plot(mod2)

summary(mod2)
