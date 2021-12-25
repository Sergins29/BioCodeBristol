library(vroom)
library(tidyverse)
library(dplyr)
soay <- vroom("Project/mass_qm21840.tsv")
NAO <- vroom("Project/NAO_qm21840.tsv")

#Hypothesis:
 #NAO affects survival of soay
 #Weight affects survival 
 #Must find relationship between NAO and body mass (weight)


#look at data:
head(soay)
head(NAO)

#soay data split in years but not months, will probably not need month column in NAO dataset

#dataframe with mean weights per year for better visualization:
soay_weight_year <- soay %>%
  group_by(year) %>%
  summarise("weight" = mean(weight, na.rm = T))


#plot the mean weight of the soay per year:
ggplot(data = soay_weight_year, aes(x = year, y = weight)) + 
  geom_point() +
  geom_line()

#split NAO values by year too:
NAO_by_year <- NAO %>%
  group_by(year) %>%
  summarise("NAO" = mean(NAO, na.rm = T))

#When plotting this data, clear increase in NAO by year
ggplot(data = NAO_by_year, aes(x = year, y = NAO)) +
  geom_point() +
  geom_smooth() +
  geom_line()

#Since both sets of data contain every year from 1980 to 2020, can simply add the NAO column per year to the soay weight per year dataframe
soay_weight_year$NAO <- NAO_by_year$NAO
soay_weight_year

#Can now visualize the data with ggplot:
ggplot(data = soay_weight_year, aes(x = NAO, y = year)) +
  geom_point() +
  geom_smooth()

#This plot clearly shows a trend between NAO and body mass. However, this could be due to other variables changing throughout the years.