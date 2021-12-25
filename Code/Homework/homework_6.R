library(vroom)
library(tidyverse)
library(devtools)

data1 <- vroom("Data/Workshop 6/data 1.csv")
data2 <- vroom("Data/Workshop 6/data 2.csv")
data3 <- vroom("Data/Workshop 6/data 3.csv")
data4 <- vroom("Data/Workshop 6/data 4.csv")

head(data1)
head(data2)
head(data3)
head(data4)

dat_1_long <- data1 %>%
  
  pivot_longer(cols = -c(y),
               names_to = "x",
               values_to = "value")


mod_1 <- glm(y ~ value,
             data = dat_1_long,
             family = "poisson")
class(mod_1)

plot(mod_1)
summary(mod_1)
