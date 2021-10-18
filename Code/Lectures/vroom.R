install.packages("devtools", dependencies =TRUE)
library("devtools")
install.packages("tidyverse")
library("tidyverse")
install_github("r-lib/vroom", force = TRUE)
#to require a package, need to do library(package) after installing
#Otherwise, can tell R to use a certain function from a package:
vroom::vroom()
#this way, you dont overwrite other base functions which may have the same name
#for now well require it:
library(vroom)

#.csv files: universal file format that works on anything
wad_data <- vroom("Data/wader_data.csv")

head(wad_data)

covid_data <- vroom("Data/time_series_covid19_confirmed_global.csv")

head(covid_data)

#to download data directly from github:
covid_data_git<- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")
#The link is from clicking "raw" on .csv file in github
head(covid_data_git)

##to briefly summarise it is reading in some data included in base R
##and then splitting it into 3 differnt data.frame style objects based on the values in one of the columns ("cyl")

mt <- tibble::rownames_to_column(mtcars, "model")
purrr::iwalk(
  split(mt, mt$cyl),
  
  ~ vroom_write(.x, glue::glue("mtcars_{.y}.csv"), "\t")
)

#find files in default directory which start with "mtcars" and end in "csv"
#save them with variable/object "files":
files1 <- fs::dir_ls(glob = "mtcars*csv")

files1

#then load them using vroom:
vroom(files1)


#To write .csv files:
#example data frame:
data <- data.frame("Day" = rep(1:3, each = 3),
                   "Species" = rep(letters[1:3], each = 3),
                   "Seen" = rbinom(n = 9, size = 1, prob = 0.5))

vroom_write(data, "Data/newdata.csv")

#where my_data is a data frame

#Can also write out data as an Rdata file:
save(data, file = "Data/newerdata.RData")

#how to load data in using Rdata:
load("Data/newerdata.RData")

#How to tidy up data after importing it:
#use of tidyverse or data.table with larger data sets (above 1 gigabyte)

class(covid_data_git)

#change first two names of data frame:
#The first two columns are named province/state, which may confuse R
#since itll try to divide province values by state values.
names(covid_data_git)[1:2] <- c("Province.State", "Country.Region")

#data reshaping with pivot_ (from wide to long format)
#the %>% symbol is equivalent to putting the first argument in the following function
#so we are asking R to pivot_longer the covid_data and giving the new long data the variable "covid_long"
#tidyverse doesn't need variables to be in "" thus why we needed to remove the "/" earlier
covid_long <- covid_data_git %>%
                
  pivot_longer(cols = -c(Province.State,
                         Country.Region,
                         Lat,
                         Long))
#"cols =" specifies the columns we want pivot_longer to use to pivot data around
#So now the specified columns become the main columns while all the unspecified columns become a new group called "name"
#Can specify the name of the column "name" like this:

covid_long <- covid_data_git %>%
  
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")

#province.state:long makes it so all the columns from province state to long are selected
#this is useful if we need to select many columns to keep

covid_long

#to change long data to wide, we use pivot_wider()

covid_long %>%
  pivot_wider(names_from = Date,
              values_from = Deaths)
#names_from will become new columns, while values_from will be the values that go into these set columns.