install.packages("devtools", dependencies =TRUE)
library("devtools")
install_github("r-lib/vroom")
#to require a package, need to do library(package) after installing
#Otherwise, can tell R to use a certain function from a package:
vroom::vroom()
#this way, you dont overwrite other base functions which may have the same name
#for now well require it:
library(vroom)

#.csv files: universal file format that works on anything
wad_data <- vroom("wader_data.csv")
