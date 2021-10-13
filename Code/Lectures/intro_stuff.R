#add 1 and 1 together
1+1
6-7*(8-4*5)^2/2
1/0
-1/0
0/0

#variables
a <- 1
b <- 2
c = a + b
c

#Vectors
length(c(1, 2, 3, 5))
vect1 <- c(2,6,1,5)
vect2 <- c(5,3,9,0)

#join two vectors 
vect3 <- c(vect1,vect2)
vect3

#place vectors in vectors
vect4 <- c(1, 5, 4, 5, vect3, 2)
vect4
#recall one value of vect
vect4[5]
#recall multiple values
vect4[c(5,6)]
#operations on all values of vector
vect5 <- vect4*5
vect5

#matrix using vectors
matrix <- c("a" = 1, "b" = 2, "c" = 3)
matrix

#how to turn a vector into matrix
vect1
names(vect1) <- c("a","b","c", "d")
vect1

#retrieve matrix values either by index or name:
vect1[3]
vect1["c"]
#check vector names
names(vect1)

#sort vectors:
sorted_vec <- sort(vect1)
sorted_vec

#vectors with characters in no quotation marks (not numbers)
#will be seen as objects and thus give an error

#Factors:
class_names <- c("John", "Candice", "Hugh", "Joe", "Ben", "Candice", "Ben")
class_names <- as.factor(class_names)
class_names
#to just see unique items:
levels(class_names)

#Logical Vectors:
logic_vec <- c("TRUE", "FALSE")

#runif generates random numbers with set parameters:
x <- runif(n = 30, min = 0, max = 100)
#can also write it without the n, min and max arguments, just the values
x>5

#Logical operators:
#== (equal)
#!= (not)
#< (less than)
#> (more than)
#<= (less than or equal to)
#>= (more than or equal to)
##| (or)
#%in% (within)

#checking vectors:
is.numeric(1:5)
is.character(letters[1:5])
is.logical(c(TRUE, FALSE))
#the number makes it non logical
is.logical(c(TRUE, 1))

#Functions:#easy way:
#complex way:
a <- c(1,5,3,6,7,46,4,33,52,6,34,7,8,35,7)
sum_a <- sum(a)
length_a <- length(a)
mean_a <- sum_a/length_a
mean_a
#easy way:
mean(a)

#Arguments:
#round up:
round(c(1.4567654,4.565432,3.3456,7.7654,5.23456))
#round up with specific decimal places:
round(c(1.4567654,4.565432,3.3456,7.7654,5.23456), digits = 2)

#ignore missing values:
vect_na <- c(1,5,3,7,NA,7,3)
mean(vect_na)
mean(vect_na, na.rm = TRUE)
#Getting help:
?mean()

#nesting functions:
#this is asking the mean of the squareroots of 10 random numbers from 0 to 100
mean(sqrt(runif(10,0,100)))


#new functions:

se <- function(x){
  std_er <- sd(x)/sqrt(length(x))
  return (std_er)
}

nums <- runif(10,0,100)

se(x = nums)

