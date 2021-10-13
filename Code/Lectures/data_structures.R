#matrices are 2d vectors, arrays are multidimensional vectors
matr <- matrix(seq(from = 1, to = 6, by = 1), ncol = 3, nrow = 2)
matr
matr2 <- matrix(NA, ncol = 3, nrow = 4)
matr2

#how to shape vector into matrix:
vect <- seq(from = 1, to = 6, by = 1)
dim(vect) <- c(2,3)
vect
#dim can also be used to view the dimensions of a matrix rather than modify its dimensions
dim(vect)

#task (make matrix out of 80 random nums)
rand <- rnorm(80, 30, 10)
dim(rand) <- c(10,8)
rand

#how to access stored matrix values
#first value is row, second value is column

rand[3,8]

#return multiple values:
#first value is row and second is columns
rand[1, c(3,4)]

#can also return values from full row or column
rand[1,]
rand[,2]

#matrix operations:
rand+1
rand-1
rand/2
rand*2

#replicate numbers in matrix
mat_rep <- matrix(rep(1:3, each = 5), nrow = 3, ncol = 5, byrow = TRUE)
#1:3 means from 1 to 3. each = 5 means each number repeats 5 times. nrow and ncol sets the dimensions. byrow = TRUE means matrix is filled row by row

vect <- 1:5
mat_rep * vect

#So every number in mat-rep was multiplied by numbers i vect in order (as soon as you get to 5, start again at 1. CHeck week 3 material)

mat_seq <- matrix(seq(from = 1, to = 20, length.out = 6), ncol = 3, nrow = 2)
mat_seq

#length.out determines number of values

vec_seq <- seq(from = 10, to = 4, length.out = 3)
vec_seq

#multiply matrix by vect:
mat_seq %*% vec_seq

#Boolean matrix:
mat_seq > 10

#return values that are == TRUE:
mat_seq[mat_seq > 10]

#check week 3 material for more functions

#Arrays:
array1 <- array(1:24, dim=c(3,4,2))
array1

#first value in dim is rows, second is columns, third is layers
#can have more dimensions than 3

rand_array2 <- array(seq(runif(100, min = 0, max = 1000)), dim=c(5,5,4))
rand_array2

data <- data.frame("Day" = rep(1:3, each = 3),
                   "Species" = rep(letters[1:3], each = 3),
                   "Seen" = rbinom(n = 9, size = 1, prob = 0.5))

data["Day"]

#can also access data using:

data$Day

data$location <- "United Kingdom"
data

simple_data <- data.frame("a" = runif(10,0,1),
                          "b" = rnorm(10,3,5))

#calculations between data sets in a data frame:
simple_data$calc <- (simple_data$a * simple_data$b) - simple_data$b
simple_data

#Task:
task_frame <- data.frame("name" = c("Anastasia", "Dima", "Katherine", "James", "Emily", "Michael", "Matthew", "Laura", "Kevin", "Jonas"),
                         "score" = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
                         "questions" = c(1,3,2,3,2,3,1,1,2,1),
                         "qualify" = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes'))

task_frame$mean <- task_frame$score / task_frame$questions

task_frame

#Lists:

#numeric matrix
num_mat <- matrix(rep(1:3, each = 5),
                  nrow = 3,
                  ncol = 5,
                  byrow = TRUE)

#vector of letters:
let_vec <- LETTERS[4:16]

#data frame of species:
species_dat <- data.frame("Species" = c("a", "b"),
                          "Observed" = c(TRUE, FALSE))

our_list <- list(num_mat,
                 let_vec,
                 species_dat,
                 5)

our_list

#to extract objects from list, can use double square brackets:
our_list[[3]]
#and to extract objects within osbjects, more square brackets:
our_list[[3]][1,]

#can also give each list object a name like in data frames to use the $ operator:
our_list <- list("matrix" = num_mat,
                 "letters" = let_vec,
                 "species" = species_dat,
                 "number" = 5)

our_list$matrix

#can also make a list of lists:

listof_lists = list("first_list" = our_list,
                   "second_list" = our_list)
listof_lists

#can now double call an object:

listof_lists$second_list$matrix


