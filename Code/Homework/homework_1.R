#create random number vector
a <- runif(100,0,50)
sorted_a <- sort(a,decreasing = TRUE)
sorted_a

#function to find vector with subtracted log base 10
log_vect <- function(x){
  base <- log(x)
  new_vect = x - base
  return (new_vect)
}

#function to calculate standard error
se <- function(x){
  std_er <- sd(x)/sqrt(length(x))
  return (std_er)
}

#run log function on sorted vector
new_vect <- log_vect(sorted_a)
new_vect

#standard error, mean and standard deviation
#standard error:
error <- se(new_vect)
#mean:
mean <- mean(new_vect)
#standard deviation:
dev <- sd(new_vect)

error
mean
dev


