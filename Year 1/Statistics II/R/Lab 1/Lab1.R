###############################################################
#2024-02-20
#Lab 1
###############################################################
# Basic commands
###############################################################

# Numeric

x <- 2

This_is_a_really_long_name <- 3 #don't do it, stupid
this_is_a_really_long_name <- 4

mode(x) #find the mode
length(x) # find the length

X <- 2 + 8 # assign operation to object X

# Character
A <- "QE"

mode(A)

# Logical
z <- TRUE
y <- FALSE

mode(z)

# Other useful functions
ls() # #list of all the objects
rm(x) #remove object x
rm(z, y)
rm(list = ls()) #remove all, objects

# Help
help(rm)

###############################################################
## Vectors
###############################################################

# Numeric vector
a <- c(2, 3, 4, 5, 6) #Build a numeric vector
mode(a)

# Find the first element in a
a[1]

#Build vector b which has elements from a
b <- a[2:5]

#Build vector c which has elements in a
c <- a[c(1,5)]

#Build a vector that contains copy of a and b and c
d <- c(a, b, c)

#Replace element
a[1] <- 500
a[2] <- NA

###############################################################
# Processing numeric vectors
###############################################################

#Power
b^3 # Power
sqrt(b) #square root
min(b) #find min
max(b) #find max

x <- c(2, 3, 4, 5, 6)
y <- c(4, 5, 6, 7, 8)

#Sum
x+y

#Subtract
x-y

#Product
x*y

#Divide
x/y

#Sum of product
sum(x*y)
mean(x) #mean
var(x) #variance
sd(x) #standard deviation

# Character vector
D_vec <- c("QE", "VU", "EVAF")

C_vec <- c("1", "2", "3")

C_vec[1]

# Logical vectors
# Operators > < >= <= == != & |

# Extract elements in x
x > 4

xxy <- x[x > 4]
xxy <- x[x(x > 4)] #same

xxc <- x[x != 4]

xxd <- x[x < 5 & x > 3]

#Sequences -------------------------------------------
?seq


# Generate a sequence
x <- seq(1, 5, by = 0.5)

y <- seq(length.out = 9, from = 1, by = 0.5)

# Create a vector with identical elements
rep(1, 6)

# Combine
z <- rbind(x, y)
z

r <- cbind(x, y)
r

###############################################################
# Processing matrices
###############################################################

# Build a 5x4 matrix

x1 <- matrix(1:20, nrow = 5, byrow = TRUE)
x1

x0 <- matrix(1:20, ncol = 4, byrow = TRUE)
x0

x2 <- matrix(c(2, 4, 5, 6), nrow = 2)
x2

# Calculations

# Sum
x1 + x0 
# Subtract
x1 - x0
# Product
x1 * x0
# Matrix product
t(x1) %*% x0

# Combine matrix
cbind(x1, x0)
rbind(x1, x0)

# Inverse of matrix

# Square matrix

x3 <- matrix(c(5, 1, 0, 3, -1, 2, 4, 0, -1), nrow = 3)
x3

solve(x3) # inverse is solve()

dim(x1) # dimension

x1[2, 2]
x1[3, 1] <- 500

d <- c(2, 3, "QE")
d

#############################################################
# List
#############################################################

# Logical vector, length 2
x <- c(TRUE, FALSE)
x

# Character vector, length 3
y <- c("a", "b", "c")
y

# Numeric matrix
z <- x0
z

# Build a list
xyz_list <- list("Logical" = x, "Character" = y, "Numeric" = z)
xyz_list

xyz_list <- list(x, y, z)
xyz_list

length(xyz_list)

xyz_list$Character[2]

names(xyz_list)

xyz_list[[1]][1]

##################################################################
# Writing a condition
##################################################################
# if (expr1) expr2 else expr3

cc <- 1

#1
#
#if (cc == 1)
#  dd <- 4 else
#dd <- 5
#dd

#2

if (cc != 1){
  dd <- 4 } else {
     dd <- 5}
dd

#3
#if (cc == 1) dd <-4 else dd <-5
#dd

########################################################################
# Writing for loop
########################################################################
# for (name in exp1) expr2

# Build a vector in which each element is a square:
# z0 = [1^2, 2^2, ... , 10^2]

# 1. Create an empty z0 vector
z0 <- rep(0, 10)
z0

# 2. Write the for loop

for (i in 1:10) {
  z0[i] <- i^2
}
z0

# Example 2:
# create a vector: y = [1^3, 2^2, 3^3, 4^2, ... , 10^2]

y <- rep(0, 10)

v1 <- seq(1, 10, 2) #odd position
v2 <- seq(2, 10, 2) #even position

#Fill in the odd position of y
for (i in v1) {
  y[i] <- i^3
}
for (i in v2) {
  y[i] <- i^2
}

###################################################################
# Writing a while loop
###################################################################

# Built a vector in which each element whose
# order is smaller than 5 is the cube of the element
# and other elements are zero:
# z1 = [1^3, 2^3, 3^3, 4^3, 0, 0, .... , 0]

z1 <- rep(0, 10)
z1

i <- 1 #starting with i = 1 

while(i < 5){ #Run while the loop for i < 5
  z1[i] <- i^3
  i <- i + 1
  }
z1

###########################################################################
# Writing a function
###########################################################################
# name <- function(input){expr}

# Example 1:
square_fun <-function(x){
  x^2
}

square_fun(4)

#Example 2:
add_fun <- function(x, y){
  z <- y + 1
  return(x + z)
}

add_fun(59, 21)




