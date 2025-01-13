#Q1.1
x <- round(12.1343,digits=3)  # Round to 3 digits
print(x)

#Q1.2
x <- round(123.12344,digits=3)  # Round to 3 digits
print(x)

#Q1.3
x <- round(1234.12344,digits=3)  # Round to 3 digits
print(x)

#Q1.4
x <- round(12345.12344,digits=3)  # Round to 3 digits
print(x)

#Q1.5
options(digits=15)  # Set digits to 15
x <- round(1234.12344,digits=3)  # Round to 3 digits
print(x)
x <- round(12345.12344,digits=3)  # Round to 3 digits
print(x)

#Q1.6
formatC(round(12345.12344,digits=3),format="f",digits=3)  # Format rounded value

#Q1.7
x <- (1234.12344)  # Assign value to x
print(x)

#Q1.8
x <- 1234.723  # Assign value to x
print(x,digits=3)  # Print with 3 digits
x <- 1234.723  # Assign value to x
print(x,digits=5)  # Print with 5 digits

#Q1.9
x <- round(123456788.123,digits=3)  # Round to 3 digits
print(x)

#Q1.10
print(round(123456788.123,digits=2),digits=20)  # Round to 2 digits, print with 20 digits

#Q1.11
print(round(123456789.1234,digits=4),digits=20)  # Round to 4 digits, print with 20 digits

#Q1.12
paste("Hello World")  # Concatenate string
paste("Hello","World")  # Concatenate two strings

#Q1.13
paste(1:10)  # Concatenate sequence
paste(1:10)[4]  # Get 4th element of paste sequence

#Q1.14
as.numeric(paste(1:10))  # Convert paste output to numeric

#Q1.15
paste(1:10,collapse=".")  # Concatenate with period separator

#Q1.16
paste(c("Hello", "World"), 1:10, sep = "-")  # Concatenate with hyphen separator
print(paste("Hello", 1:10, sep = "-"))  # Print concatenated string

#Q2.1
x <- 0:10  # Sequence from 0 to 10
print(x)

#Q2.2
x <- 15:5  # Sequence from 15 to 5
print(x)

#Q2.3
x <- seq(0,1.5,0.1)  # Sequence from 0 to 1.5 with step 0.1
print(x)

#Q2.4
x <- seq(6,4,-0.2)  # Sequence from 6 to 4 with step -0.2
print(x)

#Q2.5
N <- c(55,76,92,103,84,88,121,91,65,77,99)  # Define vector N
print(N)

#Q2.6
x <- seq(from=0.04,by=0.01,length=11)  # Sequence with specified length
print(x)
y <- seq(0.04,by=0.01,along=N)  # Sequence with length matching N
print(y)

#Q2.7
x <- seq(from=0.04,to=0.14,along=N)  # Sequence with specified range and length
print(x)

#Q2.8
x <- sequence(c(4,3,4,4,4,5))  # Generate sequences of unequal lengths
print(x)

#Q2.9
rep(9,5)  # Repeat 9 five times
rep(1:4,2)  # Repeat 1:4 two times
rep(1:4,each=2)  # Repeat each element twice
rep(1:4,each=2,times=3)  # Repeat each element twice, three times
rep(1:4,1:4)  # Repeat with varying counts

#Q2.10
rep(1:4,c(4,1,4,2))  # Repeat with specific counts
rep(c("cat", "dog", "goldfish", "rat"), c(2, 3, 2, 1))  # Repeat strings

#Q2.11
seq(-1,1,by=0.1)  # Sequence from -1 to 1 with step 0.1
seq(-1,1,0.1)  # Equivalent sequence with step 0.1

#Q2.12
seq(-1,1,length=7)  # Sequence with length 7

#Q2.13
x <- -1 + 0.1 * (0:20)  # Generate sequence without seq()
print(x)


#Q3.1
x <- 3/0  # Division by zero (infinity)
print(x)

#Q3.2
x <- exp(-Inf)  # Exponentiation with negative infinity
print(x)

#Q3.3
x <- (0:3)**Inf  # Raising numbers to infinity (results in Inf or NaN)
print(x)

#Q3.4
x <- 0/0  # Indeterminate form (NaN)
print(x)

#Q3.5
x <- Inf-Inf  # Infinity minus infinity (NaN)
print(x)

#Q3.6
x <- Inf/Inf  # Infinity divided by infinity (NaN)
print(x)

#Q3.7
x <- is.finite(10)  # Check if 10 is finite (TRUE)
print(x)

#Q3.8
is.infinite(10)  # Check if 10 is infinite (FALSE)

#Q3.9
is.infinite(Inf)  # Check if Inf is infinite (TRUE)

#Q3.10
y <- c(4, NA, 7)  # Create vector with NA
y == "NA"  # Comparison with string "NA" (not applicable)
is.na(y)  # Check which elements are NA (TRUE for the second element)

#Q3.11
y[!is.na(y)]  # Remove NA values from vector (only 4 and 7 remain)

#Q3.12
c1 <- c(1, 2, 3, NA)  # Vector with NA
c2 <- c(5, 6, NA, 8)  # Vector with NA
c3 <- c(9, NA, 11, 12)  # Vector with NA
c4 <- c(NA, 14, 15, 16)  # Vector with NA
full.frame <- data.frame(c1, c2, c3, c4)  # Create data frame
print("Dataframe:")  # Print header
print(full.frame)  # Print full data frame
reduced.frame <- full.frame[!is.na(full.frame$c1), ]  # Remove rows where c1 is NA
print("Reduced Frame:")  # Print header
print(reduced.frame)  # Print reduced data frame
x <- mean(c1, na.rm = TRUE)  # Calculate mean of c1 ignoring NAs
print("Mean of c1 in reduced frame:")  # Print header
print(x)  # Print mean of c1

#Q3.13
v <- c(1:6, NA, NA, 9:12)  # Create vector with NAs
seq(along=v)[is.na(v)]  # Get indices of NAs using seq() and logical indexing
which(is.na(v))  # Get indices of NAs using which()


# (4) Vectors

# (i) Vector assignment and queries
vec <- c(4, 7, 6, 5, 6, 7)
class(vec)  # Display class of the vector
length(vec) # Length of the vector
min(vec)    # Minimum value in the vector
max(vec)    # Maximum value in the vector

# (ii) Vector creation using keyboard input
vec <- scan()  # Create a vector using keyboard input

# (iii) Vector subscripts
vec[4]  # Access the 4th element (1-based indexing)

# (iv) Extracting multiple elements using two ways
ind <- c(2, 3, 6)
vec[ind]      # Using index vector
vec[c(2, 3, 6)] # Another syntax to access the same elements

# (v) Drop elements using minus symbol
vec[-1]  # Remove the first element from the vector

# (vi) Remove the last element
vec[-length(vec)]  # Remove the last element from the vector

# (vii) Write a trim function to remove largest and smallest two values
trim <- function(x) { 
  sorted_x = sort(x)  # Sort the vector
  sorted_x[c(-1, -2, -(length(sorted_x)), -(length(sorted_x)-1))]  # Remove 2 min and 2 max
}
trim(vec)  # Apply trim function to the vector

# (viii) Use sequences to extract elements
vec[1:3]  # Extract first 3 elements
vec[seq(2, length(vec), 2)]  # Extract every second element starting from index 2
vec[1:length(vec) %% 2 == 0]  # Extract even-indexed elements using logical condition

# (ix) Sum of values of x that are less than 5
x <- 0:10
sum(x[x < 5])  # Sum using logical subsetting
total_sum <- sum(x[x < 5])  # Another way, storing result in a variable

# (x) Sum of three largest values
x <- c(10, 13, 2, 6, 3, 14, 1)
sorted_x <- sort(x, decreasing = TRUE)
sum(sorted_x[1:3])  # Sum the top 3 largest values

# (xi) Finding index of vector corresponding to max or min value
which.max(x)  # Index of the maximum value in x
which.min(x)  # Index of the minimum value in x

# (xii) Combining vectors as columns or rows
cbind(1:10, 10:1)  # Combine vectors as columns
rbind(1:10, 10:1)  # Combine vectors as rows

# (xiii) Basic operations with vectors
X <- c(1:10)  # Create vector X
Y <- c(1:10 * 5)  # Create vector Y
X * Y  # Element-wise multiplication
X + Y  # Element-wise addition
X / Y  # Element-wise division
X ^ Y  # Element-wise exponentiation
log(X)  # Logarithm of each element in X
exp(Y)  # Exponential of each element in Y

# (5) Matrices/Dataframes/Arrays

# Creating arrays and changing dimensions
y <- 1:24
dim(y) <- c(2, 4, 3)  # Create a 2x4x3 array
print(y)  # Print the array
dim(y) <- c(3, 2, 4)  # Change the dimensions to 3x2x4
print(y)  # Print the modified array

# (i) Matrix creation with the matrix function
X <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)
print(X)  # Identity matrix

# (ii) Another way to convert vector to matrix using dim function
vector <- c(1, 2, 3, 4, 4, 3, 2, 1)
V <- matrix(vector, byrow = TRUE, nrow = 2)  # Convert to matrix by row
dim(vector) <- c(4, 2)  # Convert vector to matrix using dim
is.matrix(vector)  # Check if the vector is now a matrix

# (6) Vector functions

# (i) Common functions used on vectors
vec <- c(5, 2, 8, 1)
min(vec)  # Minimum value in the vector
max(vec)  # Maximum value in the vector
sum(vec)  # Sum of the elements
range(vec)  # Range of the elements (min, max)
sort(vec)  # Sorted vector

# (ii) Column means of a matrix
M <- matrix(1:6, nrow = 2, ncol = 3)
colMeans(M)  # Calculate column means of the matrix

# (iii) Matrix operations: Outer product, Transpose, Dot product
X <- c(1, 2, 3, 4)
Y <- c(5, 6, 7)

# Outer product (X[1:4] %o% Y[1:3])
Z <- X[1:4] %o% Y[1:3]
YoX <- Y[1:3] %o% X[1:4]

# Transpose of matrices
t(Z)   # Transpose of Z
t(YoX) # Transpose of YoX

# Dot product using %*%
X %*% Y  # Dot product of X and Y

# Dot product using element-wise multiplication
sum(X * Y)  # Another way to calculate dot product

# Cross product
crossprod(X[1:4], Z)  # Cross product of X and Z

# Identity matrix of size 4
diag(4)  # Create a 4x4 identity matrix

# (iv) Check the class of X
class(X)  # Display the class of X (numeric vector)
