#LAB7
#Lab7.1

# (i) Creating 3x4 matrix with numbers from 10 to 120 in steps of 10
amat <- matrix(seq(10, 120, by = 10), nrow = 3, byrow = TRUE)
print(amat)
# Creatung another matrix with byrow = FALSE
amat2 <- matrix(seq(10, 120, by = 10), nrow = 3, byrow = FALSE)
print(amat2)# amat(byrow=TRUE) fills rows wise, amat2 fills  column wise

# (ii) Assigning row and column names
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print(amat)  # Checking assignments

# (iii) Creating two 4x4 matrices A and B
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow = 4)
B <- matrix(c(12, 5, 3, 17, 1, 18, 9, 10, 1, 12, 5, 10, 4, 15, 15, 4), nrow = 4)
print(A)
print(B)
# Element-wise multiplication
print(A * B)
# Matrix multiplication
print(A %*% B)

# (iv) Outer and inner product of vectors X and Y
X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
# Outer product
print(outer(X, Y))
# Inner product
print(sum(X * Y))  # Same as crossprod(X, Y)

# (v) Creating a diagonal matrix from X
diag_matrix <- diag(X)
print(diag_matrix)

# (vi) Printing diagonal elements of A
print(diag(A))

# (vii) Creating a 6x6 identity matrix
Iden_6 <- diag(6)
print(Iden_6)

# (viii) Creating a 3x3 matrix A
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow = 3)
print(A)

# (ix) Creating a 3x1 matrix B
B <- matrix(c(5, -3, 13), nrow = 3)
print(B)

# (x) Solving AX = B for X
X <- solve(A, B)
print(X) 
print(typeof(X)) #finding what type of object X is

# (xi) Finding the inverse of A
Ainv <- solve(A)
print(Ainv)
# Checking if Ainv * A gives an identity matrix
print(Ainv %*% A)  # Should be close to identity
print(round(Ainv %*% A,10))

# (xii) Computing eigenvalues and eigen vectors of A
results <- eigen(A)
print(results)
str(results)  # The structure of results gives a list with values and vectors
# Matrix-vector multiplication using second eigen vector
eig_vec2 <- results$vectors[,2]
Av2<-A %*% eig_vec2 #result is lambda2*eig_vec2 but we need to cross check this
print(Av2)

lambda2 <- results$values[2] #extracting eigen value2
print(lambda2)  
# Multiplying the eigenvector by its corresponding eigenvalue
lambda2_v2 <- lambda2 * eig_vec2
print(lambda2_v2)  #check if it same as Av2


#Lab7.2

# (i) Creating a new column after loading csv
data4 <- read.csv("/home/ibab/Downloads/BrainCancer.csv")  # Loading dataset
data4$GTV_time_sum <- data4$gtv^2 + data4$time  # Computing new column

# (ii) Printing row and column names
print(rownames(data4))  
print(colnames(data4)) 

# (iii) Renaming rows using 'paste' function
rownames(data4) <- paste("Row-",1:nrow(data4), sep="") 
print(rownames(data4))

# (iv) 
data4$ki <- NULL  # Removing column 'ki'
print(colnames(data4))  # Printing the column names to verify


#Lab7.3
#(i)
install.packages("readxl") #installing excel package

#(ii)
library(readxl) #loading excel package

#(iii)
data_ex <- read_excel("/home/ibab/R/pone.0148733.s001.xlsx",1) #loading the excel document,1 here stands for the sheet number

#(iv)Printing column names and dimensions of the excel document
print(names(data_ex))
print(dim(data_ex))


#Lab7.4
# (i) Creating two vectors, A and B  
A <- c("a", "b", "c", "d", "e")  
B <- c("d", "e", "f", "g")  
# Printing both vectors 
print(A)  
print(B)  

# (ii) Finding the union (all unique elements from both A and B)  
union(A, B)  

# (iii) Finding the intersection (elements that exist in both A and B)  
intersect(A, B)  

# (iv) Finding the difference (elements in A but not in B)  
setdiff(A, B)  

# Finding the difference (elements in B but not in A)  
setdiff(B, A)  

# (v) Checking if the union is the same as the combined differences and intersection  using setequal 
setequal(c(setdiff(A, B), intersect(A, B), setdiff(B, A)), union(A, B))  #it is equal

# (vi) Finding elements of B that are in A using two different ways  
A%in%B #gives you TRUE or FALSE for every element in seta
A[A %in% B]  # method1-Using logical indexing  
intersect(A, B)  # method2-Using the intersection function  

# (vii) Finding elements of A that are in B  
B[B %in% A]  # Similar approach using logical indexing  


#Lab7.5
# (i) Creating a vector with given elements
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
# (a) Values greater than 12
vec[vec > 12]
# (b) Values greater than 10 and less than 20
vec[vec > 10 & vec < 20]

# (ii) Creating an array with given values
A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
print(A)
# Removing NA and keep values < 100
A_new <- A[!is.na(A) & A < 100]
print(A_new)

# (iii) Replacing NA values with 0
A[is.na(A)] <- 0
print(A)

# (iv) Creating vectors for gene names and gender
genes <- paste0("gene-", 1:7)
gender <- c("M", "M", "F", "M", "F", "F", "M")

# (v) Creating 7 result vectors
result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)
genes <- paste0("gene-", 1:7)
gender <- c("M", "M", "F", "M", "F", "F", "M")
# (vi) Creating a dataframe with the given columns
datframe <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)
print(head(datframe))

# (vii) Renaming columns
colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")
print(colnames(datframe))

# (viii) Subset data where "expt2" values are greater than 20
subset_expt2 <- datframe[datframe$expt2 > 20, ]
print(subset_expt2)

# (ix) Subset data with only Female gender
subset_female <- datframe[datframe$Gender == "F", ]
print(subset_female)

# (x) Subset data with Male gender and "expt2" values less than 30
subset_male_expt2 <- datframe[datframe$Gender == "M" & datframe$expt2 < 30, ]
print(subset_male_expt2)


#Lab7.6
# (i) Checking which quadrant an angle belongs to using if-else-if  
angle <- 45  # defining angle as 45

if (angle > 0 & angle < 90) {  
  print("First quadrant")  
} else if (angle > 90 & angle < 180) {  
  print("Second quadrant")  
} else if (angle > 180 & angle < 270) {  
  print("Third quadrant")  
} else if (angle > 270 & angle < 360) {  
  print("Fourth quadrant")  
} else {  
  print("Angle is on an axis or out of range")  
}  

# (ii) Sorting three numbers in decreasing order without using sorting functions  
a <- 15  
b <- 8  
c <- 20  

if (a >= b & a >= c) {  
  if (b >= c) {  
    print(c(a, b, c))  
  } else {  
    print(c(a, c, b))  
  }  
} else if (b >= a & b >= c) {  
  if (a >= c) {  
    print(c(b, a, c))  
  } else {  
    print(c(b, c, a))  
  }  
} else {  
  if (a >= b) {  
    print(c(c, a, b))  
  } else {  
    print(c(c, b, a))  
  }  
}  

# (iii) Calculating ticket cost based on distance and age  
distance <- 1200  #example
age <- 65   #example

if (distance <= 100) {  
  cost <- 100  #cost for first 100km
} else if (distance <= 1000) {  
  cost <- 100 + (distance - 100) * 1.5  #additional cost upto 1000km
} else {  
  cost <- 100 + (900 * 1.5) + ((distance - 1000) * 2)  #additional cosr after 1000km
}  

if (age > 60) {  
  cost <- cost * 0.75  # Senior citizen discount of 25 percent 
} else if (age < 6) {  
  cost <- cost * 0.5  # Child discount of 50 percent
}  

print(paste("Ticket cost:", cost))  


#Lab7.7
# (i) Function to replace all negative values in a vector with zeros  
replace_negatives <- function(vec) {  
  vec[vec < 0] <- 0  
  return(vec)  
}  

test_vec <- c(-5, 3, -2, 7, 0, -8, 9)  #testing the function
print(replace_negatives(test_vec))  

# (ii) Function to compute factorial using Stirling’s approximation  
stirling_factorial <- function(n) {  
  if (n < 0) {  
    return("Factorial not defined for negative numbers")  
  }  
  pi_part <- sqrt(2 * pi * n)  
  power_part <- (n / exp(1))^n  
  correction <- 1 + (1 / (12 * n)) + (1 / (288 * n^2)) - (139 / (51840 * n^3)) - (571 / (2488320 * n^4))  
  return(pi_part * power_part * correction)  
}  

print(stirling_factorial(-5))  #testing the function
print(stirling_factorial(10))  #testing the function

# (iii) Function to sum the digits of a number  
sum_digits <- function(num) {  
  num_str <- as.character(abs(num))  # Converting to string and taking absolute value  
  digits <- as.numeric(unlist(strsplit(num_str, "")))  # Spliting into individual digits  
  return(sum(digits))  
}  

print(sum_digits(1234))  #testing the function
print(sum_digits(-5678))  #testing the function




