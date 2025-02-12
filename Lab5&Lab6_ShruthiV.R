# LAB5.5
data = read.csv("/home/ibab/Downloads/BrainCancer.csv", header=TRUE) 
data$sex <- factor(data$sex, levels=c("Male","Female")) # Convert 'sex' to a factor
class(data$sex)
print(nlevels(data$sex)) # Print number of levels in 'sex'
print(levels(data$sex))  # Print the levels of the 'sex' factor

#Lab5.6
rows_num <- nrow(data) # Get the number of rows in the data
temp <- gl(3, 1, length = rows_num, labels = c("Hot", "Cold", "Lukewarm")) # Generate a factor 'temp'
print(temp)
new_df <- data.frame(data,temp) # Add the 'temp' factor to the data frame
print(new_df)

#Lab5.7
print(tapply(data$gtv, data$ki, mean)) # Calculate mean of gtv grouped by ki
filter3 = subset(data$gtv,data$ki ==70) # Subset gtv where ki equals 70
print(filter3)
print(mean(filter3)) # Mean of the subset
sorted_filter3 <- sort(filter3) # Sort the subset
mean(sorted_filter3,trim=0.1) # Trimmed mean of the sorted subset
print(tapply(data$gtv, data$ki, mean, trim=0.1)) # Trimmed mean of gtv grouped by ki 
# The trim option removes a fraction of observations from each end before computing the mean

#Lab5.8
print(pmin(data$gtv, data$ki, data$time)) # Find the parallel minimum of gtv, ki, and time
print(pmax(data$gtv, data$ki, data$time)) # Find the parallel maximum of gtv, ki, and time

#Lab5.9
ranks <- rank(data$gtv) # Calculate ranks of gtv
sorted <- sort(data$gtv) # Sort gtv values
ordered <- order(data$gtv) # Get the order of gtv values
view <- data.frame(data$gtv,ranks,sorted,ordered) # Create a data frame to view the results
print(view)
view2 <- data$diagnosis[ordered] # Get diagnosis values based on the order of gtv
print(view2)
new2_df = data.frame(sort(data$gtv),view2) # Create new df with sorted gtv and ordered diagnosis
write.csv(new2_df,file="/home/ibab/R/lab4 ordered data.csv") # Write the new data frame to a CSV file

#Lab5.10
filter1 = data[1:6,3:8] # Extract rows 1-6, columns 3-8
print(filter1)
filtermat = as.matrix(filter1) # Convert the extracted data to a matrix
print(attributes(filtermat)) # Print matrix attributes
newcol = data$ki + data$gtv + data$time # Create a new column by summing ki, gtv, and time
newcoladded <- data.frame(data,newcol) # Add the new column to the data frame
print(newcoladded)
newcoladded2 <- cbind(data,newcoladded) # Add the new column to the data frame using cbind
print(colnames(newcoladded2)) # Print the column names to verify the new column
newrow <- data[26:35,] # Extract rows 26-35
newrowadded = rbind(data,newrow) # Add the extracted rows to the data frame
print(newrowadded)
print(dim(data)) # Print the dimensions of the original data frame
print(dim(newrowadded)) # Print the dimensions of the new data frame

#Lab5.11
X <- c(1, 0, 2, 5, 3, 1, 1, 3, 1, 3, 3, 1, 0, 2, 2, 1, 0, 2, 1, 0)
X <- matrix(X,nrow=4,ncol=5)
print(rownames(X))
rownames(X) <- rownames(X,do.NULL=FALSE,prefix = "Trial") # Add row names with the prefix "Trial"
print(rownames(X))
drug_names <- c("aspirin","paracetamol","nurofen","hedex","placebo") # Define column names
colnames(X) <- drug_names # Assign the column names
print(colnames(X))
dimnames(X) <- list(NULL,paste("drug",1:5,sep = "")) # Assign column names using dimnames
print(colnames(X))

#Lab5.12
# LAB5.12.a
print(mean(X[,5])) # Mean of column 5
# LAB 5.12.b
print(var(X[4,]))  # Variance of row 4
# LAB 5.12.c
print(rowSums(X))  # Row sums
print(apply(X,1,sum)) # Row sums (using apply)
# LAB 5.12.d
print(colSums(X))  # Column sums
print(apply(X,2,sum)) # Column sums (using apply)
# LAB 5.12.e
print(rowMeans(X)) # Row means
print(apply(X,1,mean))# Row means (using apply)
# LAB 5.12.f
print(colMeans(X)) # Column means
print(apply(X,2,mean)) # Column means (using apply)
# LAB 5.12.g
print(rowsum(X,c("A","B","B","A"))) # Row sums by group
group=c("A","B","B","A")
print(rowsum(X,group))# Row sums by group
print(row(X)) # Row indices
print(col(X)) # Column indices
print(X)
print(row(X))
print(col(X))
print(group[row(X)],col(X))
print(list(print(group[row(X)],col(X))))
print(tapply(X,list(group[row(X)], col(X))),sum)# tapply with group

# 12.h Shuffle matrix columns
print(apply(X,2,sample)) 

# 12.i Add row means & row variances
X <- rbind(X, apply(X,2,mean))
X <- cbind(X, apply(X,1,var))
headings <- c(paste("drug.",1:5,sep=""),"var")
dimnames(X) <- list(NULL,headings)
print(X)

# 13.a Create dataframe
data <- read.csv("~/Desktop/biostat/BrainCancer.csv", header=TRUE)
eg_sweep <- data.frame(data$ki, data$gtv, data$time)

# 13.b Column means
cols <- apply(eg_sweep,2,mean)
print(cols)

# 13.c Manual sweeping
cols.means <- matrix(rep(cols, rep(dim(eg_sweep)[1], dim(eg_sweep)[2])),
                     nrow=dim(eg_sweep)[1])
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)

# 13.d Sweep function
eg_sweep1 <- sweep(eg_sweep, 2, cols)
print("Method 2")
print(eg_sweep1)

# 14.a Read data & extract first 54 cols
data <- read.table("pgfull.txt", header = TRUE)
species <- data[, 1:54]

# 14.b Max column index per row
max_indices <- max.col(species)
print(max_indices)

# 14.c Get species names
species_names <- names(species)[max_indices]
print(species_names)

# 14.d Frequency table of species
species_freq <- table(species_names)
print(species_freq)

# 14.e Min column index per row
min_indices <- max.col(-species) 
print(min_indices)

# 14.f Alternative method for min index
min_indices <- apply(species, 1, which.min)
print(min_indices)

# 15.a Lists with different types
apples <- c(4,4.5,4.2,5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)

# 15.b Access list elements
print(items[[3]])
print(items[[3]][3])

# 15.c Named list
items <- list(first=apples, second=oranges, third=chalk, fourth=cheese)
print(items$fourth)

# 15.d List properties
print(class(items))
print(lapply(items, length))
print(lapply(items, class))
print(lapply(items, mean)) # Predict output
