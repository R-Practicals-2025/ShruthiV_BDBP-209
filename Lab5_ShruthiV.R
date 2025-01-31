# LAB5
# Using BrainCancer.csv
# LAB5.1
data = read.csv("/home/ibab/Downloads/BrainCancer.csv", header=TRUE)  # Read BrainCancer.csv

# LAB5.2
# LAB5.2.a
print(dim(data))  # Print dimensions (rows and columns) of data
print(length(data))  # Print number of columns in data

# lab5.2.b
print(colnames(data))  # Print column names of data

# lab5.2.c
print(rownames(data))  # Print row names of data

# lab5.2.d
print(head(data, n=30))  # Print first 30 rows of data

# lab5.2.e
#print(table(data))  # Frequency table for all columns
print(table(data$diagnosis))  # Frequency table for "diagnosis" column

# lab5.2.f
# Convert categorical columns to factors with specific levels
data$sex <- factor(data$sex, levels = c("Male", "Female"))
data$diagnosis <- factor(data$diagnosis, levels = c("Meningioma", "HG glioma", "Other"))
data$loc <- factor(data$loc, levels = c("Infratentorial", "Supratentorial"))
data$stereo <- factor(data$stereo, levels = c("SRT", "SRS"))

# lab5.2.f
# There are 4 categorical variables present

# lab5.3.g
# The categorical variables are sex, diagnosis, loc, stereo

# lab5.2.h
# Print the number of levels and the levels of each categorical variable
print(nlevels(data$sex))
print(nlevels(data$diagnosis))
print(nlevels(data$loc))
print(nlevels(data$stereo))
print(levels(data$sex))
print(levels(data$diagnosis))
print(levels(data$loc))
print(levels(data$stereo))

# lab5.3.a
print(mean(data$gtv))  # Print mean of GTV column

# lab5.3.b
print(mean(data$time))  # Print mean of time column

# lab5.3.c
print(median(data$gtv))  # Print median of GTV column

# median is less than mean indicating left skewness

# lab5.3.d
print(names(which.max(table(data$gtv))))  # Print mode of GTV (most frequent value)

# Distribution is not symmetric

# lab5.3.e
print(sd(data$gtv))  # Print standard deviation of GTV

# lab5.3.f
summary(data$gtv)  # Print summary of GTV data
hist(data$gtv)  # Plot histogram for GTV

# The histogram agrees with mean, median, mode

# lab5.3.g
library(moments)  # Load moments package
skewness(data$gtv)  # Print skewness of GTV

# lab5.3.h
kurtosis(data$gtv)  # Print kurtosis of GTV

# Skewness and Kurtosis match with the expectations; they are more than 0

# lab5.3.i
boxplot(data$gtv)  # Default boxplot of GTV
boxplot(data$gtv, xlabel="x", ylabel="y", horizontal=FALSE, range=0.1, border=c("blue"), col=c("red"))  # Boxplot with specific settings
boxplot(data$gtv, xlabel="x", ylabel="y", horizontal=FALSE, range=0.2, border=c("blue"), col=c("red"))  # Boxplot with range=0.2
boxplot(data$gtv, xlabel="x", ylabel="y", horizontal=FALSE, range=0.05, border=c("blue"), col=c("red"))  # Boxplot with range=0.05

# As range decreases, spread becomes smaller and data is more clustered

# lab5.3.j
boxplot(data$gtv)  # Boxplot of GTV data
boxplot(data$ki)  # Boxplot of KI data
boxplot(data$time)  # Boxplot of Time data

# Comparing all three
boxplot(data$gtv, data$ki, data$time, 
        names = c("GTV", "KI", "Time"), 
        main = "Boxplot Comparison of GTV, KI, and Time", 
        ylab = "Values")

# The broadest distribution is time with the widest spread and interquartile range

# Lab5.4.a
filter1 = subset(data, data$gtv > 20)  # Filter rows where GTV > 20
print(filter1)
print(dim(filter1))  # Print dimensions of the filtered data

# lab5.4.b
filter2 = data[c(1, 3, 8, 9, 13, 14, 18, 21)]  # Filter rows by specific indices
print(filter2)

# lab5.4.c
filter3_indices = which(data$sex == "Female")  # Get indices where sex is Female
filter3 = data[filter3_indices,]  # Build a subset based on these indices
print(filter3)

# We can also do this by the below method
filter4 = subset(data, data$sex == "Female")  # Filter rows where sex is Female
print(filter4)

# lab5.4.d
data$newcol = data$gtv * data$ki / 234  # Create a new column using the formula
new_df = data[, c("gtv", "ki", "newcol")]  # Create a new dataframe with selected columns
print(new_df)

# lab5.4.e
filter5_indices = which(data$sex == "Female")  # Get indices where sex is Female
filter5 = data[filter5_indices, ]  # Build a subset of female data
write.csv(filter5, file = "/home/ibab/R/lab4_female_BrainCancer.csv", row.names = FALSE)  # Write the subset to a CSV file

# Using Heart.csv
# lab5.1
data1 = read.csv("/home/ibab/Downloads/Heart.csv", header = TRUE)  # Read Heart.csv

# lab5.2
# LAB5.2.a
print(dim(data1))  # Print dimensions (rows and columns) of heart data
print(length(data1))  # Print number of columns in heart data

# lab5.2.b
print(colnames(data1))  # Print column names of heart data

# lab5.2.c
print(rownames(data1))  # Print row names of heart data

# lab5.2.d
print(head(data1, n=30))  # Print first 30 rows of heart data

# lab5.2.e
# Print frequency table for RestBP column
print(table(data1$RestBP))

# lab5.2.f
# Convert categorical columns to factors with specific levels
data1$ChestPain <- factor(data1$ChestPain, levels = c("typical", "asymptomatic", "nonalignal", "nontypical"))
data1$Thal <- factor(data1$Thal, levels = c("fixed", "normal", "reversable"))
data1$AHD <- factor(data1$AHD, levels = c("Yes", "No"))

# There are 3 categorical variables present

# lab5.2.g
# The categorical values are ChestPain, Thal, AHD

# lab5.2.h
# Print the number of levels and the levels of each categorical variable
print(nlevels(data1$ChestPain))
print(nlevels(data1$Thal))
print(nlevels(data1$AHD))
print(levels(data1$ChestPain))
print(levels(data1$Thal))
print(levels(data1$AHD))

# lab5.3.a
print(mean(data1$Chol))  # Print mean of Chol column

# lab5.3.b
print(mean(data1$RestBP))  # Print mean of RestBP column

# lab5.3.c
print(median(data1$Chol))  # Print median of Chol column

# median is slightly less than mean indicating left skewness

# lab5.3.d
print(names(which.max(table(data1$Chol))))  # Print mode of Chol (most frequent value)

# Distribution is not symmetric

# lab5.3.e
print(sd(data1$Chol))  # Print standard deviation of Chol

# lab5.3.f
summary(data1$Chol)  # Print summary of Chol data
hist(data1$Chol)  # Plot histogram for Chol

# The histogram agrees with mean, median, mode

# lab5.3.g
library(moments)  # Load moments package
skewness(data1$Chol)  # Print skewness of Chol

# lab5.3.h
kurtosis(data1$Chol)  # Print kurtosis of Chol

# Skewness and Kurtosis match with the expectations; they are more than 0

# lab5.3.i
boxplot(data1$Chol)  # Default boxplot of Chol data
boxplot(data1$Chol, xlabel="x", ylabel="y", horizontal=FALSE, range=0.1, border=c("blue"), col=c("red"))  # Boxplot with specific settings
boxplot(data1$Chol, xlabel="x", ylabel="y", horizontal=FALSE, range=0.2, border=c("blue"), col=c("red"))  # Boxplot with range=0.2
boxplot(data1$Chol, xlabel="x", ylabel="y", horizontal=FALSE, range=0.05, border=c("blue"), col=c("red"))  # Boxplot with range=0.05

# As range decreases, spread becomes smaller and data is more clustered

# lab5.3.j
boxplot(data1$Chol)  # Boxplot of Chol data
boxplot(data1$RestBP)  # Boxplot of RestBP data
boxplot(data1$MaxHR)  # Boxplot of MaxHR data

# Comparing all three
boxplot(data1$Chol, data1$RestBP, data1$MaxHR, 
        names = c("Chol", "RestBp", "MaxHR"), 
        main = "Boxplot Comparison of Chol, RestBP, and MaxHR", 
        ylab = "Values")

# The broadest distribution is Chol with the widest spread and interquartile range

# Lab5.4.a
filter1 = subset(data1, data1$Chol >= 20)  # Filter rows where Chol >= 20
print(filter1)
print(dim(filter1))  # Print dimensions of the filtered data

# lab5.4.b
filter3 = data1[c(2, 4, 6, 9, 23, 28, 31), ]  # Filter rows by specific indices
print(filter3)

# lab5.4.c
filter2 = unique(subset(data1, data1$ChestPain == "asymptomatic"))  # Filter unique rows where ChestPain is asymptomatic
print(filter2)

# lab5.4.d
new_data1 <- data.frame(Chol = data1$Chol, RestBP = data1$RestBP, new_column = data1$Chol * data1$RestBP / 234)  # Create new dataframe with calculated column
print(dim(new_data1))  # Print dimensions of the new dataframe
print(new_data1)  # Print new dataframe

# lab5.4.e
filter4_ind = which(data1$Sex == "1")  # Get indices where Sex is 1
print(filter4_ind)

filter4 = data1[filter4_ind, ]  # Build a subset of data based on these indices
print(filter4)

write.csv(filter4, file = "/home/ibab/R/lab4_female_Heart.csv")  # Write the filtered data to a CSV file












