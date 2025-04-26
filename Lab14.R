###PART 1
#Question1
# Step 1: Define the means and standard errors
means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)

# Step 2: Create the bar plot
bar_labels <- c("A", "B", "C")
bar_pos <- barplot(means,
                   names.arg = bar_labels,
                   col = "grey",
                   ylim = c(0, max(means + stderr) + 2),  
                   main = "Errors on bar plot",
                   xlab = "Groups",                     
                   ylab = "Mean Value")

# Step 3: Add error bars using arrows()
arrows(bar_pos, means + stderr,
       bar_pos, means - stderr,
       angle = 90, code = 3, length = 0.06, col = "red")

#Question2
#defining x,y,errors
x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

#plot the points
plot(x, y,
     pch = 16, 
     type = "b",
     col = "blue",
     xlab = "Concentration",      
     ylab = "Optical Activity",   
     main = "Error bars on data points", 
     ylim = c(min(y - errors) - 1, max(y + errors) + 1)) 

arrows(x , y + errors ,
       x , y - errors,
       angle = 90, code = 3, length = 0.06, col = "red")

#Question 3
x <- c(10,20,30,40,50,60,70,80,90,100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov_xy <- cov(x, y)
print(paste("Covariance:", cov_xy))
cor_xy <- cor(x, y)
print(paste("Correlation (Pearson's r):", cor_xy))
cor(longley) #multivariate data

###PART 2 ONE SAMPLE TESTS
#Question1 One sample Z test
#a) writing the z test function
one_sample_Ztest <- function(x, sigma, muzero, alpha, null) {
  n <- length(x)
  xbar <- mean(x)
  stderr <- sigma / sqrt(n)
  # Z statistic
  z_value <- (xbar - muzero) / stderr
  # p-value based on null hypothesis
  if (null == "equal") { #two test
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis: μ not equal to μ0",
                         "Fail to reject the null hypothesis: μ = μ0")
  } else if (null == "less_than_or_equal") { #right tail
    p_value <- 1 - pnorm(z_value)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis: μ <= μ0 (i.e., μ > μ0)",
                         "Fail to reject the null hypothesis: μ <= μ0")
  } else if (null == "more_than_or_equal") { #left tail
    p_value <- pnorm(z_value)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis: μ >= μ0 (i.e., μ < μ0)",
                         "Fail to reject the null hypothesis: μ >= μ0")
  } else {
    stop("Invalid null hypothesis type. Choose from 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  # Return Z-value, p-value and conclusion
  return(list(
    Z_value = z_value,
    P_value = p_value,
    Conclusion = conclusion
  ))
}
#b) testing the function
# Given data
x <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
       137.4, 145.6, 135.6, 135.4, 121.5)
# Parameters
muzero <- 124.6
sigma <- 14.5
alpha <- 0.05
# Run test
result <- one_sample_Ztest(x, sigma, muzero, alpha, null = "equal")
# Print the result
print(result)


#Question2 One sample t test
#a) writing the t test function
one_sample_t_test <- function(x, muzero, alpha, null) {
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  stderr <- s / sqrt(n)
  # t-statistic
  t_value <- (xbar - muzero) / stderr
  df <- n - 1  # degrees of freedom
  # p-value and conclusion
  if (null == "equal") {
    p_value <- 2 * (1 - pt(abs(t_value), df))
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis: μ not equal to μ0",
                         "Fail to reject the null hypothesis: insufficient evidence to say μ not equal to μ0")
  } else if (null == "less_than_or_equal") {
    p_value <- 1 - pt(t_value, df)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis: μ ≤ μ0 (i.e., μ > μ0)",
                         "Fail to reject the null hypothesis: insufficient evidence to say μ > μ0")
  } else if (null == "more_than_or_equal") {
    p_value <- pt(t_value, df)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis: μ ≥ μ0 (i.e., μ < μ0)",
                         "Fail to reject the null hypothesis: insufficient evidence to say μ < μ0")
  } else {
    stop("Invalid null hypothesis type. Choose from 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  # Return results
  return(list(
    T_value = t_value,
    P_value = p_value,
    Conclusion = conclusion
  ))
}
#b) testing the function
x <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)
muzero <- 100
alpha <- 0.05
result <- one_sample_t_test(x, muzero, alpha, null = "equal")
print(result)


#Question 3 One sample proportion test
# Parameters
x <- 710
n <- 2600
p <- 0.25
alternative <- "greater"
# Exact binomial test
print("Exact Binomial Test (binom.test):")
binom_result <- binom.test(x, n, p = p, alternative = alternative)
print(binom_result)
# Proportion test using normal approximation
print("Proportion Test (prop.test):")
prop_result <- prop.test(x, n, p = p, alternative = alternative, correct = TRUE)
print(prop_result)


#Question 4 one sample variance test
#a) writing the function
one_sample_variance_test <- function(x, test_sigma, alpha) {
  n <- length(x)  # Sample size
  s_squared <- var(x)  # Sample variance
  chi_squared_statistic <- (n - 1) * s_squared / test_sigma^2  # Chi-square test statistic
  
  # Find the critical values
  lower_critical_value <- qchisq(alpha / 2, df = n - 1)  # Left tail critical value
  upper_critical_value <- qchisq(1 - alpha / 2, df = n - 1)  # Right tail critical value
  
  # Conclusion based on the test statistic
  if (chi_squared_statistic < lower_critical_value || chi_squared_statistic > upper_critical_value) {
    conclusion <- "Reject the null hypothesis: The variance is not equal to the hypothesized value."
  } else {
    conclusion <- "Fail to reject the null hypothesis: The variance is equal to the hypothesized value."
  }
  
  # Return the test statistic, critical values, and conclusion
  return(list(
    Chi_Squared_Statistic = chi_squared_statistic,
    Lower_Critical_Value = lower_critical_value,
    Upper_Critical_Value = upper_critical_value,
    Conclusion = conclusion
  ))
}
#b)testing the function
# Given data
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Hypothesized population standard deviation (σ)
test_sigma <- 29
# Significance level
alpha <- 0.05
# Run the variance test
result <- one_sample_variance_test(x, test_sigma, alpha)
# Print the result
print(result)


#Question 5  Wilcoxon test
# Given data
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)
# Hypothesized median value (μ0)
mu0 <- 160
# Perform the one-sample Wilcoxon signed-rank test
wilcox.test(x,y=NULL,alternative,mu=mu0,paired=FALSE,
            exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
# Print the result
print(result)


###PART 3
#Question 1
#a) writing the function
# Function to perform a two-sample Z test
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null) {
  # Calculate the means of the two samples
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  # Calculate the sample sizes
  n1 <- length(x1)
  n2 <- length(x2)
  # Calculate the Z statistic for the two-sample Z test
  z_stat <- (mean_x1 - mean_x2) / sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  # Determine the critical value and p-value based on the null hypothesis
  if (null == 'μ1 ≥ μ2') {  # One-sided test (H0: μ1 ≥ μ2, alternative: μ1 < μ2)
    z_critical <- qnorm(1 - alpha)  # Right-tail critical value
    p_value <- 1 - pnorm(z_stat)  # p-value for one-sided test
  } else if (null == 'μ1 = μ2') {  # Two-sided test (H0: μ1 = μ2, alternative: μ1 ≠ μ2)
    z_critical <- qnorm(1 - alpha / 2)  # Two-tailed critical value
    p_value <- 2 * (1 - pnorm(abs(z_stat)))  # p-value for two-sided test
  }
  # Make the conclusion based on the comparison of Z statistic and critical value
  if (abs(z_stat) > z_critical) {
    conclusion <- "Reject the null hypothesis"
  } else {
    conclusion <- "Fail to reject the null hypothesis"
  }
  # Return the results of the Z test
  return(list(z_statistic = z_stat, z_critical = z_critical, p_value = p_value, conclusion = conclusion))
}
#b)Example
# Load the data from the file 'two-sample.dat' 
x1 = c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
        246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
        174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
        231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
        173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
        218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)
x2 = c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
        206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
        233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
        178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
        243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
        267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)
# Known standard deviations for the two samples
sigma_x1 <- 24.6
sigma_x2 <- 27.8
# Significance level
alpha <- 0.05
# Test the null hypothesis that μ1 ≥ μ2
result <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, 'μ1 >= μ2')
# Print the results of the Z test
print(result)


#Question 2
#a)
# Welch's t-test for two independent samples with unequal variances
# Define the function to perform Welch's t-test and conclude
welch_t_test_conclusion <- function(Xvar, Yvar) {
  # Perform Welch's t-test
  result_welch <- t.test(x = Xvar, y = Yvar, 
                         alternative = "two.sided", 
                         mu = 0, 
                         paired = FALSE, 
                         var.equal = FALSE, 
                         conf.level = 0.95)
  # Print the result
  print(result_welch)
  # Conclusion based on p-value
  if (result_welch$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: There is a significant difference between the means of the two groups."
  } else {
    conclusion <- "Fail to reject the null hypothesis: There is no significant difference between the means of the two groups."
  }
  # Return the conclusion
  return(conclusion)
}
# Sample data
Xvar <- c(4.95, 5.37, 4.70, 4.96, 4.72, 5.17, 5.28, 5.12, 5.26, 5.48)
Yvar <- c(4.65, 4.86, 4.57, 4.56, 4.96, 4.63, 5.04, 4.92, 5.37, 4.58, 4.26, 4.40)
# Call the function and get the conclusion
result <- welch_t_test_conclusion(Xvar, Yvar)
# Print the conclusion
print(result)
#b)
# Paired t-test: comparing before and after measurements
# Define the function to perform the paired t-test and conclude
paired_t_test_conclusion <- function(data_before, data_after) {
  # Perform paired t-test
  result_paired <- t.test(x = data_before, y = data_after,
                          alternative = "two.sided",
                          mu = 0,
                          paired = TRUE,
                          var.equal = FALSE, 
                          conf.level = 0.95)
  # Print the result
  print(result_paired)
  # Conclusion based on p-value
  if (result_paired$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: There is a significant difference in the means before and after the treatment."
  } else {
    conclusion <- "Fail to reject the null hypothesis: There is no significant difference in the means before and after the treatment."
  }
  # Return the conclusion
  return(conclusion)
}
# Sample data
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)
# Call the function and get the conclusion
result <- paired_t_test_conclusion(data_before, data_after)
# Print the conclusion
print(result)



#Question 3
#a)
proportion_test_conclusion <- function(successes, totals) {
  # Perform the proportion test
  result_prop <- prop.test(x = successes,
                           n = totals,
                           p = NULL,
                           alternative = "two.sided",
                           correct = TRUE)
  # Print the result of the test
  print("Proportion Test Result:")
  print(result_prop)
  # Conclusion based on p-value
  if (result_prop$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: The proportions of people who said 'Yes' to using antibiotics are significantly different between men and women."
  } else {
    conclusion <- "Fail to reject the null hypothesis: The proportions of people who said 'Yes' to using antibiotics are not significantly different between men and women."
  }
  
  # Print the conclusion
  print("Conclusion:")
  print(conclusion)
}
# Number of people who said "Yes" to using antibiotics
successes <- c(520, 550)  # Men, Women
# Total number of people surveyed
totals <- c(600, 600)
# Call the function and get the conclusion
proportion_test_conclusion(successes, totals)


#b)
# Creating the 2x2 table(matrix) based on the given data
tobacco_data <- matrix(c(11, 17, 42, 39), 
                       nrow = 2, 
                       byrow = TRUE)
# Assign row and column names (optional, for better display)
rownames(tobacco_data) <- c("Higher Income", "Lower Income")
colnames(tobacco_data) <- c("Tobacco Abuse", "No Abuse")
# Run Fisher's Exact Test
fisher_test_conclusion <- function(data_matrix) {
  result <- fisher.test(data_matrix,
                        alternative = "two.sided",
                        conf.int = TRUE,
                        conf.level = 0.95)
  print(result)
  # Conclusion
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: There is a significant association between the variables."
  } else {
    conclusion <- "Fail to reject the null hypothesis: There is no significant association between the variables."
  }
  return(conclusion)
}
fisher_result <- fisher_test_conclusion(tobacco_data)
print(fisher_result)



#Question 4
#a)
# Function to perform two-sample variance test using F-distribution
two_sample_variance_test <- function(x, y, alpha) {
  # Calculate variances and sample sizes
  var_x <- var(x)
  var_y <- var(y)
  n_x <- length(x)
  n_y <- length(y)
  # Calculate F-statistic (larger variance / smaller variance)
  if (var_x > var_y) {
    F_stat <- var_x / var_y
    df1 <- n_x - 1
    df2 <- n_y - 1
  } else {
    F_stat <- var_y / var_x
    df1 <- n_y - 1
    df2 <- n_x - 1
  }
  # Calculate p-value from F-distribution
  p_value <- 2 * (1 - pf(F_stat, df1, df2))
  #Critical value for two-tailed test
  critical_upper <- qf(1 - alpha/2, df1, df2)
  critical_lower <- qf(alpha/2, df1, df2)
  # Conclusion
  if (F_stat < critical_lower || F_stat > critical_upper) {
    conclusion <- "Reject H0: Variances are significantly different."
  } else {
    conclusion <- "Fail to reject H0: No significant difference in variances."
  }
  # Output 
  print(paste("F-statistic:", round(F_stat, 4)))
  print(paste("Degrees of Freedom:", df1, "and", df2))
  print(paste("p-value:", round(p_value, 4)))
  print(paste("Conclusion:", conclusion))
}
#b)testing the function
x <- c(1067.7, 984.3, 998.8, 1025.9, 1060.9, 959.1, 1013.8, 1047.0, 987.8, 1051.0, 885.2, 1049.5, 1098.2, 1001.5, 1011.1, 991.6)
y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0, 1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)
# Set alpha level
alpha <- 0.05
# Call the function
two_sample_variance_test(x, y, alpha)


#Question 5
# Function to perform Wilcoxon signed-rank test 
wilcoxon_conclusion <- function(before, after) {
  result <- wilcox.test(before, after,
                        paired = TRUE,
                        alternative = "greater",
                        conf.level = 0.95,
                        exact = FALSE)
  print(result)
  # Conclusion based on p-value
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: Pre-therapy scores are significantly greater than Post-therapy scores."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant evidence that Pre-therapy scores are greater than Post-therapy scores."
  }
  return(conclusion)
}
# Define the paired samples
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)
# Call the function 
wilcox_result <- wilcoxon_conclusion(Pre_therapy, Post_therapy)
print(wilcox_result)


#Question 6
# Define the function for Wilcoxon Rank Sum Test with conclusion
wilcoxon_rank_sum_conclusion <- function(group1, group2) {
  result <- wilcox.test(group1, group2,
                        alternative = "less",
                        conf.level = 0.95)
  print(result)
  # Conclusion based on p-value
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: The placebo group has significantly lower values than the drug group."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant evidence that the placebo group has lower values than the drug group."
  }
  return(conclusion)
}
# Sample data 
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)
# Call the function
wilcox_rs_result <- wilcoxon_rank_sum_conclusion(placebo, drug)
print(wilcox_rs_result)


#Question 7
# Define the function to perform Kruskal-Wallis test and give conclusion
kruskal_test_conclusion <- function(x, y) {
  result <- kruskal.test(x, y)
  print(result)
  # Conclusion based on p-value
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: At least one group differs significantly from the others."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant difference among the groups."
  }
  return(conclusion)
}
# Group data 
group1 <- c(220, 214, 203, 184, 186, 200, 165)
group2 <- c(262, 193, 225, 200, 164, 266, 179)
group3 <- c(272, 192, 190, 208, 231, 235, 141)
group4 <- c(190, 255, 247, 278, 230, 269, 289)
# Combine into a single vector
x <- c(group1, group2, group3, group4)
# Create factor labels for each group
y <- factor(rep(c("Group1", "Group2", "Group3", "Group4"), each = 7))
# Call the function and print the conclusion
kruskal_result <- kruskal_test_conclusion(x, y)
print(kruskal_result)


#Question 8
# Chi-square Goodness-of-Fit (GoF) Test Function
gof_test <- function(observed, expected, alpha = 0.05) {
  #Calculate the test statistic
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  #Degrees of freedom = number of categories - 1
  df <- length(observed) - 1
  #Critical value from chi-square distribution
  critical_val <- qchisq(1 - alpha, df)
  #Compute the p-value
  p_val <- 1 - pchisq(chi_sq_stat, df)
  #Print the results
  cat("Chi-square statistic:", round(chi_sq_stat, 3), "\n")
  cat("Degrees of freedom:", df, "\n")
  cat("Critical value at alpha =", alpha, ":", round(critical_val, 3), "\n")
  cat("P-value:", round(p_val, 4), "\n")
  # Decision
  if (chi_sq_stat > critical_val) {
    cat("Result: Reject the null hypothesis.\n")
  } else {
    cat("Result: Do not reject the null hypothesis.\n")
  }
}
#testing the function
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)
gof_test(observed, expected)



#####PART 4
#Question 1
#a) 
# Read the Titanic data
titanicData <- read.csv("/home/ibab/R/titanic.csv")
# View the first few rows
head(titanicData)
# Setting up a 3x1 plot grid for 3 classes
par(mfrow = c(3, 1))
# Plot histogram for 1st class
hist(titanicData$age[titanicData$passenger_class == '1st'],
     breaks = 40, main = "1st Class Passengers", 
     xlab = "Age", col = "lightblue")
#Plot histogram for 2nd class
hist(titanicData$age[titanicData$passenger_class == '2nd'],
     breaks = 40, main = "2nd Class Passengers",
     xlab = "Age", col="salmon")
#Plot histogram for 3rd class
hist(titanicData$age[titanicData$passenger_class == '3rd'],
     breaks = 40, main = "3rd Class Passengers",
     xlab = "Age", col="lightgreen") 
#all three have similar variances/spread(they are not that different)
#b)
# Loading required libraries
library(dplyr)
titanic_by_passenger_class<- group_by(titanicData,passenger_class)
summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),
          group_sd=sd(age,na.rm=TRUE))
print("Conclusion: The standard deviations across the three classes are fairly similar.")
print("Thus, the assumption of equal variances is reasonably satisfied, and we can proceed with ANOVA.")
# (c) Fit ANOVA model and print conclusion
run_anova <- function(data) {
  lmresults <- lm(age ~ passenger_class, data = data)
  anova_result <- anova(lmresults)
  print(anova_result)
  
  p_val <- anova_result$`Pr(>F)`[1]
  print("[ANOVA Conclusion]:")
  if (!is.na(p_val) && p_val < 0.05) {
    print(paste(" The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("At least one passenger class has a significantly different mean age.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print(" No significant difference in mean age among passenger classes.")
  }
  return(lmresults)
}
lm_model <- run_anova(titanicData)
# (d) Tukey HSD post-hoc test
tukey_result <- TukeyHSD(aov(lm_model))
print(tukey_result)
# (e) Kruskal-Wallis non-parametric test
run_kruskal_test <- function(data) {
  kw_result <- kruskal.test(age ~ passenger_class, data = data)
  print(kw_result)
  
  print("[Kruskal-Wallis Conclusion]:")
  if (kw_result$p.value < 0.05) {
    print(paste("p-value =", round(kw_result$p.value, 4), "< 0.05 → Reject H0."))
    print("At least one group differs significantly in age (non-parametric evidence).")
  } else {
    print(paste("p-value =", round(kw_result$p.value, 4), ">= 0.05 → Fail to reject H0."))
    print("→ No significant difference in age among classes (non-parametric test).")
  }
}
run_kruskal_test(titanicData)

#Question 2
# (a) Read the Cuckoo Egg Size Data
cuckooData <- read.csv("/home/ibab/R/cuckooeggs.csv")
# View the first few rows
head(cuckooData)

# Setting up a 3x1 plot grid (assuming 3 host species)
par(mfrow = c(3, 1))
# Plot histogram for each host species
hist(cuckooData$egg_length[cuckooData$host_species == 'Hedge Sparrow'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "lightgreen")
hist(cuckooData$egg_length[cuckooData$host_species == 'Meadow Pipit'],
     breaks = 20, main = "Meadow Pipit Host", 
     xlab = "Cuckoo Egg Length", col = "lightblue")
hist(cuckooData$egg_length[cuckooData$host_species == 'Pied Wagtail'],
     breaks = 20, main = "Pied Wagtail Host", 
     xlab = "Cuckoo Egg Length", col = "salmon")
hist(cuckooData$egg_length[cuckooData$host_species == 'Robin'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "lightgreen")
hist(cuckooData$egg_length[cuckooData$host_species == 'Tree Pipit'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "blue")
hist(cuckooData$egg_length[cuckooData$host_species == 'Wren'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "red")
#the spreads (variances) look reasonably similar, but we'll check using summary statistics next.

#b)
# Group by host species
cuckoo_by_host <- group_by(cuckooData, host_species)

# Summarize: mean and sd
summary_stats <- summarise(cuckoo_by_host,
                           group_mean = mean(egg_length, na.rm = TRUE),
                           group_sd = sd(egg_length, na.rm = TRUE))
print(summary_stats)
print("The standard deviations are similar, we can proceed with ANOVA testing.")
#c)
# Fit the ANOVA model
run_anova <- function(data) {
  # Perform ANOVA using lm (linear model)
  lm_result <- lm(egg_length ~ host_species, data = data)
  anova_result <- anova(lm_result)
  
  # Print the ANOVA result
  print(anova_result)
  
  # Extract p-value and make a conclusion
  p_val <- anova_result$`Pr(>F)`[1]
  
  print("[ANOVA Conclusion]:")
  if (!is.na(p_val) && p_val < 0.05) {
    print(paste(" The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("At least one host species has a significantly different mean egg length.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print("No significant difference in egg length among host species.")
  }
  
  return(lm_result)
}

# Run the ANOVA
lm_model <- run_anova(cuckooData)
#d)
# Tukey-Kramer post-hoc test to determine which host species are significantly different
tukey_result <- TukeyHSD(aov(lm_model))
print(tukey_result)
# Conclusion based on Tukey-Kramer test
print("[Tukey-Kramer Conclusion]:")
# The comparisons are stored in tukey_result$host_species 
# Check if p-values are available and filter for significant differences (p.adj < 0.05)
# We extract the p-values from the result
significant_comparisons <- tukey_comparisons[tukey_comparisons[, "p adj"] < 0.05, ]
# Print significant comparisons if there are any
if (nrow(significant_comparisons) > 0) {
  print("There are significant differences in mean egg lengths between the following pairs of host species:")
  print(significant_comparisons)
} else {
  print("No significant differences were found between any pair of host species.")
}


#Question 3
#a)
# Load the data
malaria_maize_data <- read.csv("/home/ibab/R/malaria vs maize.csv")

# View the first few rows of the data
head(malaria_maize_data)

# Setting up a 3x1 plot grid for 3 levels of maize production
par(mfrow = c(3, 1))

# Plot histogram for low maize yield
hist(malaria_maize_data$incidence_rate[malaria_maize_data$maize_yield == 'Low'],
     breaks = 3, main = "Low Maize Yield", xlab = "Malaria Incidence (per 10,000)", col = "lightblue")

# Plot histogram for medium maize yield
hist(malaria_maize_data$incidence_rate[malaria_maize_data$maize_yield == 'Medium'],
     breaks = 3, main = "Medium Maize Yield", xlab = "Malaria Incidence (per 10,000)", col = "salmon")

# Plot histogram for high maize yield
hist(malaria_maize_data$incidence_rate[malaria_maize_data$maize_yield == 'High'],
     breaks = 3, main = "High Maize Yield", xlab = "Malaria Incidence (per 10,000)", col = "lightgreen")

#b)
# Group by maize yield level
maize_by_yield <- group_by(malaria_maize_data, maize_yield)

# Summarize: mean and standard deviation
summary_stats_maize <- summarise(maize_by_yield,
                                 group_mean = mean(incidence_rate_per_ten_thousand, na.rm = TRUE),
                                 group_sd = sd(incidence_rate_per_ten_thousand, na.rm = TRUE))
print(summary_stats_maize)

print("Based on the standard deviations, spreads seem quite different.")
print("This suggests violation of ANOVA's equal variance assumption.")

#c)
# (c) Log-transform the Malaria Incidence Rate and Redraw Histograms

# Create a new variable for the log-transformed incidence rate
malaria_maize_data$log_incidence <- log(malaria_maize_data$incidence_rate_per_ten_thousand)

# Set up a 3x1 plot grid again
par(mfrow = c(3, 1))

# Plot histograms for log-transformed data
hist(malaria_maize_data$log_incidence[malaria_maize_data$maize_yield == 'High'],
     breaks = 5, main = "High Maize Yield (Log Incidence)", 
     xlab = "Log Malaria Incidence Rate", col = "lightgreen")
hist(malaria_maize_data$log_incidence[malaria_maize_data$maize_yield == 'Medium'],
     breaks = 5, main = "Medium Maize Yield (Log Incidence)", 
     xlab = "Log Malaria Incidence Rate", col = "lightblue")
hist(malaria_maize_data$log_incidence[malaria_maize_data$maize_yield == 'Low'],
     breaks = 5, main = "Low Maize Yield (Log Incidence)", 
     xlab = "Log Malaria Incidence Rate", col = "salmon")
# the spreads after log transformation look more similar.
# Group by maize yield
maize_by_yield_log <- group_by(malaria_maize_data, maize_yield)

# Summarise mean and standard deviation for log-transformed data
summary_stats_maize_log <- summarise(maize_by_yield_log,
                                     group_mean_log = mean(log_incidence, na.rm = TRUE),
                                     group_sd_log = sd(log_incidence, na.rm = TRUE))
print(summary_stats_maize_log)

print("After log-transformation, standard deviations are more similar.")
print("The data now better meet ANOVA assumptions.")

#d)
# Define a function to run ANOVA
run_anova_maize <- function(data) {
  # Fit linear model
  lm_result_maize <- lm(log_incidence ~ maize_yield, data = data)
  anova_result_maize <- anova(lm_result_maize)
  
  # Print ANOVA table
  print(anova_result_maize)
  
  # Extract p-value and make conclusion
  p_val_maize <- anova_result_maize$`Pr(>F)`[1]
  
  print("[ANOVA Conclusion]:")
  if (!is.na(p_val_maize) && p_val_maize < 0.05) {
    print(paste("The p-value is", round(p_val_maize, 4), "< 0.05, so we reject H0."))
    print("There is a significant association between maize yield and malaria incidence.")
  } else {
    print(paste("The p-value is", round(p_val_maize, 4), ">= 0.05, so we fail to reject H0."))
    print("No significant association between maize yield and malaria incidence.")
  }
  
  return(lm_result_maize)
}
# Run the ANOVA
lm_model_maize <- run_anova_maize(malaria_maize_data)



#Question 4
#a)
# Load the data
circadian_data <- read.csv("/home/ibab/R/circadian mutant health.csv")

# Check the first few rows
head(circadian_data)

# Set up a 3x1 plot grid
par(mfrow = c(3, 1))

# Plot histograms for each genotype
hist(circadian_data$days_to_death[circadian_data$genotype == "tim01 (rescued)"],
     breaks = 5, main = "Normal Flies", 
     xlab = "Days to Death", col = "lightgreen")

hist(circadian_data$days_to_death[circadian_data$genotype == "tim01"],
     breaks = 5, main = "tim01 Mutant Flies", 
     xlab = "Days to Death", col = "lightblue")

hist(circadian_data$days_to_death[circadian_data$genotype == "wild type"],
     breaks = 5, main = "Heterozygous Flies", 
     xlab = "Days to Death", col = "salmon")
#b)
perform_kruskal_test <- function(data) {
  # Perform Kruskal-Wallis test
  kruskal_test_result <- kruskal.test(days_to_death ~ genotype, data = data)
  print(kruskal_test_result)
  # Extract p-value
  p_val <- kruskal_test_result$p.value
  # Print the conclusion based on p-value
  print("[Kruskal-Wallis Test Conclusion]:")
  if (p_val < 0.05) {
    print(paste("The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("There is a significant difference in lifespan between the groups of flies.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print("There is no significant difference in lifespan between the groups of flies.")
  }
}
# Example usage
perform_kruskal_test(circadian_data)

