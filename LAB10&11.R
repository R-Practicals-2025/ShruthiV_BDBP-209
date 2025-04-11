#LAB10&11

#Q1.1
x <- seq(1,100)
print(x)
s <- sample(x,10)  # Sample 10 values without replacement
print(s)
s2 <- sample(x,10,replace=TRUE)  # Sample 10 values with replacement
print(s2)

#Q1.2
install.packages("gtools")
library(gtools)
x <- c("A","B","C","D")
per <- permutations(n=length(x), r=3, v=x, repeats.allowed=TRUE)  # Permutations of 3 elements
print(per)
comb <- combinations(n=length(x), r=3, v=x)  # Combinations of 3 elements
print(comb)

#Q2.1
n <- 10 ; p <- 0.4 ; m <- 3

#a
print(dbinom(m,n,p))  # P(X=3) for Binomial(n=10, p=0.4)

#b
print(pbinom(m,n,p))  # P(X≤3) for Binomial(n=10, p=0.4)

#c
print(qbinom(0.8,n,p))  # Smallest m such that P(X≤m)≥0.8

#d
print(rbinom(5,n,p))  # 5 random samples from Binomial(n=10, p=0.4)

#e
x <- 0:n
a <- dbinom(x,n,p)
e <- dbinom(x,n,0.7)
plot(x, a, type="h", col="red", lwd=5, main="Binomial Distribution", xlab="Number of Successes", ylab="Probability")
lines(x, e, type="h", col="blue", lwd=5)
legend("topleft", legend=c("p=0.4", "p=0.7"), col=c("red", "blue"), lwd=2)

#f
r_100 <- rbinom(100, n, p)
r_1000 <- rbinom(1000, n, p)
f_100 <- table(r_100)
f_1000 <- table(r_1000)
par(mfrow=c(2,1))
barplot(f_100, col="lightblue", main="Histogram of 100 Samples", xlab="Number of Successes", ylab="Frequency", border="black")
barplot(f_1000, col="lightgreen", main="Histogram of 1000 Samples", xlab="Number of Successes", ylab="Frequency", border="black")

#Q2.2
N <- 100; K <- 70; n <- 12

#a
x <- 0:n
prob <- dhyper(x, N, N-K, n)
barplot(prob, names.arg=x, main="Hypergeometric Distribution", xlab="Number of Successes", ylab="Probability", col="lightgreen", border="black")

#b
print(round(phyper(10, N, N-K, n), 3))  # Cumulative probability P(X≤10)

#c
print(qhyper(0.9, N, N-K, n))  # Smallest m such that P(X≤m)≥0.9

#d
print(rhyper(5, N, N-K, n))  # 5 random samples from Hypergeometric(N=100, K=70, n=12)

#Q2.3
#a
par(mfrow=c(1,2))
p1 <- 0.3; p2 <- 0.8
x <- 1:10
d1 <- dgeom(x-1, p1)
d2 <- dgeom(x-1, p2)

barplot(d1, names.arg=x, main="Geometric Distribution (p=0.3)", xlab="Trial of First Success", ylab="Probability", col="salmon", border="black")
barplot(d2, names.arg=x, main="Geometric Distribution (p=0.8)", xlab="Trial of First Success", ylab="Probability", col="salmon", border="black")

#b
print(pgeom(3, p1))  # P(X≤4) for Geometric(p=0.3)

#c
print(qgeom(0.2, p1) + 1)  # Smallest trial number with cumulative probability ≥ 0.2

#d
print(rgeom(6, 0.4) + 1)  # 6 random samples from Geometric(p=0.4)

#Q2.4
p <- 0.3; r <- 3; y <- 5

#a
print(dnbinom(y, r, p))  # P(Y=5) for Negative Binomial(r=3, p=0.3)

#b
print(pnbinom(y, r, p))  # P(Y≤5)

#c
print(qnbinom(0.5, r, p))  # Median number of failures before r successes

#d
print(rnbinom(4, r, p))  # 4 random samples from Negative Binomial

#e
r <- 10; x <- 0:20
dist <- dnbinom(x, r, p)
barplot(dist, names.arg=x, main="Negative Binomial Distribution", xlab="Failures Before 10 Successes", ylab="Probability", col="lightblue", border="black")

#f
samples <- rnbinom(10000, r, p)
hist(samples, breaks=30, col="lightblue", border="black", main="Histogram of Negative Binomial Deviates", xlab="Failures Before 10 Successes", probability=TRUE)

#Q2.5
lambda <- 10; m <- 7

#a
print(dpois(m, lambda))  # P(X=7) for Poisson(λ=10)

#b
print(ppois(m, lambda))  # P(X≤7)

#c
n <- 10000; p <- 0.01
#p <- 0.05 ; n <- 5000
lambda <- n * p
x <- 0:10000
dist_bin <- dbinom(x, n, p)
dist_pois <- dpois(x, lambda)
par(mfrow=c(1,1))
#barplot(dist_bin, names.arg=x, col="blue", main="Binomial Distribution", xlab="X", ylab="Probability", border="black")
#barplot(dist_pois, names.arg=x, col="red", main="Poisson Approximation", xlab="X", ylab="Probability", border="black")

plot(x,dist_bin,type="h", col="blue", main="Binomial Distribution", xlab="X", ylab="Probability",xlim=c(50,140))
lines(x,dist_pois,col="red",lty=2,lwd=2,xlim=c(50,140))
#plot(x,dist_bin,type="h", col="blue", main="Binomial Distribution", xlab="X", ylab="Probability",xlim=c(0,n))
#lines(x,dist_pois,col="red",lty=2,lwd=2,xlim=c(0,n))

#d
print(qpois(0.22, lambda=10))  # Smallest m such that P(X≤m)≥0.22

#e
samples <- rpois(10000, lambda=9)
hist(samples, breaks=30, col="lightblue", border="black", main="Histogram of Poisson Deviates", xlab="X", probability=TRUE)

#Q2.6
#a
n <- 12
mean <- 12
sd <- 2
gauss_norm <- dnorm(n,mean,sd)
print(gauss_norm)
#b
z<- 2.0
cdf <- pnorm(z)
cdf_neg <- pnorm(-z)
print(cdf)
print(1 - cdf_neg)
#both are same
#c
x <- seq(mean - 4*sd,mean+4*sd,length=100)
y <- dnorm(x,mean,sd)

plot(x,y,type="l",main="Normal Distribution",xlab="X",ylab="Density")
legend("topleft", legend=c("Normal Curve"), col=c("blue"), lty=1, bty="o",border="black",cex=0.8)
#d
q75 <- qnorm(0.75, mean, sd)
print(q75)
#e

norm_samples <- rnorm(10000, mean, sd)
hist(norm_samples, probability = TRUE, col="salmon", main="Histogram of Normal Samples", xlab="X", ylab="Density")
curve(dnorm(x, mean, sd), col="blue", lwd=2, add=TRUE)

#f
n <- 100 ; p <- 0.5 ; np <- n*p
sigma_bin <- sqrt(np *(1-p))
m <- rbinom(10000, n, p)
W <- (m-np)/sigma_bin
hist(W, probability=TRUE, col="lightgray", main="Normalized Binomial Distribution", xlab="W", ylab="Density",breaks=40)
x_range <- seq(-1, 1, length = 100)  
curve(dnorm(x, mean=0, sd=1), col="blue", lwd=2, add=TRUE)

#g
# Setup 2x2 plotting grid
par(mfrow = c(2, 2), mar=c(4, 4, 3, 1))

# Lambda values to compare
lambdas <- c(1, 10, 100, 1000)

for (lambda in lambdas) {
  # x range around lambda ± 4*sqrt(lambda)
  x_min <- max(0, floor(lambda - 4 * sqrt(lambda)))
  x_max <- ceiling(lambda + 4 * sqrt(lambda))
  x_vals <- x_min:x_max
  
  # Poisson probabilities
  poisson_probs <- dpois(x_vals, lambda)
  
  # Convert x to Z-scores
  z_vals <- (x_vals - lambda) / sqrt(lambda)
  
  # Standard normal curve
  z_smooth <- seq(min(z_vals), max(z_vals), length.out = 500)
  normal_pdf <- dnorm(z_smooth)
  
  # Plot Poisson probabilities in Z-space
  plot(z_vals, poisson_probs, type = "h", lwd = 2, col = "darkgreen",
       main = paste("Poisson vs Standard Normal (λ =", lambda, ")"),
       xlab = "Z = (x - λ)/√λ", ylab = "Probability",
       ylim = c(0, max(c(poisson_probs, normal_pdf)) * 1.1))
  
  # Overlay standard normal curve
  lines(z_smooth, normal_pdf, col = "red", lwd = 2)
  
  # Legend
  legend("topright", legend = c("Poisson (Z)", "Standard Normal"),
         col = c("darkgreen", "red"), lty = c(1, 1), lwd = 2, bty = "n")
}



#h
library(MASS) #contains mvrnorm-multivariate normal random sampling
xy <- mvrnorm(1000, mu=c(50,60), Sigma=matrix(c(4,3.7,3.7,9), 2))
#i
var(xy)
#ii
x <- xy[,1]
y <- xy[,2]
print(x)
print(y)
plot(x, y, main="Scatterplot of x and y", xlab="x", ylab="y")
#iii
v1 <- var(x + y)
v2 <- var(x) + var(y)
print(v1) #both are not equal because thye are correlated
print(v2)
v3 <- var(x) +var(y)  + 2*cov(x,y) # this is equal to v1
print(v3)
#iv
cor_xy <- cor(x, y)
var_x <- var(x)
var_y <- var(y)
cov_calculated <- cor_xy * sqrt(var_x) * sqrt(var_y)
print(cov_calculated)
print(cov(x, y)) #both are same


#Q7
#a
a <- runif(5, min = 0, max = 1)
print(a)

#b
b <- runif(5, min = 50, max = 100)
print(b)

#c
# Generate 10,000 uniform deviates
c <- runif(10000, min = 1, max = 2)
# Plot histogram
hist(c,
     breaks = 20,            
     col = "skyblue",
     xlim = c(1, 2),          # Set x-axis limits from 1 to 2
     main = "Histogram of Uniform(1,2)",
     xlab = "Value")

#Q8
# (a) Density at x = 3, λ = 2
x <- 3
lambda <- 2
density_val <- dexp(x, rate = lambda)
print(paste("Density at x =", x, "with λ =", lambda, "is", density_val))

#(b) Quantile at CDF = 0.995 for λ = 2
quantile_val <- qexp(0.995, rate = lambda)
print(paste("Quantile at 0.995 CDF for λ =", lambda, "is", quantile_val))

#c
# X-axis range
x_vals <- seq(0, 1, length.out = 1000)

# Plot CDF for λ = 2
plot(x_vals, pexp(x_vals, rate = 2), type = "l", col = "blue", lwd = 2,
     main = "Exponential CDFs for Different λ",
     xlab = "x", ylab = "Cumulative Probability", ylim = c(0,1))

# Add λ = 10
lines(x_vals, pexp(x_vals, rate = 10), col = "red", lwd = 2)

# Add λ = 100
lines(x_vals, pexp(x_vals, rate = 100), col = "darkgreen", lwd = 2)

# Add legend
legend("bottomright", legend = c("λ = 2", "λ = 10", "λ = 100"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

#d 
set.seed(42)  # Optional for reproducibility
rexp(4, rate = 3)

#Q9
#a
# Set up a 1x2 plotting grid
par(mfrow=c(1,2))

# Plot 1: Vary alpha, fix theta = 4
curve(dgamma(x, shape=1, scale=4), from=0, to=50, col="black", lwd=2, ylab="Density", main="Varying α, θ=4")
curve(dgamma(x, shape=2, scale=4), add=TRUE, col="blue", lwd=2)
curve(dgamma(x, shape=3, scale=4), add=TRUE, col="red", lwd=2)
curve(dgamma(x, shape=4, scale=4), add=TRUE, col="magenta", lwd=2)
legend("topright", legend=c("α=1", "α=2", "α=3", "α=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

# Plot 2: Vary theta, fix alpha = 4
curve(dgamma(x, shape=4, scale=1), from=0, to=50, col="black", lwd=2, ylab="Density", main="Varying θ, α=4")
curve(dgamma(x, shape=4, scale=2), add=TRUE, col="blue", lwd=2)
curve(dgamma(x, shape=4, scale=3), add=TRUE, col="red", lwd=2)
curve(dgamma(x, shape=4, scale=4), add=TRUE, col="magenta", lwd=2)
legend("topright", legend=c("θ=1", "θ=2", "θ=3", "θ=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

#b
dgamma(6, shape=4, scale=1)

#c
pgamma(6, shape=4, scale=1)

#d
qgamma(0.95, shape=4, scale=1)

#e
set.seed(123)  # For reproducibility
samples <- rgamma(10000, shape=4, scale=1)
hist(samples, breaks=50, main="Histogram of Gamma(α=4, θ=1)", xlab="x", col="lightblue", border="black")


#Q10
#a
# Plot multiple chi-square PDFs on one graph
curve(dchisq(x, df=2), from=0, to=25, col="black", lwd=2, ylab="Density", main="Chi-square PDFs")
curve(dchisq(x, df=3), add=TRUE, col="blue", lwd=2)
curve(dchisq(x, df=5), add=TRUE, col="red", lwd=2)
curve(dchisq(x, df=10), add=TRUE, col="green", lwd=2)
legend("topright", legend=c("df=2", "df=3", "df=5", "df=10"), col=c("black", "blue", "red", "green"), lwd=2)

#b
dchisq(6, df=5)

#c
pchisq(6, df=10)

#d
qchisq(0.85, df=6)

#e
set.seed(123)
data <- rchisq(10000, df=6)
hist(data, breaks=30, col="lightblue", border="black", main="Histogram of χ², df=6", xlab="x")
text(20, 1200, "r=6", cex=1.5)  

#f
mu <- 2
sigma <- 1
x <- seq(0, 5, length=100)
Z2 <- ((x - mu)^2) / (sigma^2)

# Plot chi-square with df = 1
curve(dchisq(x, df=1), from=0, to=10, col="purple", lwd=2, main="χ² PDF with df = 1", ylab="Density")


###part 3- CENTRAL LIMIT THEOREM
#Q1
set.seed(123)

# (i) Generate 10,000 samples of size 5 from Uniform[0,10]
samples <- matrix(runif(10000 * 5, min = 0, max = 10), ncol = 5)
sample_means <- rowMeans(samples)

# (ii) Plot histogram to get bin width info
hist_out <- hist(sample_means, breaks = 50, col = "lightblue", 
                 main = "CLT: Uniform[0,10] Sample Means", xlab = "Sample Mean")

# (ii continued) Print mean and standard deviation
mean_sample <- mean(sample_means)
sd_sample <- sd(sample_means)
print(paste("Mean of sample means:", mean_sample))
print(paste("Standard deviation of sample means:", sd_sample))

# (iii) Create x values and compute normal PDF
x_vals <- seq(min(sample_means), max(sample_means), length.out = 100)
normal_pdf <- dnorm(x_vals, mean = mean_sample, sd = sd_sample)

# (iv) Scale using actual bin width
bin_width <- hist_out$breaks[2] - hist_out$breaks[1]
scaled_pdf <- normal_pdf * length(sample_means) * bin_width

# (v) Plot histogram and overlay correct normal curve
hist(sample_means, breaks = 50, col = "lightblue", 
     main = "CLT: Uniform[0,10] Sample Means", xlab = "Sample Mean", freq = TRUE)
lines(x_vals, scaled_pdf, col = "red", lwd = 2)
legend("topright", legend = c("Sample Means", "Normal Curve"), 
       col = c("lightblue", "red"), lwd = 2, fill = c("lightblue", NA))

#Q2
# (i) Single die roll – Uniform distribution
a <- sample(1:6, replace = TRUE, 10000)
hist(a,
     breaks = seq(0.5, 6.5, by = 1),
     col = "skyblue",
     main = "Single Die Roll (Uniform)",
     xlab = "Dice Value")

# (ii) Sum of two dice – Triangular distribution
b <- sample(1:6, replace = TRUE, 10000)
ab_sum <- a + b
hist(ab_sum,
     breaks = seq(1.5, 12.5, by = 1),
     col = "lightgreen",
     main = "Sum of Two Dice",
     xlab = "Sum")

# (iii) Sum of three dice – Bell starts to appear
c <- sample(1:6, replace = TRUE, 10000)
abc_sum <- a + b + c
hist(abc_sum,
     breaks = seq(2.5, 18.5, by = 1),
     col = "lightcoral",
     main = "Sum of Three Dice",
     xlab = "Sum")


# (iv) Sum of five dice – Nearly normal
d <- sample(1:6, replace = TRUE, 10000)
e <- sample(1:6, replace = TRUE, 10000)
abcde_sum <- a + b + c + d + e

# Plot histogram
hist(abcde_sum,
     breaks = seq(4.5, 30.5, by = 1),
     col = "orchid",
     main = "Sum of Five Dice",
     xlab = "Sum")

mu <- mean(abcde_sum)
sigma <- sd(abcde_sum)
x_vals <- seq(min(abcde_sum), max(abcde_sum), by = 0.1)
normal_pdf <- dnorm(x_vals, mean = mu, sd = sigma)

# Scale normal curve to match histogram (bin width = 1, so scale = 10000 * 1 = 10000)
lines(x_vals, normal_pdf * 10000, col = "darkblue", lwd = 2)


# Load necessary package
library(pROC)

# Step 1: Read the white wine data
white_wine <- read.csv("winequality-white.csv", sep=";")

# Step 2: Create binary columns for thresholds 6 to 10
thresholds <- 6:10
for (t in thresholds) {
  col_name <- paste0("quality_", t)
  white_wine[[col_name]] <- ifelse(white_wine$quality >= t, 1, 0)
}
# Step 3: Plot ROC curves for each threshold
colors <- c("red", "blue", "green", "purple", "orange")
plot(NULL, xlim=c(1, 0), ylim=c(0, 1), xlab="1 - Specificity", ylab="Sensitivity", main="ROC Curves for Different Thresholds")
abline(a=0, b=1, lwd=2, lty=2)  # identity line

for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  response <- white_wine[[paste0("quality_", t)]]
  roc_obj <- roc(response, white_wine$alcohol, legacy.axes=TRUE, ci=TRUE)
  plot(roc_obj, col=colors[i], add=TRUE, print.auc=TRUE, print.thres=TRUE)
}

legend("bottomright", legend=paste("Threshold >=", thresholds), col=colors, lwd=2)
