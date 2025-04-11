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
hist(W, probability=TRUE, col="lightgray", main="Normalized Binomial Distribution", xlab="W", ylab="Density")
x_range <- seq(-1, 1, length = 100)  
curve(dnorm(x, mean=0, sd=1), col="blue", lwd=2, add=TRUE)

#g
# Set up a 2x2 plotting grid
par(mfrow=c(2,2))

# Lambda values to test
lambdas <- c(1, 10, 100, 1000)

# Loop through each lambda
for (lambda in lambdas) {
  
  # Define x range: from around (λ - 4√λ) to (λ + 4√λ)
  x_min <- max(0, floor(lambda - 4 * sqrt(lambda)))
  x_max <- ceiling(lambda + 4 * sqrt(lambda))
  x_vals <- x_min:x_max
  
  # Compute Poisson PMF values
  poisson_probs <- dpois(x_vals, lambda)
  
  # Plot Poisson PMF
  plot(x_vals, poisson_probs, type="h", lwd=2, col="darkgreen",
       main=paste("Poisson vs Normal (λ =", lambda, ")"),
       xlab="x", ylab="Probability")
  
  # Compute Normal approximation (same mean & SD as Poisson)
  x_smooth <- seq(x_min, x_max, length=500)
  normal_approx <- dnorm(x_smooth, mean=lambda, sd=sqrt(lambda))
  
  # Overlay Normal curve
  lines(x_smooth, normal_approx, col="red", lwd=2)
  
  # Add legend
  legend("topright", legend=c("Poisson", "Normal Approx"), col=c("darkgreen", "red"), lty=1, lwd=2, bty="n")
  
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
     breaks = 50,            
     col = "skyblue",
     xlim = c(1, 2),          # Set x-axis limits from 1 to 2
     main = "Histogram of Uniform(1,2)",
     xlab = "Value")

pa












