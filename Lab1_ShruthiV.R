#Lab1 jan3 2025
#Q1.1
x <- 2.7/2
print(x)
#Q1.2
x<- 2.7 %/% 2
print(x)
#Q1.3
x <- 10+5i/2
print(x)
#Q1.4
x <- round(2.5)
print(x)
#Q1.5
x <- round(-2.5)
print(x)
#Q1.6
x <- 2%/%4-1
print(x)
#Q1.7
x <- 3*2**2
print(x)
#Q1.8
x <- 3**2*2
print(x)
#Q1.9
x <- 7%/% 4
print(x)
#Q1.10
x <- 7%%4
print(x)
#Q1.11
x <- -7%%4
print(x)
#Q1.12
x <- trunc(5.7)
print(x)
#Q1.13
x <- trunc(-5.7)
print(x)

#Q2
#Create a function which adds 0.5 to the number and gives the floor of it
ceilingfunc <- function(x) floor(x+0.5)
result <- ceilingfunc(5.7)
print(result)

#Q3
a <- 1 ; b <- 2 ; c <- 4
x <- a & b
print(x)
y <- !(a<b) | (c>b)
print(y)

#Q4.1
x<- c(5,3,7,8)
print(x)
#Q4.2
is.integer(x)
#Q4.3
is.numeric(x)
#Q4.4
#integer() doesn't work for an existing vector so it gives an error
x<- integer(x)
print(x)
#Q4.5
x<-as.integer(x)
is.integer(x)
#x is converted into integer with as.integer(x) and it is checked with is.integer()
 
#Q5.1
x <- sqrt(2)
print(x)
#Q5.2
x*x ==2 
#The above gives a false because x*x is not exactly 2(rounding error of computer)
#Q5.3
y <- x*x-2
print(y)
#The above gives you a number because of rounding errors
z <- all.equal(x*x,2)
print(z)
#all.equal sees if they are close enough not equal to avoid the above issues.
 


