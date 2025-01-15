#Q1
ops <- function (k,n){ #function to perform arthimatic operations 
  sum <- k + n
  diff <- k - n
  prod <- k * n
  quo <- k / n
  remain <- k %% n
  result = c(sum,diff,prod,quo,remain)
  print("The arithematic operations of the numbers are:")
  return(result)
}
ops(4,2)
ops(12,3)
#Q2
quadratic_eq <- function(a,b,c){ #function to find roots of quadratic equation
  discrimnant <- b^2 - 4*a*c #calculate the dicrimnant
  root_a <- (-b + sqrt(discrimnant)) / (2*a) #calculate root1
  root_b <- (-b - sqrt(discrimnant)) / (2*a) #calculate root2
  roots <- c(root_a,root_b)
  print("The roots of the quadratic equation for the given real numbers are:")
  return(roots)
}
quadratic_eq(1,2,1)
quadratic_eq(3,-6,3)