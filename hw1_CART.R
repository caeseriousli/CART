# Two random points from space S = [−1, 1] × [−1, 1]
set.seed(5)
p1 = runif(2, min = -1, max = 1)
p2 = runif(2, min = -1, max = 1)

# find f the "real" function
diff = p2-p1
slope = diff[2]/diff[1]
intercept = p1[2] - slope*p1[1]

# Generate data 
n = 20
X = data.frame(x0 = rep(.01, n), 
               x1 = runif(n, min = -1, max = 1), 
               x2 = runif(n, min = -1, max = 1))

# Assuming the slope isn't inf. assign data points "above the line 
# to have value 1 (+1), else 0 (-1)
y = ifelse(X$x2 > slope*(X$x1) + intercept, 1, -1)

# Construct weight vector W
# Initialize W
W = c(0, 0, 0)

# A function for checking classifitions
check = function(a) {
  a = ifelse(sign(a$x0*W[1]+a$x1*W[2]+a$x2*W[3])*y == 1, 
                           FALSE, TRUE)
  return(a)
}

# PLA Algorithm
# Hafta use R's inefficient while loop here D:
# Initialize missclassification indicator (initially all misclassified)
# add observation number so we can easily sample (hard to directly
# sample rows in R since, unlike Python, R data frame does not 
# treat rows as elements)
X$misclassified = rep(TRUE, n)
X$obs = seq(1, n)
count = 0
while (sum(X$misclassified) > 0) {
  
  # Samples one observation from the misclassfied pool of points
  i = sample(X[X$misclassified==TRUE, 5], 1)
  
  s = sign(sum(as.numeric(X[i, 1:3])*W))
  
  # If data point is on the incorrect "side", rotate the hyperplane
  # towards the data point so the angle between them (normal vector and data)
  # < 90 degrees
  if (y[i]==1 & s<=0) {
    W = W + as.numeric(X[i, 1:3])
  } else if (y[i]==0 & s>=0) {
    W = W - as.numeric(X[i, 1:3])
    correct = 0
  }
  X$misclassified = check(X)
  count = count + 1
}

cat("Iterations till converge: \n", count)

library(ggplot2) 

X$y = as.factor(y)
ggplot(X, aes(x=x1, y = x2, shape = y)) +
  geom_point() +
  geom_abline(intercept=intercept, slope=-W[2]/W[3], color = "red") +
  xlim(-1, 1) + ylim(-1, 1)

