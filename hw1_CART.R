# Two random points from space S = [−1, 1] × [−1, 1]
set.seed(5)
p1 = runif(2, min = -1, max = 1)
p2 = runif(2, min = -1, max = 1)

# find f the "real" function
diff = p2-p1
slope = diff[2]/diff[1]
intercept = p1[2] - slope*p1[1]

# Generate data 
n = 5
X = data.frame(x1 = runif(n, min = -1, max = 1), x2 = runif(n, min = -1, max = 1))

# Assuming the slope isn't inf. assign data points "above the line 
# to have value 1 (+1), else 0 (-1)
X$y = ifelse(x2 > slope*x1 + intercept, 1, 0)

# Construct weight vector W
# Initialize W
W = rep(0, n)