
library(rpart)

n = 500
x = runif(n)
y = sin(9*x) + rnorm(n, 0, 0.2)
df = data.frame(x=x, y=y)

myPredict = function(x, y, new.x) {
  y[which.min(sapply(x, function(z) sqrt(sum((z - new.x) ^ 2))))]
}

MAX_DEPTH = 2
NUM_GRID = 100  # number of points on which we evaluate the fit
MAX_B = 200
lambda = 0.1

residuals = rep(0, n)
range.x = range(x)
grid.x = seq(range.x[1], range.x[2], length.out = NUM_GRID)
fitted.y = rep(0, NUM_GRID)

predictor = list()
par( mfrow = c( 2, 2 ) )

tmp.tr = rpart(y~x, df, control=rpart.control(maxdepth = MAX_DEPTH))
for (i in 1:NUM_GRID) {
  py = predict(tmp.tr, data.frame(x=grid.x[i]))
  fitted.y[i] = fitted.y[i] + py
}

for (i in 1:n) {
  residuals[i] = df$y[i] - myPredict(grid.x, fitted.y, df$x[i])
}

for (B in seq(1, MAX_B-1)) {
  if (B %% 5 == 0) print(paste("Iteration ==> ", B, sep=""))
  # plot points and current fit  
  plot(df$x, df$y, ylim=c(-1.5,1.5))
  points(grid.x, fitted.y, cex=1, col="red")

  # plot the current fit 
  plot(grid.x, fitted.y, cex=1, col="red", ylim=c(-1.5,1.5))
  
  # plot residuals
  plot(df$x, residuals, col="black")
  
  # fit residuals
  tmp.tr = rpart(res~x, data.frame(res=residuals, x=df$x), control=rpart.control(maxdepth = MAX_DEPTH))
  # plot fit and crushed fit
  for (cx in grid.x) {
    py = predict(tmp.tr, data.frame(x=cx))
    points(cx, py, col="red")
    points(cx, lambda * py, col="green")
  }
  
  # update fit
  for (i in 1:NUM_GRID) {
    py = predict(tmp.tr, data.frame(x=grid.x[i]))
    fitted.y[i] = fitted.y[i] + lambda * py
  }
  
  # update residuals
  for (i in 1:n) {
    residuals[i] = df$y[i] - myPredict(grid.x, fitted.y, df$x[i])
  }
  
  # skip plot
  plot.new()
}
