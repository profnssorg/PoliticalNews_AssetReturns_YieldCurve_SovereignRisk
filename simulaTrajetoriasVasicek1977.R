# Programa SimulaTrajetoriasVasicek1977
# Autor: Lee
# Referência: https://www.r-bloggers.com/fun-with-the-vasicek-interest-rate-model/

## Simulate Sample Paths ##

## define model parameters
r0 <- 0.03
theta <- 0.10
k <- 0.3
beta <- 0.03

## simulate short rate paths
n <- 10    # MC simulation trials
T <- 10    # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval

r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- k*(theta-r[i-1,j])*dt + beta*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 

## plot paths
t <- seq(0, T, dt)
rT.expected <- theta + (r0-theta)*exp(-k*t)
rT.stdev <- sqrt( beta^2/(2*k)*(1-exp(-2*k*t)))
matplot(t, r[,1:10], type="l", lty=1, main="Short Rate Paths", ylab="rt") 
abline(h=theta, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)