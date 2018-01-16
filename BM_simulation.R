#input

mu = 0.16; sigma = 0.2; T=1; n=100; S0 = 10;

#BM
dt = T/n

t = seq(0, T, by=dt)
x = c(S0, mu*dt+sigma*sqrt(dt)*rnorm(n, mean=0, sd=1))
Xt = cumsum(x)
plot(t, Xt, type="l", xlab="time")


#Geometric BM
mu=0.16
sigma = .2

x = c(sqrt(dt)*rnorm(n, mean=0, sd=1))
Xt = cumsum(x)
gmxt = S0 * exp(mu-sigma^2/2*t + sigma * Xt)
plot(t, gmxt, type="l", xlab="time", ylim = c(0, 20))
X<-matrix(rep(0, length(gmxt)*100), 100)
for (i in 1:100){
  X[i,]<-GBM(S0, r =mu, sigma=sigma, T=T, N=n)
  lines(t,X[i,], col="red")
}

q<-quantile(X, c(0.05, 0.95))

lines(t, rep(q[1], length(t)), col="blue", lty=2, lwd=2)
lines(t, rep(q[2], length(t)), col="blue", lty=2, lwd=2)

