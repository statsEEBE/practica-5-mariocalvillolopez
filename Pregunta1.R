#Poblacion
mu<- 95.3
sigma<- 5.7

curve(dnorm(x, mean=mu,sd=sigma),xlim=c(80,120))

rnorm(1,mu,sigma)
pnorm(90,mu,sigma)


set.seed(123)
rnorm(4,mu,sigma) #muestra de cuatro encuestas aleatorias

Y <- function(i)(sum(rnorm(4,mu,sigma)))
Y(1)
Y10000 <- sapply(1:10000,Y)
hist(Y10000)
mean(Y10000)

#En teoria la media de la suma muestral
# de tamaño n=4
4*mu

#varianza de la suma muestral
4*sigma^2

var(Y10000)

###
hist(Y10000,freq = FALSE)
curve(dnorm(x,mean=4*mu,sd=sqrt(4)*sigma),add=TRUE)


#en teoria
100*sigma^2

#c

1-pnorm(103,mu,sigma)

Y<- function(i)(sum(rnorm(1,mu,sigma)))
Y100000 <- sapply(1:100000,Y)

mean(Y100000>103)

#d n=4
Xbar <- function(i)(mean(rnorm(4,mu,sigma)))
Xbar100000 <- sapply(1:100000,Xbar)
hist(Xbar100000)

mean(Xbar100000<98)
pnorm(98,mu,sigma/sqrt(4))

#e

Ssq <- function(i)(var(rnorm(100,mu,sigma)))
Ssq100000 <- sapply(1:100000,Ssq)
hist(Ssq100000)

mean(Ssq100000>32)
#teoria
1-pchisq((100-1)*32/sigma^2,100-1)

hist(Ssq100000*(100-1)/sigma^2,prob=TRUE)
curve(dchisq(x,100-1),add=TRUE,col="red")

(100-1)*32/sigma^2
