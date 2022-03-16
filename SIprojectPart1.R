# Statistical Inference Project
# part 1
set.seed(123)
lambda=.2
n=40
nsim=10000

expdist = rep(NA,nsim)

for (i in 1:nsim){
        expdist[i] = mean(rexp(n,lambda))  
}

empiricalmean=mean(expdist)
theoreticalmean=1/lambda

empiricalvariance=var(expdist)
theoreticalvariance=1/((lambda^2)*n)

cumulativemean=cumsum(expdist) / seq_along(expdist)
cumvar=cumsum((expdist-empiricalmean)^2)/(seq_along(expdist)-1)

####
plot(seq_along(expdist),cumulativemean,type="l",lty=1,lwd=1,
     main=expression("Means of 40 exp"(lambda*"=0.2")),xlab="Iteration"
     ,ylab="Mean")
abline(h=theoreticalmean,col="salmon",lwd=1)
legend("bottomright", legend=c("Theoretical Mean","Empirical Mean"),
       col=c("salmon","black"), lty=c(1,1),lwd=c(2,2),cex=.8,bty="n")

plot(seq_along(expdist),cumvar,type="l",lty=1,lwd=1,
     main=expression("Variance of 40 exp"(lambda*"=0.2")),
     xlab="Iteration",ylab="Variance")
abline(h=theoreticalvariance,col="salmon",lwd=1)
legend("bottomright", legend=c("Theoretical Variance",
                               "Empirical Variance"),
       col=c("salmon","black"), lty=c(1,1),lwd=c(2,2),cex=.8,bty="n")


#########
x=seq(0,8,0.01) 
hist(expdist,breaks=25,freq=FALSE,col="lightblue",main=expression("Empirical Exponential Distribution of Means of 40 exp"(lambda*"=0.2")),xlab="Value",ylab="Density")
curve(dnorm(x, mean=theoreticalmean, sd=sqrt(theoreticalvariance)),
      add=TRUE,lwd=2,col="salmon")
abline(v=theoreticalmean,lwd=2,col="darkred")
legend("topright", legend=c("Theoretical Mean","Theoretical Distribution"),
       col=c("darkred","salmon"), lty=c(1,1),lwd=c(2,2),cex=.8,bty="n")
######

##################

qqnorm(expdist,pch=16,cex=.8,col="lightblue")
qqline(expdist,col="salmon",lwd=2)
legend("bottomright", legend=c("Empirical Values","Normal Line"),
       col=c("lightblue","salmon"),pch=c(16,NA),
       lwd=c(NA,2),lty=c(NA,1),cex=.8,bty="n")


