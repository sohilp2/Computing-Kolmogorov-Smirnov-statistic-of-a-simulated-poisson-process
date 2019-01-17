install.packages("seewave")
library(seewave)
sampleI=1
j=1 #not needed, same as samples
count = 0
sumof=0
for (i in 1:10^6) { 
  y <- -(log(1-runif(1,0,1)))/0.5
  sumof[i+1] = sumof[i] + y
  if(sumof[i+1]<=5){ 
    count[j]=count[j]+1
    next
  }
  #print(sampletime[i])
  sampleI=sampleI+1
  j=j+1
  sumof[i+1] = sumof[i+1]-5
  while (sumof[i+1]>=5) {
    sumof[i+1] = sumof[i+1]-5
    sampleI=sampleI+1
    count[j]=0
    j=j+1
  }
  count[j] = 1 # Adding count to next sample
  }
mean(count)
sampleI
ko=hist(count)
ko$counts = ko$counts/sampleI
plot(ko, ylab='Probability')
mean(count)
sampleI
par(new = TRUE)
ytheo = rpois(10^6, 2.5);  
curve(dnorm(x, mean(ytheo), sd(ytheo)), col="red", lwd=2, add=T)
proca=hist(sumof)
proca$counts=proca$counts/sum(proca$counts)
plot(proca,main = 'Histogram of Conditional arrivals',ylab='Probability')
plot(ecdf(sumof),main='Empirical CDF', ylab='probability')
par(new = TRUE)
cdf<-abline(0,0.2,col='red',lty=2 )
ecdfs= ecdf(sumof)
spect1=cbind(unique(sumof), ecdfs(unique(sumof)))
points = runif(10^6+1,0,5)
value= points/5
spect2=cbind(points,value)
ks.dist(spect1,spect2)

