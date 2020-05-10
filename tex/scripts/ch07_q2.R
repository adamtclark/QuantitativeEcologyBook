runf<-function(r,N0=1) {
r=r;					               # growth rate
K=1000;                      # carrying capacity
s=-r/K;                      # set s to hold K constant
dt=1;                        # set time step size
timevec=seq(0, 20, by=dt);   # vector of time steps
Nvec=numeric(length(timevec)); # empty vector for storing abundances
N=N0;                        # initial abundance
t=0;                         # set initial time to zero
i=1;                         # counter for tracking position in N vector

while(i<=length(timevec)) {
  dN=(r+s*N)*N*dt;           # calculate dN
  N=N+dN;                    # add dN to the previous N value
  if(N<0) N=0;               # prevent negative populations
  Nvec[i]=N;                 # store new N value
  t=t+dt;                    # step forward in time
  i=i+1;                     # add one to counter
}
return(cbind(timevec, Nvec))
}

pdf("images/ch07_q2.pdf", width=8, height=4)
  par(mfrow=c(1,2), mar=c(4,4,1,1))
  out<-runf(1.8,1)
  plot(out[,1], out[,2], type="l", xlab="timevec", ylab="Nvec", lwd=1.5, ylim=c(0, 1600)) # plot results
  out<-runf(1.8,1.1)
  lines(out[,1], out[,2], col="red", lwd=1.5)
  legend("topleft", c("r = 1.8, N0=1", "r = 1.8, N0=1.1"), lty=1, col=c(1, "red"), bty="n", lwd=1.5)
  
  out<-runf(3,1)
  plot(out[,1], out[,2], type="l", xlab="timevec", ylab="Nvec", lwd=1.5, ylim=c(0, 1600), col="blue") # plot results
  out<-runf(3,1.1)
  lines(out[,1], out[,2], col="goldenrod", lwd=1.5)
  legend("topleft", c("r = 3, N0=1", "r = 3, N0=1.1"), lty=1, col=c("blue", "goldenrod"), bty="n", lwd=1.5)
dev.off()




out1<-runf(r = 3,N0 = 1000)
out2<-runf(r = 3,N0 = 1001)

#staring abundances after the inflection point
out3<-runf(r = 1.8,N0 = 500)
out4<-runf(r = 1.8,N0 = 1000)

pdf("images/ch07_q2b.pdf", width=8, height=4)
  par(mfrow=c(1,2), mar=c(4,4,1,1))
  time<-0:10
  
  #scale by K to make y-axis comparable
  deviation=(abs(out3[1:11,2]-out4[1:11,2]))/1000
  plot(time, deviation, main="r = 1.8")
  mod=lm(log(deviation)~time)
  timesq<-seq(0, 10, by=0.1)
  lines(timesq, exp(coef(mod)[1])*exp(coef(mod)[2]*timesq))
  text(labels = "d = 0.09 exp(-0.71 time)", x = 5.5, y = 0.041)
  
  #scale by K to make y-axis comparable
  deviation=abs(out1[1:11,2]-out2[1:11,2])/1000
  plot(time, deviation, main="r = 3")
  mod=lm(log(deviation)~time)
  timesq<-seq(0, 10, by=0.1)
  lines(timesq, exp(coef(mod)[1])*exp(coef(mod)[2]*timesq))
  
  text(labels = "d = 0.003 exp(0.55 time)", x = 4.5, y = 0.455)
dev.off()

