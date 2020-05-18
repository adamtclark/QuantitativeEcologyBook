# Function for plotting results quickly
sim_comp_function<-function(s11=-0.08, s12=-0.03, s21=-0.09, s22=-0.06) {
  # set up parameters
  N1=.01; N2=.01;
  r1=0.5; r2=0.8;
  
  t=0; dt=.0001; tmax=75;      # set up time steps and maximum time
  step=0;            # a counter for saving dynamics every 1000 steps
  timevec=seq(0, tmax, length=tmax/(dt*1000)); # vector of time steps
  Nmat=matrix(nrow=length(timevec), ncol=2);  # empty matrix for abundances
  i=1;                         # counter for tracking position in N vector
  
  while(t<tmax) {
    dN1=(r1+s11*N1+s12*N2)*N1*dt;
    dN2=(r2+s21*N1+s22*N2)*N2*dt;
    
    N1=N1+dN1;
    if(N1<0) {
      N1=0;
    }
    
    N2=N2+dN2;
    if(N2<0) {
      N2=0;
    }
    
    t=t+dt; step=step+1;
    if(step==1000) {
      Nmat[i,1]=N1; # store N1 is column 1
      Nmat[i,2]=N2; # store N2 is column 2
      step=0; i=i+1; # reset counter and increment i by 1
    }
  }
  return(list(timevec=timevec, Nmat=Nmat))
}

pdf("images/ch08_q2.pdf", width=6, height=5)
par(mar=c(4,4,2,2), mfrow=c(2,2))

# competition
simout<-sim_comp_function()
matplot(simout$timevec, simout$Nmat, # timevec as x values, Nmat as y values
        type = "l",  # tell R to make a line plot
        col = c("red", "blue"), # colors for the two lines
        lty = 1, # line type (a solid line)
        xlab = "time", ylab = "abundance", # axis labels
        ylim = c(0, 15), # y axis limits
        main = "competition: s12<0; s21<0") # plot title
abline(h=0, lty=3) # add dotted line at abundance = 0

# predator/prey 1
simout<-sim_comp_function(s12=0.03)
matplot(simout$timevec, simout$Nmat, # timevec as x values, Nmat as y values
        type = "l",  # tell R to make a line plot
        col = c("red", "blue"), # colors for the two lines
        lty = 1, # line type (a solid line)
        xlab = "time", ylab = "abundance", # axis labels
        ylim = c(0, 15), # y axis limits
        main = "predation: s12>0; s21<0") # plot title
abline(h=0, lty=3) # add dotted line at abundance = 0
legend("topright", c("species 1", "species 2"),
       lty=1, col=c("red", "blue"),
       bty="n")


# predator/prey 2
simout<-sim_comp_function(s21=0.09)
matplot(simout$timevec, simout$Nmat, # timevec as x values, Nmat as y values
        type = "l",  # tell R to make a line plot
        col = c("red", "blue"), # colors for the two lines
        lty = 1, # line type (a solid line)
        xlab = "time", ylab = "abundance", # axis labels
        ylim = c(0, 15), # y axis limits
        main = "predation: s12<0; s21>0") # plot title
abline(h=0, lty=3) # add dotted line at abundance = 0

# mutualism
simout<-sim_comp_function(s12=0.03, s21=0.09)
matplot(simout$timevec, simout$Nmat, # timevec as x values, Nmat as y values
        type = "l",  # tell R to make a line plot
        col = c("red", "blue"), # colors for the two lines
        lty = 1, # line type (a solid line)
        xlab = "time", ylab = "abundance", # axis labels
        ylim = c(0, 55), # y axis limits
        main = "mutualism: s12>0; s21>0") # plot title
abline(h=0, lty=3) # add dotted line at abundance = 0
dev.off()