###################################
## Step 1: Load and plot data
###################################

# load data
hdat = read.csv("data/human_pop_growth.csv")

# plot population vs. year
plot(hdat$Year, hdat$Population, xlab="Year", ylab="Population", type="b")


# fit a linear regression of population vs. year
# resulting model is of the form:
# Population = a + b*Year
# where m and b are fitted constants
mod = lm(Population~Year, data=hdat)

# add line showing model fit to plot
abline(mod, lty=2) # fit is not very good


###################################
## Step 2: Estimate delta-N and delta-t
###################################

# extract time and population vectors from data.frame
time = hdat$Year
N = hdat$Population

# calculate differences
# for each value, diff function calculates:
# dx[i] = x[i+1] - x[i]
dN = diff(N)
dtime = diff(time)

# vector of N values corresponding to each dN
# note, we have to remove the last value, since
# N[i+1] does not exist for this record, and cannot
# be calculated
Nt = N[-length(N)]

# calculate 1/N dN/dt
dNNdt = 1/Nt*dN/dtime

###################################
# Step 3: Fit regressions to dynamics
###################################

# plot dN/Ndt vs. N
par(mar=c(4,4,1,1))
plot(Nt, dNNdt, xlab="N", ylab="dNNdt",
     xlim=c(0, 12), ylim=c(-0.005, 0.035), # set axis limits
     xaxs="i",   # make no gap between edges of plot and axis limits
     axes=FALSE) # plot no axes
axis(2, at=seq(-0.005, 0.035, by=0.005), las=2) # add y-axis with specific tick marks
axis(1); box() # add x-axis and a box around the plot
abline(h=0, lty=2) # add dashed line at N = 0

# find the location in the time vector corresponding to 1965
transition = which(time==1965)
# add vertical line at this point
abline(v = Nt[transition], lty=2)

# fit two new regressions:
# one for the data prior to 1965, and
# one for the data after 1965

modpre = lm(dNNdt[1:transition]~Nt[1:transition])
c = lm(dNNdt[transition:length(dNNdt)]~Nt[transition:length(Nt)])

# add lines from regressions
abline(modpre, col="red", lty=2)
abline(modpost, col="blue", lty=2)

# Note - we are using a shorter dataset than for Fig. 6.3 in the textbook,
# so results are a little different.
