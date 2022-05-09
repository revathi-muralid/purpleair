# Locally weighted scatterplot smoother (LOWESS)
# Independent variable: Hourly outdoor measurements
# Dependent variable: Hourly I/O ratio for each I/O monitor pair
# I/O ratio expected to be close to true Finf when outdoor PM2.5 is high
# Fitted LOWESS curve approaches value of Finf asymptotically - IDEAL
# In practice, LOWESS curve will fluctuate w/ minimal variation around I/O ratio at high end of outdoor conc

# Spearman's rank correlations b/w indoor and lagged outdoor measurements

# Pair indoor measurements w/ outdoor measurements from an hour earlier for I/O ratio calc

# Exclude hourly I/O ratios > 1.2
# Max outdoor PM2.5 must exceed 95th percentile of outdoor conc

# Determine N using sensitivity analysis; Bi et al used N=20

# M was 100

# Smoother span was 100%

# Select N pairs of hourly I/O measurements w/ highest outdoor PM2.5 concentrations
# Obtain bootstrap sample w/ sample size N to fit LOWESS curve
# Mean of fitted LOWESS curve will be Finf estimate of bootstrap samples
# Repeat bootstrapping and LOWESS fitting M times to obtain M bootstrap Finf estimates
# final Finf=mean value of the M estimates
# uncertainty measure=SD of M estimates


lowess(x, ...)
# S3 method for default
lowess(x, y=NULL, f=2/3, iter=3L, delta=0.01 *
         diff(range(x)), ...)

# S3 method for formula
lowess(formula,data=parent.frame(), ..., subset, f=2/3,
       iter=3L, delta=.01*diff(range(mf[-response])))

# S3 method for lowess
plot(x, y, ..., col.lowess="red", lty.lowess=2)

plotLowess(formula, data=parent.frame(), ..., subset=parent.frame(),
           col.lowess="red", lty.lowess=2  )

