# wellChars.csv is saved from Jupyter Notebook. Contains Site, Pick, LAS info. 
# Created using the following Python code:
#-------------------------------------------------------------------------------
#    m = pd.merge(left=dt, right=wlp, left_on=['SitID', 'DEPT'], 
#            right_on=['SitID', 'Pick'], how='left')
#    m['HorID'] = m['HorID'].astype(str)
#    m.to_csv('wellChars.csv', sep=",", index=None)
#-------------------------------------------------------------------------------

mdf <- fread("wellChars.csv")
mdf[, Picked := ifelse(is.na(Pick), 0, 1)]
pickNums <- which(mdf$Picked == 1)

# Get Indices of 5 observation above and below the pick
bb <- foreach(i=1:length(pickNums)) %do% seq(pickNums[i]-5, pickNums[i]+5)
cc <- unique(sort(unlist(bb)))

# Prepare the data frame with only those observations
md10 <- mdf[cc,]
pp <- which(md10$Picked==1)
md10Pick <- md10[pp, ]

# Create Column names
metricNames <- names(unlist(md10[setdiff(seq(pp[1]-5, pp[1]+5), pp[1]),c(3:6)]))
metricIndices <- unlist(foreach(i=1:10) %do% seq(i,40,10))
newNames <- metricNames[metricIndices]

# Create a table for 10 vals of DPHI, GR, ILD, NPHI in single row for each pick
theMetrics <- matrix(nrow=length(pp), ncol=length(newNames))
for (i in c(1:length(pp))){
    theMetrics[i,] <- c(t(md10[setdiff(seq(pp[i]-5, pp[i]+5), pp[i]),c(3:6)]))
}
metricsDt <- data.table(theMetrics)
names(metricsDt) <- newNames

# Combine the new metrics data table with original table having only picks.
finalDt <- cbind(md10Pick, metricsDt)
