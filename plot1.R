library("reshape2")
# read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate the total emissions PM2.5 for each year
neiSum <- dcast(NEI, year ~ ., sum, value.var="Emissions")
names(neiSum)[2] <- "Emissions" # add name to column 2

# plot the graph
par(tck=1)
plot(neiSum, type="p", cex=1.5, pch=19, col="red",
     main=expression("Total " * PM[2.5] * " Emissions in US"),
     xlab="Year",ylab="Total Emissions (tons)",
     xlim=c(1998,2008),ylim=c(3e6,8e6))

# linear fit the data
fit <- lm(Emissions~year, neiSum)

# plot the fitted line
abline(fit, col="blue", lwd=2)

# copy to png file
dev.copy(png,file="question1.png")
dev.off()
