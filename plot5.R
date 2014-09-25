library("reshape2")
library("ggplot2")

# read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate the total emissions PM2.5 for each year
NEI <- NEI[NEI$fips=="24510",] # Baltimore City, Maryland (fips=="24510")
SCC8<-grep("Vehicle",SCC[,8],ignore.case=TRUE) # all vehicles
SCC1<-as.character(SCC[SCC8,1])

NEI <- NEI[NEI$SCC %in% SCC1,]

neiSum <- dcast(NEI, year ~ ., sum, value.var="Emissions")
names(neiSum)[2] <- "Emissions" # add name to column 2

ggplot(neiSum, aes(x=year,y=Emissions))+
  geom_point(aes(color="red"),size=4)+
  geom_text(aes(label=round(Emissions,1),col="red",face="Bold",
        hjust = ifelse(year == max(year), 1, 0), vjust = 1.8))+
  scale_x_continuous(breaks=c(2000,2004,2008))+
  geom_smooth(method="lm",formula=y~x,size=1,se=FALSE)+
  labs(title=expression("Total " * PM[2.5] * " Vehicle Emissions in Baltimore City, Maryland"))+
  labs(x="Year",y="Total Emissions (tons)")+
  theme(legend.position="none")

# copy to png file
dev.copy(png,file="plot5.png")
dev.off()
