library("reshape2")
library("ggplot2")
library("grid")

# read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate the total emissions PM2.5 for each year
NEI <- NEI[NEI$fips=="24510",] # Baltimore City, Maryland (fips=="24510")
neiSum <- dcast(NEI, year+type ~ ., sum, value.var="Emissions")
names(neiSum)[3] <- "Emissions" # add name to column 3
#neiSumType <- dcast(neiSum, year ~ type, value.var="Emissions")

png(filename="question3.png",width=800) # open graphic device

ggplot(neiSum, aes(x=year,y=Emissions))+
  geom_point(aes(color=type),size=2.5)+
  facet_grid(.~type,scales="free_y")+scale_x_continuous(breaks=c(2000,2004,2008))+
  geom_smooth(method="lm",formula=y~x,aes(color=type),size=1,se=FALSE)+
  labs(title=expression(PM[2.5] * " Emissions in Baltimore City, Maryland"))+
  labs(x="Year",y="Emissions (tons)")+
  theme(legend.position="bottom",legend.title=element_text(size=13),
        legend.key.width=unit(3,"cm"))

dev.off()
