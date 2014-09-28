library("reshape2")
library("ggplot2")

# read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC4<-grep("(.*coal.*comb.*)|(.*comb.*coal.*)",SCC[,4],ignore.case=TRUE)
SCC1<-as.character(SCC[SCC4,1])

# calculate the total emissions PM2.5 for each year
NEI <- NEI[NEI$SCC %in% SCC1,] # Coal comb data
neiSum <- dcast(NEI, year ~ ., sum, value.var="Emissions")
names(neiSum)[2] <- "Emissions" # add name to column 2
#neiSumType <- dcast(neiSum, year ~ type, value.var="Emissions")

ggplot(neiSum, aes(x=year,y=Emissions))+
  geom_point(aes(colour="red"),size=4)+
  scale_x_continuous(breaks=c(2000,2004,2008))+
  geom_smooth(method="lm",formula=y~x,size=1,se=FALSE)+
  annotate("polygon",alpha=0.3,color="brown",fill="yellow",
           x=c(2005.8,2005.5,2005,2004.5,2004.6),
           y=c(520000,565000,580000,560000,525000))+
  annotate("text",x=2005.2,y=535000,color="magenta",label="Outlier?")+
  labs(title=expression("Total " * PM[2.5] * " Emissions of Coal Combustions"))+
  labs(x="Year",y="Total Emissions (tons)")+
  theme(legend.position="none")

# copy to png file
dev.copy(png,file="plot4.png")
dev.off()
