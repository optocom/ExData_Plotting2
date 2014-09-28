library("reshape2")
library("ggplot2")
library("grid")
library("gridExtra")

# read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate the total emissions PM2.5 for each year
NEI <- NEI[NEI$fips=="24510"|NEI$fips=="06037",] 
# Baltimore City, Maryland (fips=="24510")
# Los Angeles County, California (fips == 06037)
NEI[,1] <- gsub("24510","Baltimore",NEI[,1])
NEI[,1] <- gsub("06037","Los Angeles",NEI[,1])
names(NEI)[1] <- "city"

SCC8<-grep("Vehicle",SCC[,8],ignore.case=TRUE) # all vehicles
SCC1<-as.character(SCC[SCC8,1])
NEI <- NEI[NEI$SCC %in% SCC1,]

neiSum <- dcast(NEI, year+city ~ ., sum, value.var="Emissions")
names(neiSum)[3] <- "Emissions" # add name to column 3

# normalize emissions by dividing their means
neiSum[,4]<-neiSum[,3]/rep(tapply(neiSum$Emissions, neiSum$city, mean))
names(neiSum)[4] <- "Norm.emissions" # add name to column 4

# shift emissions by subtracting their means
neiSum[,5]<-neiSum[,3]-rep(tapply(neiSum$Emissions, neiSum$city, mean))
names(neiSum)[5] <- "Shift.emissions" # add name to column 5

# change Baltimore emissions signs in column 5 and save to column 6
neiSum[,6] <- neiSum[,5]
neiSum[neiSum$city=="Baltimore",6] <- -neiSum[neiSum$city=="Baltimore",6]
names(neiSum)[6] <- "Comp.emissions" # add name to column 6

plot1<-ggplot(neiSum, aes(x=year,y=Norm.emissions))+
  geom_point(aes(color=neiSum$city,shape=neiSum$city),size=4)+
  scale_x_continuous(breaks=c(2000,2004,2008))+
  geom_smooth(method="lm",formula=y~x,aes(color=neiSum$city,linetype=neiSum$city),size=1,se=FALSE)+
  labs(title=expression("Relative " ~ PM[2.5] ~ " Vehicle Emissions"))+
  labs(x="Year",y="Vehicle Emissions Normalized to Mean")+
  theme(legend.position="bottom",legend.title=element_text(size=12, face="bold"),
        legend.key.width=unit(2,"cm"))+
  scale_color_discrete(name="City")+
  scale_shape_discrete(name="City")+
  scale_linetype_discrete(name="City")+
  scale_y_continuous(breaks=c(0.6,1.0,1.4,1.8),labels=c("60%","100%","140%","180%"))+
  geom_hline(yintercept=1,color="grey80",size=1)

plot2<-ggplot(neiSum, aes(x=year,y=Shift.emissions))+
  geom_point(aes(color=neiSum$city,shape=neiSum$city),size=4)+
  scale_x_continuous(breaks=c(2000,2004,2008))+
  geom_smooth(method="lm",formula=y~x,aes(color=neiSum$city,linetype=neiSum$city),size=1,se=FALSE)+
  labs(title=expression("Offset " ~ PM[2.5] ~ " Vehicle Emissions"))+
  labs(x="Year",y="Vehicle Emissions Offset to Mean")+
  scale_color_discrete(name="City")+
  scale_shape_discrete(name="City")+
  scale_linetype_discrete(name="City")+
  theme(legend.position="bottom",legend.title=element_text(size=12, face="bold"),
        legend.key.width=unit(2,"cm"))

plot3<-ggplot(neiSum, aes(x=year,y=Comp.emissions))+
  scale_x_continuous(breaks=c(2000,2004,2008))+
  geom_smooth(method="lm",formula=y~x,aes(color=neiSum$city,linetype=neiSum$city),size=1,se=FALSE)+
  labs(title=expression("Offset " ~ PM[2.5] ~ " Vehicle Emissions"))+
  labs(x="Year",y="Vehicle Emissions Offset to Mean\n(Y Scale Changed)")+
  geom_text(aes(x=1999.5,y=170,face="plain",hjust=0,vjust=1, # upper left
                label=paste("Sign of Baltimore values changed\n",
                       "All points not plotted")))+
  scale_color_discrete(name="City")+
  scale_linetype_discrete(name="City")+
  theme(legend.position="bottom",legend.title=element_text(size=12, face="bold"),
        legend.key.width=unit(2,"cm"),plot.margin=unit(c(1,2,0.5,0.5),"lines"))+
  coord_cartesian(ylim = c(-200, 200)) 

png(file="question6.png",height=450,width=1200)
grid.arrange(plot1, plot2, plot3, ncol=3)
dev.off()
