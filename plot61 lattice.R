library("reshape2")
library("lattice")
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
neiSum <- dcast(neiSum, year ~ city, value.var="Emissions")
names(neiSum)[3] <- "Los_Angeles"

cm <- as.data.frame(colMeans(neiSum)) # column means

neiSum <- cbind(neiSum,
                Baltimore.rel=neiSum$Baltimore / cm[2,1],
                Los_Angeles.rel=neiSum$Los_Angeles / cm[3,1],
                Baltimore.abs=neiSum$Baltimore - cm[2,1],
                Los_Angeles.abs=neiSum$Los_Angeles - cm[3,1])

plot1<-xyplot(Baltimore.rel+Los_Angeles.rel ~ year,
              data=neiSum,pch=19,cex=1.5,type=c("p","r"),lwd=2,ylim=c(0.5,1.9),
              col=c("blue","red"),
              scales=list(
                log=FALSE, # linear scale, or log10
                cex=1, # scale label size
                x=list(at=c(2000,2004,2008),labels=c("2000","2004","2008")),
                y=list(at=c(0.6,1.0,1.4,1.8),labels=c("60%","100%","140%","180%"),
                       col="brown")
              ),
              key=list(title="City",cex.title=1,border="cyan",
                       size=0.9, # legend box width
#                       rep=FALSE,
                       x=1,y=1,
                       columns=2,
                       corner=c(1,1), # top right corner
                       text=list(c("Baltimore","Los Angeles","Bal_trend","LA_trend"),
                                 col=c("blue","red","blue","red")
                                 ),
                       padding.text=2, # legend text line spacing
                       points=list(pch=19,cex=c(1,1,0,0),alpha=c(1,1,1,1),
                                   col=c("blue","red","blue","red")),
                       lines=list(lty=c(0,0,1,1),lwd=c(1,1,1,1),
                                  col=c("blue","red","blue","red"))
              ),
              main=expression("Relative " * PM[2.5] * " Vehicle Emissions"),
              sub="Comparison of Two Cities",
              xlab="Year",ylab="Relative Emissions",
#              strip=strip.custom(style=4,bg="gray75",fg="magenta",
#                                 par.strip.text=list(font=2)),
              par.settings=list(
                layout.heights=list(
                  main.key.padding = -0.5,
                  top.padding=2, 
                  bottom.padding=2),
#                strip.background=list(col="lightbrown"),
                layout.widths=list(
                  left.padding=2,
                  right.padding =2)
              ),
              grid=TRUE)

plot2<-xyplot(Baltimore.abs+Los_Angeles.abs ~ year,
              data=neiSum,pch=19,cex=1.5,type=c("p","r"),lwd=2,ylim=c(-700,600),
              col=c("blue","red"),
              scales=list(
                x=list(at=c(2000,2004,2008),labels=c("2000","2004","2008")),
                y=list(at=c(-600,-200,0,200,400),labels=c("-600","-200","0","200","400"))
              ),
              key=list(title="City",cex.title=1,border=TRUE,
                       rep=FALSE,
                       x=0.4,y=0.05,rows=4,corner=c(0,0),
                       text=list(c("Baltimore","Los Angeles","Bal_trend","LA_trend"),
                                 col=c("blue","red","blue","red")),
                       points=list(pch=19,cex=c(1,1,0,0),alpha=c(1,1,0,0),
                                   col=c("blue","red","blue","red")),
                       lines=list(lty=c(0,0,1,1),lwd=c(0,0,1,1),
                                  col=c("blue","red","blue","red"))
              ),
              main=expression("Absolute " * PM[2.5] * " Vehicle Emissions"),
              sub="Comparison of Two Cities",
              xlab="Year",ylab="Absolute Emissions",
#              strip=strip.custom(bg="gray75", par.strip.text=list(font=2)),
              par.settings=list(
                layout.heights=list(
                  main.key.padding = -0.5,
                  top.padding=2, 
                  bottom.padding=2),
                layout.widths=list(left.padding=2,right.padding =2)
              ),
              grid=TRUE)

png(file="plot61.png",height=450,width=900)
grid.arrange(plot1, plot2, ncol=2)
dev.off()
