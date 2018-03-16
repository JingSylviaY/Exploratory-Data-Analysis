#q1
setwd("~/Desktop/Coursera/4. Data Explotary Analysis/course 4 Final Project/exdata%2Fdata%2FNEI_data")
library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
attach(NEI)
totalPM2.5<-tapply(Emissions,as.factor(year) ,sum)
png("plot1.png")
barplot(totalPM2.5/1000, xlab = "Year", ylab = "Total Emission", 
       main = "Total Emission per year", ylim=c(0,8000), col=c(2,3,4,5))
dev.off()
#q2
setwd("~/Desktop/Coursera/4. Data Explotary Analysis/course 4 Final Project/exdata%2Fdata%2FNEI_data")
library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

subNEI<-subset(NEI, fips=="24510")
BCPM2.5<-tapply(subNEI$Emissions,as.factor(subNEI$year) ,sum)
png("plot2.png")
barplot(BCPM2.5/1000, xlab = "Year", ylab = "Total Emission", 
        main = "Total Emission per year in Baltimore", ylim=c(0,4), col=c(2,3,4,5))

dev.off()
#q3
setwd("~/Desktop/Coursera/4. Data Explotary Analysis/course 4 Final Project/exdata%2Fdata%2FNEI_data")
library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

totalPM2.5<-aggregate(Emissions~year+ type, subNEI,sum)
png("plot3.png")
ggplot(totalPM2.5, aes(x=year, y=Emissions,color=type))+
geom_line()+ xlab("Year")+ ylab("Total PM2.5 Emission")+ ggtitle("Total Emission per type in Baltimore")

dev.off()
#q4
setwd("~/Desktop/Coursera/4. Data Explotary Analysis/course 4 Final Project/exdata%2Fdata%2FNEI_data")
library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

position<-grep("coal", SCC$EI.Sector,ignore.case = TRUE)
subsetSCC<-SCC[position,]
coalSCC<-merge(NEI, subsetSCC[1], by="SCC")
totalPM2.5<-tapply(coalSCC$Emissions, coalSCC$year, sum)
png("plot4.png")
barplot(totalPM2.5/1000, xlab = "Year", ylab = "Total Emission", 
        main = "Total Emission from coal sources", ylim=c(0,700), col=c(2,3,4,5))

dev.off()
#q5
setwd("~/Desktop/Coursera/4. Data Explotary Analysis/course 4 Final Project/exdata%2Fdata%2FNEI_data")
library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

moter.P<-grep("mobile", SCC$EI.Sector, ignore.case = TRUE)
subsetSCC<-SCC[moter.P,]
moter.E<-NEI%>%filter(fips=="24510", SCC %in% subsetSCC[,1])
png("plot5.png")
ggplot(moter.E, aes(y=Emissions, x=factor(year), fill=factor(year)))+
        geom_bar(stat="identity")+labs(x="Year", y="Total Emissions(Moter Device)", 
                                       title="Motor Vehicle Source Emissions in Baltimore from 1999-2008")
dev.off()
#q6
setwd("~/Desktop/Coursera/4. Data Explotary Analysis/course 4 Final Project/exdata%2Fdata%2FNEI_data")
library(ggplot2)
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Find Emissions from motor device sources, join with NEI dataset for Baltimore and Los Angeles
moter.P<-grep("mobile", SCC$EI.Sector, ignore.case = TRUE)
subsetSCC<-SCC[moter.P,]
moter.Both<-NEI%>%filter(fips=="24510" | fips=="06037", SCC %in% subsetSCC[,1])
#moter.Both$fips<-gsub("24510","Baltimore")
#moter.Both$fips<-gsub("06037","Los Angeles", moter.Both)

#plot 6
png("plot6.png")
ggplot(moter.Both, aes(y=Emissions, x=factor(year), fill=factor(year)))+
        geom_bar(stat="identity")+facet_grid(.~fips)+
        labs(x="Year", y="Total Emissions(Moter Device)", 
                                       title="Comparison Motor Vehicle Source Emissionsin Baltimore/LA from 1999-2008")


dev.off()
