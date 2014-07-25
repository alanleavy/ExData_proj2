## Exploratory Data Analysis - Course Project 2 - Plot 5

## "How have emissions from motor vehicle sources changed
##  from 1999–2008 in Baltimore City?"


## Assumes the following files in the working directory:
##   "Source_Classification_Code.rds" "summarySCC_PM25.rds" 


Plot5_content <- function(){
  ## create device-independent output for plot 5
  
  library(sqldf)
  library(ggplot2)
  
  ## So, what qualifies as a 'motor vehicle' source?
  ## I found a legal definition -  http://www.law.cornell.edu/cfr/text/40/85.1703
  ## The salient points I picked out our are that it has to be 
  ##   1) mobile and 
  ##   2) intended for road use.
  ## A couple of test queries for Baltiore:
  ##    sqldf("select count(1) from NEI where type = 'ON-ROAD'and fips = '24510'")
  ##    sqldf("select count(1) from NEI 
  ##      where SCC in (select  SCC from SCC where EI_Sector  like '%Mobile - On-Road%') 
  ##        and fips = '24510'")
  ## give the same counts (1119), so I'm going with the first, simpler one.
  
  
  annual_emissions <- sqldf("select year, sum(Emissions) as total_emmisions_t
                             from NEI 
                             WHERE fips = '24510'
                              and type = 'ON-ROAD'
                             group by year order by year")
  
 ## A simple bar chart will do the job here
  annual_emissions$fYear = factor(annual_emissions$year)
  
  p <- ggplot(data=annual_emissions, aes(x=fYear,y=total_emmisions_t))
  p <- p + geom_bar(stat="identity")
  p <- p + ggtitle("Annual Emissions from Motor Vehicle Sources in Baltimore")
  p <- p + xlab("Year")
  p <- p + ylab(expression(atop('PM'[2.5]*' Emissions','(tons)')))
  p <- p + theme(axis.text.x=element_text(angle = 45, hjust = .75)) 
  p <- p + theme(legend.title=element_blank())
}


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot5.png", width = 480, height = 480, units = "px")
print(Plot5_content())
dev.off()
