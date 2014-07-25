## Exploratory Data Analysis - Course Project 2 - Plot 2

## "Have total emissions from PM2.5 decreased in the Baltimore City, 
##   Maryland (fips == "24510") from 1999 to 2008? 
##  Use the base plotting system to make a plot answering this question

## Assumes the following files in the working directory:
##   "Source_Classification_Code.rds" "summarySCC_PM25.rds" 


Plot2_content <- function(){
  ## create device-independent output for plot 2
  
  library(sqldf)
  annual_emissions <- sqldf("select year, sum(Emissions)/1000 as total_emmisions_kt
                            from NEI 
                            WHERE fips = '24510'
                            group by year order by year")
  annual_emissions$fYear = factor(annual_emissions$year)
  par(mar=c(5.1,6,4.1,2.1))
  with(annual_emissions, plot(fYear,total_emmisions_kt
                              , type="n"
                              , xlab = "Year"
                              , ylab=expression(atop('Total PM'[2.5]*' Emissions','(thousands of tons)'))
                              , border="transparent"))
  with(annual_emissions, points(fYear,total_emmisions_kt,type="b", pch=20, col="grey"))
  with(annual_emissions, points(fYear,total_emmisions_kt,type="p", pch=20, col="black"))
  title(main=expression('Annual Emissions from all PM'[2.5]*'Sources in Baltimore City'))
  
}






NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot2.png", width = 480, height = 480, units = "px")

Plot2_content()
dev.off()