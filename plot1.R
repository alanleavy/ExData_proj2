## Exploratory Data Analysis - Course Project 2 - Plot 1

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission 
##  from all sources for each of the years 1999, 2002, 2005, and 2008.

## Assumes the following files in the working directory:
##   "Source_Classification_Code.rds" "summarySCC_PM25.rds" 


Plot1_content <- function(){
  ## create device-independent output for plot 1
  
  library(sqldf)
  annual_emissions <- sqldf("select year, sum(Emissions)/1000000 as total_emmisions_Mt
                            from NEI group by year order by year")
  annual_emissions$fYear = factor(annual_emissions$year)
  par(mar=c(5.1,6,4.1,2.1))
  with(annual_emissions, plot(fYear,total_emmisions_Mt, type="n", xlab = "Year"
                              , ylab=expression(atop('Total PM'[2.5]*' Emissions','(thousands of tons)'))
                              ,border="transparent"))
  with(annual_emissions, points(fYear,total_emmisions_Mt,type="b", pch=20, col="grey"))
  with(annual_emissions, points(fYear,total_emmisions_Mt,type="p", pch=20, col="black"))
  title(main=expression('Annual Emissions from all PM'[2.5]*'Sources in the U.S.'))
}






NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot1.png", width = 480, height = 480, units = "px")

Plot1_content()
dev.off()