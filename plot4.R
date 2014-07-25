## Exploratory Data Analysis - Course Project 2 - Plot 4

## "Across the United States, how have emissions from coal 
##  combustion-related sources changed from 1999–2008?"


## Assumes the following files in the working directory:
##   "Source_Classification_Code.rds" "summarySCC_PM25.rds" 


Plot4_content <- function(){
  ## create device-independent output for plot 4
  
  library(sqldf)
  library(ggplot2)
  
  annual_emissions <- sqldf("select year, sum(Emissions)/1000 as total_emmisions_kt
                             from NEI 
                             WHERE SCC in (select distinct SCC 
                                            from SCC 
                                            where [EI_Sector]  like '%comb -%coal%'or [Short_Name]  like '%comb /%coal%')
                             group by year order by year")
  
  p <- ggplot(data=annual_emissions, aes(x=year,y=total_emmisions_kt))
  p <- p + geom_point() + geom_line()
  p <- p + ggtitle("Annual Coal Combustion-Related Emissions across the U.S.")
  p <- p + xlab("Year")
  p <- p + ylab(expression(atop('PM'[2.5]*' Emissions','(thousands of tons)')))
    ## change the x-axix to show the years for which we  have data
  p <- p + scale_x_continuous(breaks=seq(1999,2008,3), minor_breaks=seq(1999,2008,1))
  p <- p + theme(axis.text.x=element_text(angle = 45, hjust = .75)) 
  p <- p + theme(legend.title=element_blank())
}


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot4.png", width = 480, height = 480, units = "px")
print(Plot4_content())
dev.off()
