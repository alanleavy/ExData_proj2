## Exploratory Data Analysis - Course Project 2 - Plot 3

## "Of the four types of sources indicated by the type 
##   (point, nonpoint, onroad, nonroad) variable, which of these 
##   four sources have seen decreases in emissions from 1999-2008 for 
##   Baltimore City? 
## Which have seen increases in emissions from 1999-2008? 
## Use the ggplot2 plotting system to make a plot answer this question."


## Assumes the following files in the working directory:
##   "Source_Classification_Code.rds" "summarySCC_PM25.rds" 


Plot3_content <- function(){
  ## create device-independent output for plot 3
  
  library(sqldf)
  library(ggplot2)
    
    ## Calculate total annual emissions for Baltimore City (fips = '24510')
  annual_emissions <- sqldf("select year, type, sum(Emissions) as total_emmisions_t
                            from NEI 
                            WHERE fips = '24510'
                            group by year, type order by year")
  
    ## Add a new column so we can plot the the 1999 baseline for each 
    ##  source type, otherwise it's a bit difficult to see if there 
    ##  has been an absolute increase or decrease between 1999 and 
    ##  2008 for POINT type sources.
  annual_emissions <- sqldf("select year, type, total_emmisions_t,
                              (select total_emmisions_t from annual_emissions a2 where year='1999' and a1.type=a2.type) as baseline
                            from annual_emissions a1 ")
  
  p <- ggplot(data=annual_emissions, aes(x=year,y=total_emmisions_t, type=type))
  p <- p + facet_wrap( ~ type, ncol=2)
  p <- p + geom_point()
  p <- p + ggtitle(expression('Annual Emissions per PM'[2.5]*' Source Type in Baltimore City'))
  p <- p + xlab("Year")
  p <- p + ylab(expression(atop('Total PM'[2.5]*' Emissions','(tons)')))
    ## Add a linear regression line to show the trend
  p <- p + stat_smooth(aes(colour="Linear Trend"),method = "lm", se = FALSE)
    ## Add a line to show the 1999 baseline
  p <- p + geom_abline(aes(colour="1999 Baseline",intercept=baseline,slope=0))
    ## change the x-axix to show the years for which we  have data
  p <- p + scale_x_continuous(breaks=seq(1999,2008,3), minor_breaks=seq(1999,2008,1))
  p <- p + theme(axis.text.x=element_text(angle = 45, hjust = .75)) 
  p <- p + theme(legend.title=element_blank())
}


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot3.png", width = 480, height = 480, units = "px")
print(Plot3_content())
dev.off()
