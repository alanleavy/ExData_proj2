## Exploratory Data Analysis - Course Project 2 - Plot 6

## "Compare emissions from motor vehicle sources in Baltimore City 
##   with emissions from motor vehicle sources in Los Angeles County,
##   California (fips == "06037"). 
##  Which city has seen greater changes over time in motor vehicle 
##   emissions?"


## Assumes the following files in the working directory:
##   "Source_Classification_Code.rds" "summarySCC_PM25.rds" 


Plot6_content <- function(){
  ## create device-independent output for plot 6
  
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
  
  annual_emissions <- sqldf("select year, fips, sum(Emissions) as total_emmisions_t
                             from NEI 
                             WHERE (fips = '24510' or fips == '06037')
                              and type = 'ON-ROAD'
                             group by year, fips order by year")
  
    ## Add a column for the 1999 baseline for each location
  annual_emissions <- sqldf("select year
                              , case when fips= '24510' then 'Baltimore City' else 'Los Angeles County' end as fips_name
                              , total_emmisions_t
                              , (select total_emmisions_t from annual_emissions a2 
                                  where year='1999' 
                                  and a1.fips = a2.fips) as baseline
                            from annual_emissions a1 ")
  
    ## Add a set of rows, scaling the emissions for each location 
    ##  by the 1999 baseline for the other.
    ## This allows us to make both asolute and relative comparisons.
  annual_emissions <- sqldf("select *, 'Actual' as scaling from annual_emissions
                             union
                            select year
                              , fips_name
                              , total_emmisions_t/baseline * (select max(baseline) from annual_emissions a2 where a1.fips_name <> a2.fips_name) as total_emmisions_t
                              , (select max(baseline) from annual_emissions a2 where a1.fips_name <> a2.fips_name) baseline
                              ,'Scaled' as scaling
                            from annual_emissions a1")
  
  p <- ggplot(data=annual_emissions, aes(x=year,y=total_emmisions_t, shape = factor(scaling), col = factor(scaling)))
  
  p <- p + facet_wrap( ~ fips_name, ncol=2)
  p <- p + geom_point() + geom_line() 
  p <- p + ggtitle("Annual Emissions from Motor Vehicle Sources\n Baltimore City Vs. Los Angeles County")
  p <- p + xlab("Year")
  p <- p + ylab(expression(atop('PM'[2.5]*' Emissions','(tons)')))
    ## change the x-axix to show the years for which we  have data
  p <- p + scale_x_continuous(breaks=seq(1999,2008,3), minor_breaks=seq(1999,2008,1))
  p <- p + theme(axis.text.x=element_text(angle = 45, hjust = .75)) 
  p <- p + theme(legend.title=element_blank())
}


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(filename = "plot6.png", width = 480, height = 480, units = "px")
print(Plot6_content())
dev.off()
