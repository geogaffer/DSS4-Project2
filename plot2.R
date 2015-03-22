plot2 <- function(table2="", preview=TRUE) {
    
    # don't want to rebuild the data tables each time if we are just working
    # on tweaking the graphics.
    
    # NEI data is assumed to be in ./data raw/NEI_data/
    
    if (table2=="") {
        if (!exists("NEI")) {
            NEI <- readRDS("./data raw/NEI_data/summarySCC_PM25.rds")
        }
        with(NEI, baltimore <<- NEI[fips=="24510",])
        table2 <- aggregate(baltimore$Emissions, by = list(baltimore$year), 
                            FUN=sum )
    }
    
    # send to computer screen unless preview == FALSE is used to specify a 
    # plot run.
    
    if (!preview) png("plot2.png", width=640, height=480)
    
    plot(table2[,1], table2[,2]/1000, type="b", col="blue", 
         main="Changes in Baltimore PM2.5 Emissions",
         xlab="Year", ylab="PM2.5  Emission (thousand tons)",
         xlim=c(1998,2008), ylim=c(1.6, 3.6))
    
    if (!preview) dev.off()
}
