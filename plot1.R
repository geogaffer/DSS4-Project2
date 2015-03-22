plot1 <- function(table1="", preview=TRUE) {
    
    # don't want to rebuild the data tables each time if we are just working
    # on tweaking the graphics.
    
    # NEI data is assumed to be in ./data raw/NEI_data/
    
    if (table1=="") {
        if (!exists("NEI")) {
            NEI <- readRDS("./data raw/NEI_data/summarySCC_PM25.rds")
        }
        table1 <- aggregate(NEI$Emissions, by = list(NEI$year), FUN=sum)
    }
    
    # send to computer screen unless preview == FALSE is used to specify a 
    # plot run.
    
    if (!preview) png("plot1.png", width=640, height=480)
     
    plot(table1[,1], table1[,2]/1000000, type="b", col="blue", 
         main="Changes in Annual PM2.5 Emissions",
         xlab="Year", ylab="PM2.5  Emission (million tons)",
         xlim=c(1998,2008), ylim=c(3,8))
    
    if (!preview) dev.off()
}
