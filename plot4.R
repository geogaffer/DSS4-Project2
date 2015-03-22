plot4 <- function(preview=TRUE) {
    
    library(ggplot2)
    
    # Don't want to rebuild the data tables each time if we are just working
    # on tweaking the graphics.
    
    # NEI data is assumed to be in ./data raw/NEI_data/
    
    # A review of SCC documentation showed that only those EI Sector names
    # ending in "Coal" are assocated with coal combustion.
    
    SCC <- readRDS("./data raw/NEI_data/Source_Classification_Code.rds")
    coalSCC <- SCC[grep("Coal$", SCC$EI.Sector), ]
    coalSCCList <- as.character(coalSCC$SCC)
        
    if (!exists("NEI")) {
        NEI <- readRDS("./data raw/NEI_data/summarySCC_PM25.rds")
    }
    coalRecords <- NEI[NEI$SCC%in%coalSCCList, ]
    
    table4 <- aggregate(coalRecords$Emissions, 
                        by = list(coalRecords$year, coalRecords$type), 
                        FUN=sum )
    names(table4) <- c("year", "type", "PM25total")
    
    # send to computer screen unless preview == FALSE is used to specify a 
    # plot run.
    
    if (!preview) png("plot4.png", width=640, height=480)
    
    p <- ggplot(table4)
    p <- p + geom_line(aes(x=year, y=PM25total/1000, colour=type))
    p <- p + ylim(0, 600)
    p <- p + labs(x="Year", y="PM2.5 Emissions (thousand tons)")
    p <- p + ggtitle("PM2.5 Emissions by US Coal Combustion")
    print(p)
    
    if (!preview) dev.off()
}
