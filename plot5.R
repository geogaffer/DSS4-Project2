plot5 <- function(table5=NULL, preview=TRUE) {
    
    library(ggplot2)
    
    # Don't want to rebuild the data tables each time if we are just working
    # on tweaking the graphics.
    
    # NEI data is assumed to be in ./data raw/NEI_data/
    
    # A review of SCC documentation showed that only those EI Sector names
    # containing "On-Road" are all assocated with motor vehicles.  "Non-Road"
    # did contain some motor vehicles but many of the subcategories were not.
    # Not they are also "Non-Road Equipement" while the others are "On-Road
    # Vehicles" which seems to match the assignment terminology better.
    
    SCC <- readRDS("./data raw/NEI_data/Source_Classification_Code.rds")
    motorSCC <- SCC[grep("On-Road", SCC$EI.Sector), ]
    motorSCCList <- as.character(motorSCC$SCC)
    
    if (!exists("NEI")) {
        NEI <- readRDS("./data raw/NEI_data/summarySCC_PM25.rds")
    }
    motorRecords <- NEI[NEI$SCC %in% motorSCCList, ]
    baltimore <- motorRecords[motorRecords$fips == "24510", ]
    
    table5 <- aggregate(baltimore$Emissions, 
                        by = list(baltimore$year, baltimore$type), 
                        FUN=sum )
    names(table5) <- c("year", "type", "PM25total")
    
    # send to computer screen unless preview == FALSE is used to specify a 
    # plot run.
    
    if (!preview) png("plot5.png", width=640, height=480)
    
    p <- ggplot(table5, aes(x=year, y=PM25total))
    p <- p + geom_point(colour="blue", size=5, fill="white", shape=21)
    p <- p + geom_line(colour="blue") 
    p <- p + labs(x="Year", y="PM2.5 Emissions (tons)")
    p <- p + ggtitle("Baltimore PM2.5 Motor Vehicle Emissions")

    print(p)
    
    if (!preview) dev.off()
}
