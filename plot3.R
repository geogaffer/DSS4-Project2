plot3 <- function(table3=NULL, preview=TRUE) {
    
    library(ggplot2)
    
    # don't want to rebuild the data tables each time if we are just working
    # on tweaking the graphics.
    
    # NEI data is assumed to be in ./data raw/NEI_data/
    
    if (!exists("table3")) {
        if (!exists("NEI")) {
            NEI <- readRDS("./data raw/NEI_data/summarySCC_PM25.rds")
        }
        with(NEI, baltimore <<- NEI[fips=="24510",])
        table3 <- aggregate(baltimore$Emissions, 
                            by = list(baltimore$year, baltimore$type), 
                            FUN=sum )
        names(table3) <- c("year", "type", "PM25total")
    }
    
    # send to computer screen unless preview == FALSE is used to specify a 
    # plot run.
    
    if (!preview) png("plot3.pdf", width=640, height=480)
    
    p <- ggplot(table3)
    p <- p + geom_line(aes(x=year, y=PM25total, colour=type))
    p <- p + labs(x="Year", y="PM2.5 Emissions (tons)")
    p <- p + ggtitle("Categories of Baltimore PM2.5 Emissions")
    
    # Highlight the one type with increased emissions by showing the
    # 1999 baseline levels across the plot:
    
    p <- p + geom_hline(yintercept=296.795, linetype="dotted", size=0.5)
    
    print(p)
    
    if (!preview) dev.off()
}
