plot6 <- function(table5=NULL, preview=TRUE) {
    
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
    
    # Use fips codes for Baltimore and LA County
    
    twoSites <- motorRecords[motorRecords$fips == "24510" | 
                                  motorRecords$fips == "06037", ]
    
    table6 <- aggregate(twoSites$Emissions, 
                        by = list(twoSites$year, twoSites$fips), 
                        FUN=sum )
    names(table6) <- c("year", "Location", "PM25total")
    
    # send to computer screen unless preview == FALSE is used to specify a 
    # plot run.
    
    if (!preview) png("plot6.png", width=640, height=720)
    
    p1 <- ggplot(table6, aes(x=year, y=PM25total, colour=Location))
    p1 <- p1 + geom_point(size=5, fill="white", shape=21)
    p1 <- p1 + geom_line() 
    p1 <- p1 + scale_colour_discrete(labels=c("LA County", "Baltimore"))
    p1 <- p1 + labs(x="Year", y="PM2.5 Emissions (tons)")
    p1 <- p1 + ggtitle("Comparison of Total PM2.5 Emissions by MVs")
    
    #print(p)
    
    laOnly <- table6[table6$Location == "06037", ]
    laOnly$change <- laOnly$PM25total - 3931.12
    
    baltimoreOnly <- table6[table6$Location == "24510", ]
    baltimoreOnly$change <- baltimoreOnly$PM25total - 346.82
    
    table6change <- rbind(laOnly, baltimoreOnly)
    
    p2 <- ggplot(table6change, aes(x=year, y=change, colour=Location))
    p2 <- p2 + geom_point(size=5, fill="white", shape=21)
    p2 <- p2 + geom_line() 
    p2 <- p2 + scale_colour_discrete(labels=c("LA County", "Baltimore"))
    p2 <- p2 + labs(x="Year", y="PM2.5 Emissions (tons)")
    p2 <- p2 + ggtitle("Comparison of Change in PM2.5 Emissions by MVs")
    
    multiplot(p1, p2, cols=1)
    if (!preview) dev.off()
}

# Following code from:
# Winston Chang, R Graphics Cookbook, 2013 O'Reilly.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}