#
#
#

#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

q1_plot <- function(table1) {
#     if (!exists(table1)) {
#         table1 <- aggregate(NEI$Emissions, by = list(NEI$year), FUN=sum)
#     }
#     
    plot(table1[,1], table1[,2]/1000000, type="b", col="blue", 
         main="Changes in Annual PM2.5 Emissions",
         xlab="Year", ylab="PM2.5  Emission (million tons)",
         xlim=c(1998,2008), ylim=c(3,8))
}

#Baltimore City, Maryland (fips == "24510"

q2_plot <- function(table2) {
    plot(table2[,1], table2[,2]/1000, type="b", col="blue", 
         main="Changes in Baltimore PM2.5 Emissions",
         xlab="Year", ylab="PM2.5  Emission (thousand tons)",
         xlim=c(1998,2008), ylim=c(1.6, 3.6))
}

q3_plot <- function(table3) {
    p <- ggplot(table3, aes(x=Group.1, y=x, colour=type))
    p <- p + geom_line()
    p <- p + labs(x="Year", y="PM2.5 Emissions (tons)")
    p <- p + ggtitle("Categories of Baltimore PM2.5 Emissions")
    p
}