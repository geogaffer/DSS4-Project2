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