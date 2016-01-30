NEIByYear <- tapply(NEI$Emissions, NEI$year, sum)
barplot(NEIByYear, xlab="YEAR", ylab="EMISSIONS", main="total PM2.5 emission from all sources")

NEIBaltimore <- NEI[NEI$fips == 24510, ]
NEIByYear_Baltimore <- tapply(NEIBaltimore$Emissions, NEIBaltimore$year, sum)
barplot(NEIByYear_Baltimore, xlab="YEAR", ylab="EMISSIONS", main="total PM2.5 emission in Baltimore")

library(ggplot2)
gp <- ggplot(NEIBaltimore, aes(factor(year), Emissions)) + geom_bar(stat="identity") + facet_grid(.~type) + labs(x = 

"YEAR", y="EMISSIONS", title="BALITMORE CITY PM2.5 EMISSIONS")
print(gp)

coalCombS <- SCC[SCC$EI.Sector == "Fuel Comb - Comm/Institutional - Coal",]["SCC"]
coalNEI <- NEI[NEI$SCC %in% coalCombS$SCC, ]
gp <- ggplot(coalNEI, aes(factor(year), Emissions)) + geom_bar(stat="identity") + labs(x = "YEAR", y="EMISSIONS", 

title="EMISSION FOR COAL COMBUSION SOURCES")
print (gp)

vDesc <- unique(grep("vehicle",SCC$EI.Sector,ignore.case=T,value=T))
vSc <- SCC[SCC$EI.Sector %in% vDesc,]['SCC']
vehicleNEI <- NEI[NEI$SCC %in% vSc$SCC,]
vehicleNEIBaltimore <- NEI[NEI$SCC %in% vSc$SCC & NEI$fips==24510,]
gp <- ggplot(vehicleNEIBaltimore, aes(factor(year), Emissions)) + geom_bar(stat="identity") + labs(x = "YEAR", 

y="EMISSIONS", title="EMISSION FROM MOTOR VEHICLES IN BALTIMORE")
print(gp)

vDesc <- unique(grep("vehicle",SCC$EI.Sector,ignore.case=T,value=T))
vSc <- SCC[SCC$EI.Sector %in% vDesc,]['SCC']
vehicleNEIBaltimore <- NEI[NEI$SCC %in% vSc$SCC & NEI$fips==24510,]
vehicleNEIBaltimore$city <- "Baltimore City"
vehicleNEILA <- NEI[NEI$SCC %in% vSc$SCC & NEI$fips=="06037",]
vehicleNEILA$city <- "LA city"
BA_LA_NEI <- rbind(vehicleNEIBaltimore, vehicleNEILA)
gp <- ggplot(BA_LA_NEI, aes(factor(year), Emissions)) + geom_bar(stat="identity") + labs(x = "YEAR", y="EMISSIONS", 

title="EMISSION FROM MOTOR VEHICLES IN BALTIMORE") + facet_grid(.~city)
print(gp)
