library(data.table)
library(ggplot2)

# Read basic info
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Transform in tables to work with dplyr, replacing original data
NEI <- data.table(NEI, key='SCC')
SCC <- data.table(SCC, key='SCC')

# Extract relevant rows and columns from NEI and SCC
NEI <- NEI[ , list(SCC, year, Emissions, fips)]
SCC <- SCC[ , list(SCC, Short.Name)]

# Merge datatables
NEISCC <- merge(NEI, SCC)

# Select those rows with Motor Vehicles in the Short.Name and Baltimore city code in fips col. 
# Motor Vehicles defined as those that have [Mm]otor in the Short.Name definition.
Balt_MV <- NEISCC[grepl('[Mm]otor', Short.Name) & fips == '24510', list(year, Emissions) ]

# Remove unused data
rm(NEI, SCC, NEISCC)

# Summarize Total Emissiones in PM25-PRI by year for Motor vehicles in Baltimore City
results <- Balt_MV[ , list(total_emissions = sum(Emissions)), by = year]

# Order datatable by year
results <- results[order(year)]

# Build chart using ggplot
q <- ggplot(results, aes(year, total_emissions))
q <- q + geom_line(color = 'red', size = 1) +
  geom_point(color = 'red', size = 3) +
  labs(x = 'Year', y = expression('Total Emissions in tons of ' * PM[2.5])) +
  labs(title = 'Total Emissions in Baltimore City from Motor Vehicles')
print(q)

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting2/plot5.png', width = 480, height = 480)
 
# Return to screen device
dev.off()


