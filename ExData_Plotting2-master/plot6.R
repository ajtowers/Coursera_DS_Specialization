library(data.table)
library(ggplot2)

# Read basic info
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Transform in data.tables, replacing original data
NEI <- data.table(NEI, key='SCC')
SCC <- data.table(SCC, key='SCC')

# Extract relevant rows and columns from NEI and SCC
NEI <- NEI[ , list(SCC, year, Emissions, fips)]
SCC <- SCC[ , list(SCC, Short.Name)]

# Merge datatables
NEISCC <- merge(NEI, SCC)

# Select those rows with Motor Vehicles in the Short.Name and Baltimore city code in fips col. 
# Motor Vehicles defined as those that have [Mm]otor in the Short.Name definition.
Balt_LA_MV <- NEISCC[grepl('[Mm]otor', Short.Name) & (fips == '24510' | fips == '06037'), list(year, fips, Emissions) ]

# Remove unused data
rm(NEI, SCC, NEISCC)

# Deactivate Warning messages
options(warn = -1)

# Summarize Total Emissiones in PM25-PRI by year for Motor vehicles in Baltimore City and LA County
results <- Balt_LA_MV[ , list(total_emissions = sum(Emissions)), by = list(year, fips)]

# Add column with named location
results['fips' == '06037', {location := 'Los Angeles'} ]
results['fips' == '24510', {location := 'Baltimore'} ]

# Order datatable by year
results <- results[order(location, year)]

# Reactivate Warning messages
options(warn = 0)

# # Build chart using ggplot
q <- ggplot(results, aes(year, total_emissions))
q <- q + geom_line(aes(color = location), size = 1) +
  geom_point(aes(color = location), size = 3) +
  labs(x = 'Year', y = expression('Total Emissions from Motor Vehicles in tons of ' * PM[2.5])) +
  labs(title = 'Baltimore City vs Los Angeles County') +
  theme(legend.title = element_blank())
print(q)

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting2/plot6.png', width = 480, height = 480)

# Return to screen device
dev.off()

