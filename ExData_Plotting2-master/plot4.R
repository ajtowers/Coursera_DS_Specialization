library(dplyr)
library(ggplot2)

# Read basic info
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Transform in tables to work with dplyr, replacing original data
NEI <- tbl_df(data = NEI)
SCC <- tbl_df(data = SCC)

# Extract SCC & Short.Name from SCC
NEI <- select(NEI, SCC, year, Emissions)
SCC <- select(SCC, SCC, Short.Name)


# Merge newSCC and NEI
NEI <- merge(NEI, SCC, by='SCC', all=FALSE)

# Select those rows with Coal in the Short.Name.
# Does not include sources like Charcoal. To include them, change first parameter of grepl to
# regex '[Cc]oal'
NEI <- filter(NEI, grepl('Coal', Short.Name))

# Group data by year
NEI_by_year <- group_by(NEI, year)

# Remove unused data
rm(NEI, SCC)

# Summarize Total Emissiones in PM25-PRI by year
results <- summarize(NEI_by_year, total_emissions = sum(Emissions)/1000)

# Build chart using ggplot
q <- ggplot(results, aes(year, total_emissions))
q <- q + geom_point(color = 'steelblue', size = 4) +
  geom_smooth(method = 'lm', se = FALSE, color = 'red', size = 1) +
  labs(x = 'Year', y = expression('Total Emissions in 000s tons of ' * PM[2.5])) +
  labs(title = 'Total Emissions in USA from coal combustion-related sources')
print(q)

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting2/plot4.png', width = 480, height = 480)

# Return to screen device
dev.off()


