library(dplyr)
library(ggplot2)

# Read basic info
NEI <- readRDS("./data/summarySCC_PM25.rds")

# Transform in tables to work with dplyr, replacing original data
NEI <- tbl_df(data = NEI)

# Extract Baltimore data
Baltimore <- filter(NEI, fips == "24510")

# Group Baltimore data by year and type
Baltimore_by_year_type <- group_by(Baltimore, year, type)

# Remove unused data
rm(NEI, Baltimore)

# Summarize Total Emissiones in PM25-PRI by year
results <- summarize(Baltimore_by_year_type, total_emissions = sum(Emissions))

# Build chart
q <- qplot(year, total_emissions, data = results,
      geom = c('point','smooth'),
      method = 'lm',
      se = FALSE,
      facets = . ~ type,
      ylim = c(0, 2500),
      ylab = expression('Total Emissions in tons of PM'[2.5]),
      main = 'Total emissions in Baltimore by type'
      )
print(q)

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting2/plot3.png', width = 480, height = 480)

# Return to screen device
dev.off()

