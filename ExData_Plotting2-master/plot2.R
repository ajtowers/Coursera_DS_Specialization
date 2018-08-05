library(dplyr)

# Read basic info
NEI <- readRDS("./data/summarySCC_PM25.rds")

# Transform in tables to work with dplyr, replacing original data
NEI <- tbl_df(data = NEI)

# Group by year
by_years <- group_by(NEI, year)
Baltimore_by_years <- filter(by_years, fips == "24510")

# Remove unused df
rm(NEI, by_years)

# Summarize Total Emissiones in PM25-PRI by year
results <- summarize(Baltimore_by_years, total_emissions = sum(Emissions)/1000)

# Plot parameters
par(mar = c(4, 4, 3, 1),
    cex.main = 0.9,
    cex.lab = 0.8,
    cex.axis = 0.7,
    mgp = c(1.5, 0.5, 0)
)

# Initial Plot
plot(results,
     main = 'Total Emissions in Baltimore City',
     xlab = 'Year',
     # use expression to add subscript
     ylab = expression('000s tons of PM'[2.5]),
     # modify the number of ticks in x axis
     xaxp = c(1999, 2008, 3)
)

# lm line to show trend
abline(lm(results$total_emissions ~ results$year),
       col = 'red',
       lwd = 2
)

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting2/plot2.png', width = 480, height = 480)

# Return to screen device
dev.off()