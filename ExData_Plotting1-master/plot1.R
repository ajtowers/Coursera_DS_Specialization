library(data.table)

# Read large file - NA are noted with '?' or ''
f <- fread(input = './data/household_power_consumption.txt',
           sep = ";", 
           header = TRUE, 
           na.strings = c("?", ""),
           colClasses = 'character'
           )

# Convert to Date format
f$Date <- as.Date(f$Date, format = '%d/%m/%Y')

# Build the working data.table & remove unused data
f <- f[complete.cases(f)]
dt <- f[(Date == '2007-02-01' | Date == '2007-02-02'),
        list(Date, Global_active_power)]
rm(f)

# Define general parameters
par(mar = c(4, 4, 2, 2))

# Histogram
hist(as.numeric(dt$Global_active_power),
     col = 'red',
     main = 'Global Active Power',
     xlab = 'Global Active Power (in kilowatts)',
     yaxp = c(0, 1200, 6),
     cex.main = 0.9,
     cex.lab = 0.8,
     cex.axis = 0.7,
    )

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting1/plot1.png', width = 480, height = 480)

# Return to screen device, just in case.
dev.off()
