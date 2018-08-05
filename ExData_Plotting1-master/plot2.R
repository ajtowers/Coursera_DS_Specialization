library(data.table)

# Read large file - NA are noted with '?' - colClassed to character to avoid warning  
f <- fread(input = './data/household_power_consumption.txt',
           sep = ";", 
           header = TRUE, 
           na.strings = c("?", ""),
           colClasses = 'character'
)

# Convert to Date format to help subsetting
f$Date <- as.Date(f$Date, format = '%d/%m/%Y')

# Build the working data.table & remove unused data
f <- f[complete.cases(f)]
dt <- f[(Date == '2007-02-01' | Date == '2007-02-02'),
        list(newDate = strptime(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
             newGap = as.numeric(Global_active_power))
        ]
rm(f)

# Define general parameters
par(mar = c(3, 4, 2, 2))

# Build xy plot, type = line
plot(dt$newDate, dt$newGap,
     type = 'l',
     xlab = '',
     ylab = 'Global Active Power (kilowatts)',
     cex.lab = 0.8,
     cex.axis = 0.8,
     )

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting1/plot2.png', width = 480, height = 480)

# Return to screen device, just in case.
dev.off()
