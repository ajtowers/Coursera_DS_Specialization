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

# Remove NA, build the working data.table & remove unused data
f <- f[complete.cases(f), ]

dt <- f[(Date == '2007-02-01' | Date == '2007-02-02'),
        list(newDate = strptime(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
             newGap = as.numeric(Global_active_power),
             sub1 = as.numeric(Sub_metering_1),
             sub2 = as.numeric(Sub_metering_2),
             sub3 = as.numeric(Sub_metering_3),
             vol = as.numeric(Voltage),
             newGrp = as.numeric(Global_reactive_power)
        )
        ]
rm(f)

# Define general parameters
maxRange <- max(range(dt$sub1), range(dt$sub2), range(dt$sub3))
par(mfcol = c(2,2),
    mar = c(3, 3, 2, 1),
    oma = c(0.5, 0.5, 0.5, 0.5),
    mgp = c(2, 0.5, 0),
    cex.lab = 0.6,
    cex.axis = 0.5
)
# Plot (1, 1) - Build xy plot, type = line
plot(dt$newDate, dt$newGap,
     type = 'l',
     xlab = '',
     ylab = 'Global Active Power',
)

# Plot (2, 1) - Build plot, then add lines
plot(x = dt$newDate,
     y = dt$sub1,
     type = 'l',
     col = 'black',
     xlab = '',
     ylab = 'Energy sub metering',
     ylim = c(0, maxRange)
)
par(new = T)
plot(dt$newDate, dt$sub2, 
     col = 'red',
     type = 'l',
     axes = FALSE,
     xlab = '',
     ylab = '',
     ylim = c(0, maxRange)
)
par(new = T)
plot(dt$newDate, dt$sub3, 
     col = 'blue',
     type = 'l',
     axes = FALSE,
     xlab = '',
     ylab = '',
     ylim = c(0, maxRange)
)

legend('topright',
       legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
       bty = 'n',
       col = c('black', 'red', 'blue'),
       lty = c(1,1,1),
       cex = 0.5,
)

# Plot (1, 2) - Voltage, line plot
plot(dt$newDate, dt$vol,
     type = 'l',
     xlab = 'datetime',
     ylab = 'Voltage',
)

# Plot (2, 2) - Global_reactive_power, line plot
plot(dt$newDate, dt$newGrp,
     type = 'l',
     xlab = 'datetime',
     ylab = 'Global Reactive Power'
)

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting1/plot4.png', width = 480, height = 480)

# Return to screen device, just in case.
dev.off()