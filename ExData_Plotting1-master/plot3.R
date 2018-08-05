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
            sub1 = as.numeric(Sub_metering_1),
            sub2 = as.numeric(Sub_metering_2),
            sub3 = as.numeric(Sub_metering_3)
            )
        ]
rm(f)

# Define general parameters
maxRange <- max(range(dt$sub1), range(dt$sub2), range(dt$sub3))
par(mar = c(3, 4, 2, 2),
    cex.lab = 0.7,
    cex.axis = 0.7
    )

# Build plot, then add lines
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
       col = c('black', 'red', 'blue'),
       lty = c(1,1,1),
       cex = 0.6,
       )

# Copy to png graphic file device
dev.copy(device = png, filename = './ExData_Plotting1/plot3.png', width = 480, height = 480)

# Return to screen device, just in case.
dev.off()
