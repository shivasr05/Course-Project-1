# Load required library
library(ggplot2)

# Set working directory and load data
setwd("your_directory_path")
powerConsum <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

# Convert Date and Time columns to appropriate classes
powerConsum$Date <- as.Date(powerConsum$Date, format = "%d/%m/%Y")
powerConsum$DateTime <- as.POSIXct(paste(powerConsum$Date, powerConsum$Time), format = "%Y-%m-%d %H:%M:%S")

# Filter data for the desired date range
selectedDateRange <- powerConsum[powerConsum$Date >= "2007-02-01" & powerConsum$Date <= "2007-02-02",]

# Convert columns 3 to 8 to numeric
selectedDateRange[, 3:8] <- lapply(selectedDateRange[, 3:8], as.numeric)

# Save plots to PNG files
save_plot <- function(plot, filename) {
  ggsave(filename, plot, width = 6, height = 6, units = "in")
}

# Constructing Plot1
plot1 <- ggplot(selectedDateRange, aes(x = Global_active_power)) +
  geom_histogram(binwidth = 1, fill = "red") +
  labs(x = "Global Active Power (kilowatts)", y = "Frequency", title = "Global Active Power")
save_plot(plot1, "Plot1.png")

# Constructing Plot2
plot2 <- ggplot(selectedDateRange, aes(x = DateTime, y = Global_active_power)) +
  geom_line() +
  labs(x = "", y = "Global Active Power (kilowatts)") +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_minimal()
save_plot(plot2, "Plot2.png")

# Constructing Plot3
plot3 <- ggplot(selectedDateRange, aes(x = DateTime)) +
  geom_line(aes(y = Sub_metering_1), color = "black") +
  geom_line(aes(y = Sub_metering_2), color = "red") +
  geom_line(aes(y = Sub_metering_3), color = "blue") +
  labs(x = "", y = "Energy Sub Metering") +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_minimal() +
  theme(legend.position = "topright") +
  labs(color = "Legend")
save_plot(plot3, "Plot3.png")

# Constructing Plot4
plot4 <- ggplot(selectedDateRange, aes(x = DateTime)) +
  geom_line(aes(y = Global_active_power), linetype = "solid") +
  labs(x = "", y = "Global Active Power") +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_minimal()

plot4 <- plot4 + facet_grid(rows = vars(Voltage), cols = vars(Sub_metering_1))
save_plot(plot4, "Plot4.png")
