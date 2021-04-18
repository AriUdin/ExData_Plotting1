library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(gridExtra)

link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(link,"./Electric_power_consumption.zip",method = "curl")
unzip("Electric_power_consumption.zip")
pwr.consm <- read.csv2("household_power_consumption.txt")
for (x in 3:9) {
    pwr.consm[[x]] <- as.numeric((pwr.consm[[x]]))
}

pwr.consm[[2]] <- as.POSIXct(strptime(paste(pwr.consm[[1]],pwr.consm[[2]]), "%d/%m/%Y %H:%M:%S"))
pwr.consm[[1]] <- as.Date(strptime(pwr.consm[[1]], "%d/%m/%Y"))

pwr.consm <- pwr.consm[which(pwr.consm[[1]] >= as.Date("2007-02-01") &
                                 pwr.consm[[1]] <= as.Date("2007-02-02")),]

GAP <- ggplot(data = pwr.consm, aes(x = Time, y = Global_active_power)) +
    geom_line() +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
          axis.title.y = element_text(vjust = 3), axis.text.y = element_text(angle = 90),
          plot.margin = unit(c(1.2,0.8,1.2,0.8),"cm")) + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    xlab(element_blank())

Volt <- ggplot(data = pwr.consm, aes(x = Time, y = Voltage)) +
    geom_line() +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
          axis.title.y = element_text(vjust = 3), axis.text.y = element_text(angle = 90),
          plot.margin = unit(c(1.2,0.8,1.2,0.8),"cm")) + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    xlab("datetime")

GRP <- ggplot(data = pwr.consm, aes(x = Time, y = Global_reactive_power)) +
    geom_line() +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
          axis.title.y = element_text(vjust = 3), axis.text.y = element_text(angle = 90),
          plot.margin = unit(c(1.2,0.8,1.2,0.8),"cm")) + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    xlab("datetime") + labs()

ESB <- ggplot(data = gather(pwr.consm, key = "type", value = "energy",
                            -Date, -Time, -Global_active_power, -Global_reactive_power,
                            -Voltage, -Global_intensity)
) +
    geom_line(aes(x=Time, y=energy, color = type)) +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
          legend.justification = c(1,1), legend.position = c(1,1), legend.title = element_blank(),
          legend.background = element_blank(), legend.text = element_text(size = 9),
          axis.title.y = element_text(vjust = 3), axis.text.y = element_text(angle = 90),
          legend.key = element_blank(), legend.key.width = unit(1,"cm"),plot.margin = unit(c(1.2,0.8,1.2,0.8),"cm")) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    scale_colour_manual(values = c("black", "red", "blue")) +
    ylab("Energy sub metering") + xlab(element_blank())

png(filename = "Plot1.png", width = 480, height = 480, units = "px")
with(pwr.consm,
     hist(Global_active_power, col = "red",
          main = "Global Active Power", xlab = "Global Active Power (kilowatts)"))
dev.off()
