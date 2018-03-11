  ##read data file, create datetime  and format time and date in 'dat' dataframe
  dat <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
  datetime <- as.POSIXct(paste(dat$Date, dat$Time), format = "%d/%m/%Y %H:%M:%S") 
  dat$Date <- as.Date(dat$Date, format = "%d/%m/%Y")
  dat$Time <- strptime(dat$Time, format = "%H:%M:%S")
  ##bind the datetime created w/ dataframe
  dat2 <- cbind(dat,datetime)
  dat1<- dat2[dat2$Date %in% as.Date(c('2007-02-01', '2007-02-02')),]
  ##format required variables as numeric
  dat1$Sub_metering_1 <- as.numeric(as.character(dat1$Sub_metering_1))
  dat1$Sub_metering_2 <- as.numeric(as.character(dat1$Sub_metering_2))
  dat1$Sub_metering_3 <- as.numeric(as.character(dat1$Sub_metering_3))
  ##create png
  png(filename = "plot3.png", width = 480, height = 480)
  ##requires ggplot2, creates plot w/ parameters
  ggplot(dat1, aes(datetime)) + 
    geom_line(aes(y = Sub_metering_3, color ="Sub_metering_3")) + 
    geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2")) + 
    geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1")) + 
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") + 
    ylab("Energy sub metering") + 
    xlab("") +
    scale_colour_manual(values = c("black", "red","blue")) + 
    theme(panel.background = element_rect(fill = 'white'), 
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.background = element_blank(),
          legend.position = c(0.87,0.905))
  dev.off()