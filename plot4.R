rplot4 <- function(){
  
  ##create png
  png(filename = "plot4.png", width = 480, height = 480)
  
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
  dat1$Global_active_power <- as.numeric(as.character(dat1$Global_active_power))
  dat1$Voltage <- as.numeric(as.character(dat1$Voltage))
  dat1$Global_reactive_power <- as.numeric(as.character(dat1$Global_reactive_power))
  
  ## require ggplot2 package, create 4 plots and save them in variables
  p1 <- ggplot(dat1) + 
    aes(x = datetime, y = Global_active_power) + 
    geom_line() + 
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") + 
    ylab("Global Active Power")+
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  p2 <- ggplot(dat1, aes(datetime)) + 
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
          legend.background = element_blank(),
          legend.position = c(0.7,0.85))
  
  p3 <- ggplot(dat1) + 
    aes(x = datetime, y = Voltage) + 
    geom_line() + 
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") + 
    ylab("Voltage")+
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  p4 <- ggplot(dat1) + 
    aes(x = datetime, y = Global_reactive_power) + 
    geom_line() + 
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") + 
    ylab("Global_reactive_power")+
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  ## require gridExtra package, arrange 4 plots created onto 1 sheet
  grid.arrange(p1, p3, p2, p4, ncol = 2, nrow = 2) 
  
  dev.off()
  
}