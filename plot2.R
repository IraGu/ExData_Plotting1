
  ##create png
  png(filename = "plot2.png", width = 480, height = 480)
  ##read data file, create datetime  and format time and date in 'dat' dataframe
  dat <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
  datetime <- as.POSIXct(paste(dat$Date, dat$Time), format = "%d/%m/%Y %H:%M:%S") 
  dat$Date <- as.Date(dat$Date, format = "%d/%m/%Y")
  dat$Time <- strptime(dat$Time, format = "%H:%M:%S")
  
  ##bind the datetime created w/ dataframe
  dat2 <- cbind(dat,datetime)
  dat1<- dat2[dat2$Date %in% as.Date(c('2007-02-01', '2007-02-02')),]
  
  ##format required variables as numeric
  dat1$Global_active_power <- as.numeric(as.character(dat1$Global_active_power))
  
  ##requires ggplot2, creates plot w/ parameters
  ggplot(dat1) + 
    aes(x = datetime, y = Global_active_power) + 
    geom_line() + 
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") + 
    ylab("Global Active Power (kilowatts)") +
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  dev.off()
