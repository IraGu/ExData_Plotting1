rplot1 <- function(){
  
  ##create png
  png(filename = "plot1.png", width = 480, height = 480)
  
  ## read "household_power_consumption.txt" 
  dat <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
  ## format date
  dat$Date <- as.Date(dat$Date, format = "%d/%m/%Y")
  ## take data that has date 2007/02/01 and 2007/02/02
  dat1<- dat[dat$Date %in% as.Date(c('2007-02-01', '2007-02-02')),]
  ##format Global_Active_power as numeric
  dat1$Global_active_power <- as.numeric(as.character(dat1$Global_active_power))
  ##create histogram with same parameters as example
  hist(dat1$Global_active_power,
       main = "Global Active Power",
       xlab = "Global Active Power (kilowatts)",
       col = "red")

  dev.off()
  
}