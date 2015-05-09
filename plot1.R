plot1<-function()
{
  ## make sure the dataset and function file are saved in the same folder
  
  data<-read.table("household_power_consumption.txt",header=TRUE,sep=";", stringsAsFactors=FALSE)
  
  ## choose records between 1/2/2007 00:00:00 and 2/2/2007 23:59:00
  data1<-data;
  data1<-data1[data1$Date=="1/2/2007" |data1$Date=="2/2/2007",]
  
  ## add two columns to the selected records, one column is D_T which combines the date and time together
  ## one column is Time_mins which coverts the record time into mins from 1/2/2007 00:00:00
  Date_Time<-paste(data1$Date,data1$Time)
  data1$D_T<-strptime(Date_Time,"%d/%m/%Y %H:%M:%S")
  temp<-rep(data1$D_T[1],length(data1$D_T))
  data1$Time_min<-difftime(data1$D_T,temp,units="mins")
  
  ## convert Global_active_power, Global_reactive_power, Voltage, 
  ##Sub_metering_1,Sub_metering_2,Sub_metering_3 into numeric data
  data1$Global_active_power<-as.numeric(data1$Global_active_power)
  data1$Global_reactive_power<-as.numeric(data1$Global_reactive_power)
  data1$Voltage<-as.numeric(data1$Voltage)
  data1$Sub_metering_1<-as.numeric(data1$Sub_metering_1)
  data1$Sub_metering_2<-as.numeric(data1$Sub_metering_2)
  ##data$Sub_metering_3<-as.numeric(data$Sub_metering_3)
  
  ## plot1
  png('plot1.png',width = 480, height = 480, units = "px")
  hist(data1$Global_active_power,main = "Global Active Power",xlab="Global Active Power (kilowatts)",col="red")
  dev.off()
}
