##########################################################
##                                                      ##
##      Functiion to read data and create Plot 4        ##
##             Exploratory Data Analysis                ##
##              Coursea Class Project 1                 ##
##                                                      ##
##########################################################


plot4 <- function(...) {
        
        
## Retreiving Electric Power Consumption Dataset and Convert Data for Plotting
## ---------------------------------------------------------------------------    
        
        ## Download Data from Target Location
        
                if(!file.exists("./data")){
                        dir.create("./data")
                }
        
                if(!file.exists("./data/Dataset.zip")){
                        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                        download.file(fileUrl,destfile="./data/Dataset.zip")
                }
        
        ## Unzipping Data
        
                if(!file.exists("./data/household_power_consumption.txt")){
                
                        unzip(zipfile="./data/Dataset.zip",exdir="./data")
                }
        
        ## Setting Path for Unzipped File Location
        
                file_path <- file.path("./data")      
        
        ## Reading Data file into R DataFrame Variable
        
                EP_Consumption  <- read.csv(file.path(file_path, "household_power_consumption.txt"), header = TRUE, sep = ";", na.strings = "?")

        ## Setsetting dataframe for Selected Dates
        
                EP_Consumption <- EP_Consumption[EP_Consumption$Date %in% c("1/2/2007","2/2/2007"),]
        
        ## Add DateTime Column to Dataframe
        
                EP_Consumption["Datetime"] <- NA
                EP_Consumption$Datetime <- strptime(paste(EP_Consumption$Date, EP_Consumption$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
        
        ## Converting Columns to Numberic
        
                EP_Consumption$Global_active_power <- as.numeric(EP_Consumption$Global_active_power)
                EP_Consumption$Global_reactive_power <- as.numeric(EP_Consumption$Global_reactive_power)
                EP_Consumption$Voltage <- as.numeric(EP_Consumption$Voltage)
                EP_Consumption$Global_intensity <- as.numeric(EP_Consumption$Global_intensity)
                EP_Consumption$Sub_metering_1 <- as.numeric(EP_Consumption$Sub_metering_1)
                EP_Consumption$Sub_metering_2 <- as.numeric(EP_Consumption$Sub_metering_2)
                EP_Consumption$Sub_metering_3 <- as.numeric(EP_Consumption$Sub_metering_3)
        

## Creating Requested Multiple Base Plot
## -------------------------------------              
                
        ## Open PNG Device to Create Multiple Plot
                
                png("plot4.png", width=480, height=480)
                
        ## Create Multiple Plot
                
                with(EP_Consumption, {
                        
                        ## Setting Up 2 Rows and 2 Columns of Four Plots Total for Multiplot
                        
                        par(mfrow = c(2, 2)) 
                        
                        ## Plotting Global Active Power - Upper Left
                        
                        plot(Datetime, Global_active_power, type="l", xlab="", ylab="Global Active Power")
                        
                        ## Plotting Voltage - Upper Right
                        
                        plot(Datetime, Voltage, type="l", xlab="datetime", ylab="Voltage")
                        
                        ## Plotting Sub Metering 1, 2, & 3 - Lower Left
                        
                        plot(Datetime, Sub_metering_1, type="l", col="black", ylab="Energy sub metering", xlab="")
                        lines(Datetime, Sub_metering_2, type="l", col="red")
                        lines(Datetime, Sub_metering_3, type="l", col="blue")
                        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty="n", lwd=2.5, col=c("black", "red", "blue"))
                        
                        
                        ## Plotting Global Reactive Power - Lower Right
                        
                        plot(Datetime, Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
                
                        
                        })
                
        ## Close PNG File Device
                
                dev.off()                
                
}