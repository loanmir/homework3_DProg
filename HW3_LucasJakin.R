library(tidyverse)
library(ggplot2)
# 1


{data.frame(X=c(beaver1$time, beaver2$time),
            Y=c(beaver1$temp, beaver2$temp),
            Type=rep(c("beaver1", "beaver2")))} -> df2
ggplot(df2) + aes(x=X, y=Y) + geom_path(color = "red") + facet_wrap(vars(Type)) + labs(title = "Temperature of beavers over time", y = "Temperature (°C)")


{data.frame(X=c(dftemp1$datetime1, dftemp2$datetime2),
            Y=c(dftemp1$sprem1, dftemp2$sprem2),
            Type=rep(c("beaver1", "beaver2")))} -> df3
ggplot(df3) + aes(x=X, y=Y) + geom_path(color = "violet") + facet_wrap(vars(Type)) +
  scale_x_datetime(date_breaks = "6 hour",date_labels = "%b %d %H:%M")
  + labs(title = "Temperature of beavers over time", y = "Temperature (°C)")

# GGPLOT OF BEAVER 1!!!!!!!!!!!!!!!!!!!!!!
temp1 <- beaver1 # !!!!
# day to date format
beaverMonth <- as.Date(beaver1$day, format ="%B %d")
beaverMonth2 <- format(beaverMonth, "%b %d")
beaver1$dateTime <- beaverMonth2 
# time to POSIX format
beaverT <- sprintf("%04d", as.numeric(beaver1$time))
beaverTime <- format(strptime(beaverT,"%H%M"), "%H:%M")
beaver1$hmin <- beaverTime
temp1$dayAndTime <- paste(temp1$dateTime,temp1$hmin)
dftemp1 <- data.frame(datetime1 = as.POSIXct(temp1$dayAndTime, format = "%b %d %H:%M"), sprem1 = temp1$temp)

ggplot(dftemp1,aes(x=datetime1, y = sprem1)) + geom_path(color="red") +
  scale_x_datetime(date_breaks = "6 hour",date_labels = "%b %d %H:%M") +
  labs(y="Temperature (°C)", title = "Temperature of beavers over time") + theme_minimal()


#GGPLOT OF BEAVER2!!!!!!!!!!!!!!!
temp2 <- beaver2
beaverMonthTwo <- as.Date(temp2$day, format ="%B %d")
beaverMonth2Two <- format(beaverMonthTwo, "%b %d")
temp2$dateTime <- beaverMonth2Two 
# time to POSIX format
beaverT2 <- sprintf("%04d", as.numeric(temp2$time))
beaverTime2 <- format(strptime(beaverT2,"%H%M"), "%H:%M")
temp2$hmin <- beaverTime2
temp2$dayAndTime <- paste(temp2$dateTime,temp2$hmin)
dftemp2 <- data.frame(datetime2 = as.POSIXct(temp2$dayAndTime, format = "%b %d %H:%M"), sprem2 = temp2$temp)

ggplot(dftemp2,aes(x=datetime2, y = sprem2)) + geom_path(color="lightblue") +
  scale_x_datetime(date_breaks = "6 hour",date_labels = "%b %d %H:%M") +
  labs(y="Temperature (°C)", title = "Temperature of beavers over time") + theme_minimal()










#temp1$newdate <- as.Date(paste(temp1$dateTime,temp1$hmin, sep = " "), format="%b %d %H:%M")

# EXERCISE 2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c(EUR=1,datasets::euro)

convert.all <- function(amounts, to_currency) {
  # Define conversion rates
  currencies <- c(EUR = 1, datasets::euro)
  
  # Convert to upper case
  to_currency <- toupper(to_currency)
  
  # Check if to_currency exists in conversion rates
  if (!(to_currency %in% names(currencies))) {
    return(0)  # Return 0 if to_currency doesn't exist
  }
  
  # Initialize total amount
  total_amount <- 0
  
  # Loop through each currency amount
  for (currency in names(amounts)) {
    # Check if currency exists in conversion rates
    if (currency %in% names(currencies)) {
      # Convert currency amount to to_currency using conversion rate
      if(is.null(to_currency)){
        
      }
      total_amount <- total_amount + amounts[currency] * currencies[[currency]] / currencies[[to_currency]]
    }
  }
  
  return(total_amount)
}


# Pravilna druga vajaaa
convert.all <- function(amount, currency = "EUR"){
  # Define conversion rates
  currencies <- c(EUR = 1, datasets::euro)
  # if currency doesn't exist return 0
  if (!(currency %in% names(currencies))){
    return(0)
  }
  converted_values <- amount / currencies[names(amount)]
  sum_amount <- sum(converted_values, na.rm=T) # na.rm for taking care of NA values (if currency don't exist then +0)
  return (sum_amount * currencies[currency])
}

convert.all(c(EUR=6.5))
convert.all(c(EUR=3,ATS=2))
convert.all(c(EUR=3,EUR=2))
convert.all(c(EUR=3,DEM=2,SIT=10000))
convert.all(c(EUR=3,DEM=2,SIT=10000),"EUR")
convert.all(c(EUR=3,SIT=10000,DEM=2),"ESP")
convert.all(c(EUR=3,DEM=2,LIRA=100),"SIT")

