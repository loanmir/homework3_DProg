library(tidyverse)
library(ggplot2)
# 1
x1 <- beaver1$day
y1 <- beaver1$temp

x2 <- beaver2$day
y2 <- beaver2$temp

data.frame(Temperature=x1,y1) %>% ggplot()

# plot beaver1
plot_beaver1 <- ggplot(data = beaver1, aes(x= time, y = temp)) + 
  geom_line() + labs(title = "Temperature of beavers over time", y = "Temperature (°C)") +
  theme_minimal()

#plot beaver2
plot_beaver2 <- ggplot(data = beaver2, aes(x= time, y = temp)) + 
  geom_line() + labs(title = "Temperature of beavers over time", y = "Temperature (°C)") +
  theme_minimal()

#plot_side_by_side <- plot_beaver1 + plot_beaver2 + plot_layout(ncol=2)

{data.frame(X=c(beaver1$time, beaver2$time),
            Y=c(beaver1$temp, beaver2$temp),
            Type=rep(c("beaver1", "beaver2")))} -> df2
ggplot(df2) + aes(x=X, y=Y) + geom_path() + facet_wrap(vars(Type)) + labs(title = "Temperature of beavers over time", y = "Temperature (°C)")

# day to date format
beaverMonth <- as.Date(beaver1$day, format ="%B %d")
beaverMonth2 <- format(beaverMonth, "%b %d")
beaver1$dateTime <- beaverMonth2 
temp1 <- beaver1 # !!!!
# time to POSIX format
beaverT <- sprintf("%04d", as.numeric(beaver1$time))
beaverTime <- format(strptime(beaverT,"%H%M"), "%H:%M")
beaver1$hmin <- beaverTime
temp1 
temp1$newdate <- as.Date(paste(temp1$dateTime,temp1$hmin, sep = " "), format="%b %d %H:%M")
as_tibble(temp1)





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
  if (!(currency %in% names(currencies))){
    return(0)
  }
  converted_values <- amount / currencies[names(amount)]
  sum_amount <- sum(converted_values, na.rm=T)
  return (sum_amount * currencies[currency])
}

convert.all(c(EUR=6.5))
convert.all(c(EUR=3,ATS=2))
convert.all(c(EUR=3,EUR=2))
convert.all(c(EUR=3,DEM=2,SIT=10000))
convert.all(c(EUR=3,DEM=2,SIT=10000),"EUR")
convert.all(c(EUR=3,SIT=10000,DEM=2),"ESP")
convert.all(c(EUR=3,DEM=2,LIRA=100),"SIT")

