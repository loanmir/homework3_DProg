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
temp1 %>% as.tibble()
as_tibble(temp1)




# EXERCISE 2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c(EUR=1,datasets::euro)

convert_currency <- function(amounts, to_currency = "EUR") {
  # Define conversion rates
  conversion_rates <- c(EUR = 1, datasets::euro)
  
  # Convert to upper case
  to_currency <- toupper(to_currency)
  
  # Check if to_currency exists in conversion rates
  if (!(to_currency %in% names(conversion_rates))) {
    return(0)  # Return 0 if to_currency doesn't exist
  }
  
  # Initialize total amount
  total_amount <- 0
  
  # Loop through each currency amount
  for (currency in names(amounts)) {
    # Check if currency exists in conversion rates
    if (currency %in% names(conversion_rates)) {
      # Convert currency amount to to_currency using conversion rate
      total_amount <- total_amount + amounts[currency] * conversion_rates[[currency]] / conversion_rates[[to_currency]]
    }
  }
  
  return(total_amount)
}

convert_currency(c(EUR=3,DEM=2, SIT=10000))



