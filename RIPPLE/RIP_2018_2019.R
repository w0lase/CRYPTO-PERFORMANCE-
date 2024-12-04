## Importing the ripple data from 2018 to 2019 ##

library(readxl)
ripple <- read_excel("C:/Users/afand/Desktop/Final year Project/Crypto data/ripple.xlsx")

#Splitting the data into 2-year periods
library(tidyverse)
breaks <- ymd_hms(c("2018-01-01 00:00:00", "2020-01-01 00:00:00", 
                    "2022-01-01 00:00:00", "2024-01-01 00:00:00"))


ripple$period <- cut(ripple$date, breaks = breaks, labels = c("2018-2019", "2020-2021", "2022-2023"))

rip_data_split <- split(ripple, ripple$period)

rip_2018_2019 <- as.data.frame(rip_data_split[1],)

## Generating a vector for the ripple prices ##

rip_2018_2019_prices <- rip_2018_2019$X2018.2019.price

## Calculating the log Returns ##

log_ret_rip_2018_2019 <- diff(log(rip_2018_2019_prices))

log_ret_rip_2018_2019[2:8]



## Creating a vector to store the negative log returns using threshold (-0.02) ##

neg_ret_rip_2018_2019 <- rep (0, length (log_ret_rip_2018_2019))

## Assigning 1 if negative log returns are less than threshold (-0.02) and 0 otherwise ##

for (i in 1: length(log_ret_rip_2018_2019)) {
  if (log_ret_rip_2018_2019[i] < -0.02) {
    neg_ret_rip_2018_2019[i] <- 1
  }
}

## Creating a vector to store the positive log returns using threshold (0.02) ##

pos_ret_rip_2018_2019 <- rep (0, length (log_ret_rip_2018_2019))

## Assigning 1 if negative log returns are greater than threshold (0.02) and 0 otherwise ##
for (i in 1:length (log_ret_rip_2018_2019)) {
  if (log_ret_rip_2018_2019[i] > 0.02) {
    pos_ret_rip_2018_2019[i] <- 1
  }
}

## obtaining weekly Returns for negative log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_rip <- function (neg_ret_rip) {
  into_weeks_rip <- cut (seq (1, length (neg_ret_rip)), breaks= 104, labels=FALSE)
  week_sum_rip <- tapply (neg_ret_rip, into_weeks_rip, sum)
  return (week_sum_rip)
}

# Calling the function with 'neg_ret_rip' to get the quarterly summations

week_summations_rip_2018_2019 <-calc_week_sum_rip(neg_ret_rip_2018_2019)

sum(week_summations_rip_2018_2019)


## Creating a data frame with negative quarters and their corresponding values ##
neg_quart_table_rip_2018_2019 <- data.frame(
  Weekly_rip = 1:104, # Weekly numbers
  Value = week_summations_rip_2018_2019 # Corresponding values
)
# Printing the resulting table

print (neg_quart_table_rip_2018_2019)

## obtaining weekly Returns for positive log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_1_rip <- function (pos_ret_rip) {
  into_weeks_1_rip <- cut(seq (1, length (pos_ret_rip)), breaks=104, labels=FALSE)
  week_sum_1_rip <- tapply (pos_ret_rip, into_weeks_1_rip, sum)
  return (week_sum_1_rip)
}

# Calling the function with 'pos_ret_rip' to get the weekly summations

week_summations_1_rip_2018_2019 <- calc_week_sum_1_rip (pos_ret_rip_2018_2019)

## Creating a data frame with positive weeks and their corresponding values ##

pos_week_table_rip_2018_2019 <- data.frame(
  Week=1:104, #week numbers
  Value = week_summations_1_rip_2018_2019 # Corresponding values
)
# Print the resulting table

print (pos_week_table_rip_2018_2019)

## creating a cross tabulation table ##

cross_tabulation_rip_2018_2019 <-table (week_summations_rip_2018_2019,week_summations_1_rip_2018_2019)


##Adding extra rows and columns

extra_col_rip_2018_2019 <- matrix(rep(0,12),6,2)
colnames(extra_col_rip_2018_2019) <- c("6","7")
cross_tabulation_rip_2018_2019 <- cbind(cross_tabulation_rip_2018_2019,extra_col_rip_2018_2019)

extra_row_rip_2018_2019 <- matrix(rep(0,16),2,8)
rownames(extra_row_rip_2018_2019) <- c("6","7")
cross_tabulation_rip_2018_2019 <- rbind(cross_tabulation_rip_2018_2019,extra_row_rip_2018_2019)


#Adding row and column margins to the cross-tabulation table

cross_tab_with_totals_rip_2018_2019 <- addmargins (cross_tabulation_rip_2018_2019)

# Printing the table with subtotals and grand totals

print (cross_tab_with_totals_rip_2018_2019)

# Generating row(negative) and column(positive) totals

combined_data_rip_2018_2019 <- rbind(cross_tab_with_totals_rip_2018_2019[,9][-9], cross_tab_with_totals_rip_2018_2019[9,][-9])

combined_data_rf_rip_2018_2019 <- combined_data_rip_2018_2019/104
rownames(combined_data_rf_rip_2018_2019) <- c("Negative","Positive")

list(combined_data_rf_rip_2018_2019[1,],combined_data_rf_rip_2018_2019[2,])

# Create the bar plot with bars side by side of number of change in weekly returns

barplot(combined_data_rip_2018_2019, beside = TRUE, col = c("blue4", "lightblue"),
        names.arg = c(0:7),
        xlab = "Number of change in ripple weekly returns",
        ylab = "Frequencies of number of ripple weekly returns from 2018 to 2019")
legend("topright", legend = c("Negative returns", "Positive returns"), fill = c("blue4", "lightblue"))


PGF <- function(pmf) {
  
  l <- c()
  
  # Set the initial value of val
  
  val <- pmf[1]
  
  # Iterate through the PMF
  
  repeat {
    # Compute the new value based on the current value of val and the PMF
    
    new_val <- sum(pmf[1:(length(pmf))] * val^(0:(length(pmf)-1)))
    
    if (abs(new_val - val) < 0.000001)  break # Check for convergence
    
    l <- append(l, val)        # Append the new value to the list
    
    val <- new_val  # Update the value for the next iteration
    
  }
  
  return(round(l,7))
}

neg_pmf_2018_2019 <- combined_data_rip_2018_2019[1,]/sum(combined_data_rip_2018_2019[1,])

pos_pmf_2018_2019 <- combined_data_rip_2018_2019[2,]/sum(combined_data_rip_2018_2019[2,])


PGF(neg_pmf_2018_2019)
PGF(pos_pmf_2018_2019)

expectation <- function(pmf){
  
  return(round(sum(pmf[1:8]*seq(0,7)),4))
}

expectation(neg_pmf_2018_2019)

expectation(pos_pmf_2018_2019)

##  PLOTTING ##

## Split the list into chunks of 7 and plotting the time series graphs
split_list <- function(x, chunk_size) {
  split(x, ceiling(seq_along(x) / chunk_size))
}
# Split the list into chunks of 7

chunks <- split_list(log_ret_rip_2018_2019, 7)

# Compute the sums of each chunk
chunk_means <- sapply(chunks, mean)

plot(chunk_means,type = "l", col="blue4" , xlab = "Weeks")

##########

log_ret_rip_2018_2019 <- c(NA,log_ret_rip_2018_2019)
rip_2018_2019 <- cbind(rip_2018_2019,log_ret_rip_2018_2019)


ggplot(rip_2018_2019,aes(x =X2018.2019.date, y = log_ret_rip_2018_2019)) +
  geom_line() +geom_point()+geom_point(colour = "blue4",size = 1)+
  labs(x = "Date", y = "Log returns", title = "")+
  theme_classic() + geom_hline(yintercept = 0.02,col="red4")+
  geom_hline(yintercept = -0.02,col="red4")


##############