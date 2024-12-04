## Importing the ethereumdata from 2020 to 2021 ##

library(readxl)
ethereum<- read_excel("C:/Users/afand/Desktop/Final year Project/Crypto data/ethereum.xlsx")

#Splitting the data into 2-year periods
library(tidyverse)
breaks <- ymd_hms(c("2018-01-01 00:00:00", "2020-01-01 00:00:00", 
                    "2022-01-01 00:00:00", "2024-01-01 00:00:00"))


ethereum$period <- cut(ethereum$date, breaks = breaks, labels = c("2018-2019", "2020-2021", "2022-2023"))

eth_data_split <- split(ethereum, ethereum$period)

eth_2020_2021 <- as.data.frame(eth_data_split[2],)

## Generating a vector for the ethereumprices ##

eth_2020_2021_prices <- eth_2020_2021$X2020.2021.price

## Calculating the log Returns ##

log_ret_eth_2020_2021 <- diff(log(eth_2020_2021_prices))

## Creating a vector to store the negative log returns using threshold (-0.02) ##

neg_ret_eth_2020_2021 <- rep (0, length (log_ret_eth_2020_2021))

## Assigning 1 if negative log returns are less than threshold (-0.02) and 0 otherwise ##

for (i in 1: length(log_ret_eth_2020_2021)) {
  if (log_ret_eth_2020_2021[i] < -0.02) {
    neg_ret_eth_2020_2021[i] <- 1
  }
}

## Creating a vector to store the positive log returns using threshold (0.02) ##

pos_ret_eth_2020_2021 <- rep (0, length (log_ret_eth_2020_2021))

## Assigning 1 if negative log returns are greater than threshold (0.02) and 0 otherwise ##
for (i in 1:length (log_ret_eth_2020_2021)) {
  if (log_ret_eth_2020_2021[i] > 0.02) {
    pos_ret_eth_2020_2021[i] <- 1
  }
}

## obtaining weekly Returns for negative log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_eth <- function (neg_ret_eth) {
  into_weeks_eth <- cut (seq (1, length (neg_ret_eth)), breaks= 104, labels=FALSE)
  week_sum_eth <- tapply (neg_ret_eth, into_weeks_eth, sum)
  return (week_sum_eth)
}

# Calling the function with 'neg_ret_eth' to get the quarterly summations

week_summations_eth_2020_2021 <-calc_week_sum_eth(neg_ret_eth_2020_2021)

## Creating a data frame with negative quarters and their corresponding values ##
neg_quart_table_eth_2020_2021 <- data.frame(
  Weekly_eth = 1:104, # Weekly numbers
  Value = week_summations_eth_2020_2021 # Corresponding values
)
# Printing the resulting table

print (neg_quart_table_eth_2020_2021)

## obtaining weekly Returns for positive log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_1_eth <- function (pos_ret_eth) {
  into_weeks_1_eth <- cut(seq (1, length (pos_ret_eth)), breaks=104, labels=FALSE)
  week_sum_1_eth <- tapply (pos_ret_eth, into_weeks_1_eth, sum)
  return (week_sum_1_eth)
}

# Calling the function with 'neg_ret_eth' to get the weekly summations

week_summations_1_eth_2020_2021 <- calc_week_sum_1_eth (pos_ret_eth_2020_2021)

## Creating a data frame with positive weeks and their corresponding values ##

pos_week_table_eth_2020_2021 <- data.frame(
  Week=1:104, #week numbers
  Value = week_summations_1_eth_2020_2021 # Corresponding values
)
# Print the resulting table

print (pos_week_table_eth_2020_2021)

## creating a cross tabulation table ##

cross_tabulation_eth_2020_2021 <-table (week_summations_eth_2020_2021,week_summations_1_eth_2020_2021)

## showing totals ##

extra_col_eth_2020_2021 <- matrix(rep(0,10),5,2)
colnames(extra_col_eth_2020_2021) <- c("6","7")
cross_tabulation_eth_2020_2021 <- cbind(cross_tabulation_eth_2020_2021,extra_col_eth_2020_2021)


extra_row_eth_2020_2021 <- matrix(rep(0,24),3,8)
rownames(extra_row_eth_2020_2021) <- c("5","6","7")
cross_tabulation_eth_2020_2021 <- rbind(cross_tabulation_eth_2020_2021,extra_row_eth_2020_2021)


#Adding row and column margins to the cross-tabulation table

cross_tab_with_totals_eth_2020_2021 <- addmargins (cross_tabulation_eth_2020_2021)

# Printing the table with subtotals and grand totals

print (cross_tab_with_totals_eth_2020_2021)

# Generating row(negative) and column(positive) totals

combined_data_eth_2020_2021 <- rbind(cross_tab_with_totals_eth_2020_2021[,9][-9], cross_tab_with_totals_eth_2020_2021[9,][-9])

combined_data_rf_eth_2020_2021<- combined_data_eth_2020_2021/104

rownames(combined_data_rf_eth_2020_2021) <- c("Negative","Positive")


# Create the bar plot with bars side by side of number of change in weekly returns

barplot(combined_data_eth_2020_2021, beside = TRUE, col = c("blue4", "lightblue"),
        names.arg = c(0:7),
        xlab = "Number of change in ethereumweekly returns",
        ylab = "Frequencies of number of ethereumweekly returns from 2020 to 2021")
legend("topright", legend = c("Negative returns", "Positive returns"), fill = c("blue4", "lightblue"))

                     ## PLOTTING ##
## Split the list into chunks of 7 and plotting the time series graphs
split_list <- function(x, chunk_size) {
  split(x, ceiling(seq_along(x) / chunk_size))
}
# Split the list into chunks of 7

chunks <- split_list(log_ret_eth_2020_2021, 7)

# Compute the sums of each chunk
chunk_means <- sapply(chunks, mean)

plot(chunk_means,type = "l", col="blue4" , xlab = "Weeks")

##########

log_ret_eth_2020_2021 <- c(NA,log_ret_eth_2020_2021)
eth_2020_2021 <- cbind(eth_2020_2021,log_ret_eth_2020_2021)


ggplot(eth_2020_2021,aes(x =X2020.2021.date , y = log_ret_eth_2020_2021)) +
  geom_line() +geom_point(colour = "blue4",size = 1)+
  labs(x = "Date", y = "Log returns", title = "")+
  theme_classic() + geom_hline(yintercept = 0.02,col="red4")+
  geom_hline(yintercept = -0.02,col="red4")


##############