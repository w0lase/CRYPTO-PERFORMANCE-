## Importing the binancecoindata from 2020 to 2021 ##

library(readxl)
binancecoin<- read_excel("C:/Users/afand/Desktop/Final year Project/Crypto data/binancecoin.xlsx")

#Splitting the data into 2-year periods
library(tidyverse)
breaks <- ymd_hms(c("2018-01-01 00:00:00", "2020-01-01 00:00:00", 
                    "2022-01-01 00:00:00", "2024-01-01 00:00:00"))


binancecoin$period <- cut(binancecoin$date, breaks = breaks, labels = c("2018-2019", "2020-2021", "2022-2023"))

bin_data_split <- split(binancecoin, binancecoin$period)

bin_2020_2021 <- as.data.frame(bin_data_split[2],)

## Generating a vector for the binancecoinprices ##

bin_2020_2021_prices <- bin_2020_2021$X2020.2021.price

## Calculating the log Returns ##

log_ret_bin_2020_2021 <- diff(log(bin_2020_2021_prices))

## Creating a vector to store the negative log returns using threshold (-0.02) ##

neg_ret_bin_2020_2021 <- rep (0, length (log_ret_bin_2020_2021))

## Assigning 1 if negative log returns are less than threshold (-0.02) and 0 otherwise ##

for (i in 1: length(log_ret_bin_2020_2021)) {
  if (log_ret_bin_2020_2021[i] < -0.02) {
    neg_ret_bin_2020_2021[i] <- 1
  }
}

## Creating a vector to store the positive log returns using threshold (0.02) ##

pos_ret_bin_2020_2021 <- rep (0, length (log_ret_bin_2020_2021))

## Assigning 1 if negative log returns are greater than threshold (0.02) and 0 otherwise ##
for (i in 1:length (log_ret_bin_2020_2021)) {
  if (log_ret_bin_2020_2021[i] > 0.02) {
    pos_ret_bin_2020_2021[i] <- 1
  }
}

## obtaining weekly Returns for negative log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_bin <- function (neg_ret_bin) {
  into_weeks_bin <- cut (seq (1, length (neg_ret_bin)), breaks= 104, labels=FALSE)
  week_sum_bin <- tapply (neg_ret_bin, into_weeks_bin, sum)
  return (week_sum_bin)
}

# Calling the function with 'neg_ret_bin' to get the quarterly summations

week_summations_bin_2020_2021 <-calc_week_sum_bin(neg_ret_bin_2020_2021)

## Creating a data frame with negative quarters and their corresponding values ##
neg_quart_table_bin_2020_2021 <- data.frame(
  Weekly_bin = 1:104, # Weekly numbers
  Value = week_summations_bin_2020_2021 # Corresponding values
)
# Printing the resulting table

print (neg_quart_table_bin_2020_2021)

## obtaining weekly Returns for positive log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_1_bin <- function (pos_ret_bin) {
  into_weeks_1_bin <- cut(seq (1, length (pos_ret_bin)), breaks=104, labels=FALSE)
  week_sum_1_bin <- tapply (pos_ret_bin, into_weeks_1_bin, sum)
  return (week_sum_1_bin)
}

# Calling the function with 'neg_ret_bin' to get the weekly summations

week_summations_1_bin_2020_2021 <- calc_week_sum_1_bin (pos_ret_bin_2020_2021)

## Creating a data frame with positive weeks and their corresponding values ##

pos_week_table_bin_2020_2021 <- data.frame(
  Week=1:104, #week numbers
  Value = week_summations_1_bin_2020_2021 # Corresponding values
)
# Print the resulting table

print (pos_week_table_bin_2020_2021)

## creating a cross tabulation table ##

cross_tabulation_bin_2020_2021 <-table (week_summations_bin_2020_2021,week_summations_1_bin_2020_2021)

## showing totals ##

extra_col_bin_2020_2021 <- matrix(rep(0,5),5,1)
colnames(extra_col_bin_2020_2021) <- c("7")
cross_tabulation_bin_2020_2021 <- cbind(cross_tabulation_bin_2020_2021,extra_col_bin_2020_2021)


extra_row_bin_2020_2021 <- matrix(rep(0,24),3,8)
rownames(extra_row_bin_2020_2021) <- c("5","6","7")
cross_tabulation_bin_2020_2021 <- rbind(cross_tabulation_bin_2020_2021,extra_row_bin_2020_2021)


#Adding row and column margins to the cross-tabulation table

cross_tab_with_totals_bin_2020_2021 <- addmargins (cross_tabulation_bin_2020_2021)

# Printing the table with subtotals and grand totals

print (cross_tab_with_totals_bin_2020_2021)

# Generating row(negative) and column(positive) totals

combined_data_bin_2020_2021 <- rbind(cross_tab_with_totals_bin_2020_2021[,9][-9], cross_tab_with_totals_bin_2020_2021[9,][-9])

combined_data_rf_bin_2020_2021<- combined_data_bin_2020_2021/104

rownames(combined_data_rf_bin_2020_2021) <- c("Negative","Positive")


# Create the bar plot with bars side by side of number of change in weekly returns

barplot(combined_data_bin_2020_2021, beside = TRUE, col = c("blue4", "lightblue"),
        names.arg = c(0:7),
        xlab = "Number of change in binancecoinweekly returns",
        ylab = "Frequencies of number of binancecoinweekly returns from 2020 to 2021")
legend("topright", legend = c("Negative returns", "Positive returns"), fill = c("blue4", "lightblue"))

                     ## PLOTTING ##
## Split the list into chunks of 7 and plotting the time series graphs
split_list <- function(x, chunk_size) {
  split(x, ceiling(seq_along(x) / chunk_size))
}
# Split the list into chunks of 7

chunks <- split_list(log_ret_bin_2020_2021, 7)

# Compute the sums of each chunk
chunk_means <- sapply(chunks, mean)

plot(chunk_means,type = "l", col="blue4" , xlab = "Weeks")


##########

log_ret_bin_2020_2021 <- c(NA,log_ret_bin_2020_2021)
bin_2020_2021 <- cbind(bin_2020_2021,log_ret_bin_2020_2021)


ggplot(bin_2020_2021,aes(x =X2020.2021.date , y = log_ret_bin_2020_2021)) +
  geom_line() +geom_point(colour = "blue4",size = 1)+
  labs(x = "Date", y = "Log returns", title = "")+
  theme_classic() + geom_hline(yintercept = 0.02,col="red4")+
  geom_hline(yintercept = -0.02,col="red4")


##############