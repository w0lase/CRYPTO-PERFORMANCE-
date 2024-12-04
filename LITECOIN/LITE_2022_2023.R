## Importing the litecoin data from 2022 to 2023 ##

library(readxl)
litecoin <- read_excel("C:/Users/afand/Desktop/Final year Project/Crypto data/litecoin.xlsx")

#Splitting the data into 2-year periods
library(tidyverse)
breaks <- ymd_hms(c("2018-01-01 00:00:00", "2020-01-01 00:00:00", 
                    "2022-01-01 00:00:00", "2024-01-01 00:00:00"))


litecoin$period <- cut(litecoin$date, breaks = breaks, labels = c("2018-2019", "2020-2021", "2022-2023"))

lite_data_split <- split(litecoin, litecoin$period)

lite_2022_2023 <- as.data.frame(lite_data_split[3],)

## Generating a vector for the litecoin prices ##

lite_2022_2023_prices <- lite_2022_2023$X2022.2023.price

## Calculating the log Returns ##

log_ret_lite_2022_2023 <- diff(log(lite_2022_2023_prices))

## Creating a vector to store the negative log returns using threshold (-0.02) ##

neg_ret_lite_2022_2023 <- rep (0, length (log_ret_lite_2022_2023))

## Assigning 1 if negative log returns are less than threshold (-0.02) and 0 otherwise ##

for (i in 1: length(log_ret_lite_2022_2023)) {
  if (log_ret_lite_2022_2023[i] < -0.02) {
    neg_ret_lite_2022_2023[i] <- 1
  }
}

## Creating a vector to store the positive log returns using threshold (0.02) ##

pos_ret_lite_2022_2023 <- rep (0, length (log_ret_lite_2022_2023))

## Assigning 1 if negative log returns are greater than threshold (0.02) and 0 otherwise ##
for (i in 1:length (log_ret_lite_2022_2023)) {
  if (log_ret_lite_2022_2023[i] > 0.02) {
    pos_ret_lite_2022_2023[i] <- 1
  }
}

## obtaining weekly Returns for negative log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_lite <- function (neg_ret_lite) {
  into_weeks_lite <- cut (seq (1, length (neg_ret_lite)), breaks= 104, labels=FALSE)
  week_sum_lite <- tapply (neg_ret_lite, into_weeks_lite, sum)
  return (week_sum_lite)
}

# Calling the function with 'neg_ret_lite' to get the quarterly summations

week_summations_lite_2022_2023 <-calc_week_sum_lite(neg_ret_lite_2022_2023)

## Creating a data frame with negative quarters and their corresponding values ##
neg_quart_table_lite_2022_2023 <- data.frame(
  Weekly_lite = 1:104, # Weekly numbers
  Value = week_summations_lite_2022_2023 # Corresponding values
)
# Printing the resulting table

print (neg_quart_table_lite_2022_2023)

## obtaining weekly Returns for positive log returns
##
## Creating a function to calculate the weekly Summation ##

calc_week_sum_1_lite <- function (pos_ret_lite) {
  into_weeks_1_lite <- cut(seq (1, length (pos_ret_lite)), breaks=104, labels=FALSE)
  week_sum_1_lite <- tapply (pos_ret_lite, into_weeks_1_lite, sum)
  return (week_sum_1_lite)
}

# Calling the function with 'neg_ret_lite' to get the weekly summations

week_summations_1_lite_2022_2023 <- calc_week_sum_1_lite (pos_ret_lite_2022_2023)

## Creating a data frame with positive weeks and their corresponding values ##

pos_week_table_lite_2022_2023 <- data.frame(
  Week=1:104, #week numbers
  Value = week_summations_1_lite_2022_2023 # Corresponding values
)
# Print the resulting table

print (pos_week_table_lite_2022_2023)

## creating a cross tabulation table ##

cross_tabulation_lite_2022_2023 <-table (week_summations_lite_2022_2023,week_summations_1_lite_2022_2023)

## showing totals ##
extra_col_lite_2022_2023 <- matrix(rep(0,12),6,2)
colnames(extra_col_lite_2022_2023) <- c("6","7")
cross_tabulation_lite_2022_2023 <- cbind(cross_tabulation_lite_2022_2023,extra_col_lite_2022_2023)


extra_row_lite_2022_2023 <- matrix(rep(0,16),2,8)
rownames(extra_row_lite_2022_2023) <- c("6","7")
cross_tabulation_lite_2022_2023 <- rbind(cross_tabulation_lite_2022_2023,extra_row_lite_2022_2023)
#Adding row and column margins to the cross-tabulation table

cross_tab_with_totals_lite_2022_2023 <- addmargins (cross_tabulation_lite_2022_2023)

# Printing the table with subtotals and grand totals

print (cross_tab_with_totals_lite_2022_2023)

#

combined_data_lite_2022_2023 <- rbind(cross_tab_with_totals_lite_2022_2023[,9][-9], 
                                     cross_tab_with_totals_lite_2022_2023[9,][-9])

combined_data_rf_lite_2022_2023 <- combined_data_lite_2022_2023/104

rownames(combined_data_rf_lite_2022_2023) <- c("Negative","Positive")

# Create the bar plot with bars side by side of number of change in weekly returns

barplot(combined_data_lite_2022_2023, beside = TRUE, col = c("blue4", "lightblue"),
        names.arg = c(0:7),ylim = c(0,50),
        xlab = "Number of change in litecoin weekly returns",
        ylab = "Frequencies of number of litecoin weekly returns from 2020 to 2021")
legend("topright", legend = c("Negative returns", "Positive returns"), fill = c("blue4", "lightblue"))


## Split the list into chunks of 7 and plotting the time series graphs
split_list <- function(x, chunk_size) {
  split(x, ceiling(seq_along(x) / chunk_size))
}
# Split the list into chunks of 7

chunks <- split_list(log_ret_lite_2022_2023, 7)

# Compute the sums of each chunk
chunk_means <- sapply(chunks, mean)

plot(chunk_means,type = "l", col="blue4" , xlab = "Weeks")

##########

log_ret_lite_2022_2023 <- c(NA,log_ret_lite_2022_2023)
lite_2022_2023 <- cbind(lite_2022_2023,log_ret_lite_2022_2023)


ggplot(lite_2022_2023,aes(x =X2022.2023.date , y = log_ret_lite_2022_2023)) +
  geom_line() +geom_point(colour = "blue4",size = 1)+
  labs(x = "Date", y = "Log returns", title = "")+
  theme_classic() + geom_hline(yintercept = 0.02,col="red4")+
  geom_hline(yintercept = -0.02,col="red4")


##############

summary(log_ret_lite_2018_2019,na.rm  =T)
summary(log_ret_lite_2020_2021,na.rm  =T)
summary(log_ret_lite_2022_2023,na.rm  =T)
