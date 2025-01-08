data_net <- data.frame(
  time = 1:1000,
  bitcoin = vec_btc_net,
  binance_coin = vec_bin_net,
  ethereum = vec_eth_net,
  ripple = vec_rip_net,
  litecoin = vec_lite_net
)

# Convert data to long format for ggplot2
library(tidyr)
data_long_net <- pivot_longer(data_net, cols = -time, names_to = "crypto", values_to = "value")

# Plotting the lines
ggplot(data_long_net, aes(x = time, y = value, color = crypto, group = crypto)) +
  geom_line(size = 0.5) +           # Draw lines
  ggtitle('') +
  xlab('Time') +
  ylab('Value') +
  theme_minimal() +
  theme(legend.title = element_blank())  # Optional: remove legend title





################################
#######

data_pos <- data.frame(
  time = 1:1000,
  bitcoin = vec_btc_pos,
  binance_coin = vec_bin_pos,
  ethereum = vec_eth_pos,
  ripple = vec_rip_pos,
  litecoin = vec_lite_pos
)

# Convert data to long format for ggplot2
library(tidyr)
data_long_pos <- pivot_longer(data_pos, cols = -time, names_to = "crypto", values_to = "value")

# Plotting the lines
ggplot(data_long_pos, aes(x = time, y = value, color = crypto, group = crypto)) +
  geom_line(size = 0.5) +           # Draw lines
  ggtitle('Cryptocurrency Trends Over Time') +
  xlab('Time') +
  ylab('Value') +
  theme_minimal() +
  theme(legend.title = element_blank())  # Optional: remove legend title


##########3


# Combine data into a data frame
data <- data.frame(
  time = 1:1000,
  Negative = vec_btc_net,
  Positive = vec_btc_pos
)

# Convert data to long format for ggplot2
library(tidyr)
data_long <- pivot_longer(data, cols = -time, names_to = "type", values_to = "value")

# Plotting the lines
ggplot(data_long, aes(x = time, y = value, color = type, group = type)) +
  geom_line(size = 0.5) +           # Draw lines
  scale_color_manual(values = c("Negative" = "red4", "Positive" = "blue4")) +
  labs(y = "", x = "", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom") 

# Calculate pairwise differences
diff_btc <- vec_btc_net - vec_btc_pos
diff_bin <- vec_bin_net - vec_bin_pos
diff_eth <- vec_eth_net - vec_eth_pos
diff_lite <- vec_lite_net - vec_lite_pos
diff_rip <- vec_rip_net - vec_rip_pos

# Create a data frame with the differences
data <- data.frame(
  time = 1:1000,
  Bitcoin = diff_btc,
  `Binance Coin` = diff_bin,
  Ethereum = diff_eth,
  Litecoin = diff_lite,
  Ripple = diff_rip
)

# Convert data to long format for ggplot2
library(tidyr)
data_long <- pivot_longer(data, cols = -time, names_to = "crypto", values_to = "difference")
# Plotting the pairwise differences
ggplot(data_long, aes(x = time, y = difference, color = crypto, group = crypto)) +
  geom_line(size = 0.5) +           # Draw lines
  ggtitle('Pairwise Differences Between Negative and Positive Returns Over Time') +
  xlab('Time') +
  ylab('Difference') +
  theme_minimal()



