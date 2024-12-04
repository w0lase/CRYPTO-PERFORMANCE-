
pmf_btc_1 <- c(combined_data_rf_btc_2018_2019[seq(1,16,2)],
               combined_data_rf_btc_2018_2019[seq(2,16,2)])

pmf_btc_2 <- c(combined_data_rf_btc_2020_2021[seq(1,16,2)],
               combined_data_rf_btc_2020_2021[seq(2,16,2)])

pmf_btc_3 <- c(combined_data_rf_btc_2022_2023[seq(1,16,2)],
               combined_data_rf_btc_2022_2023[seq(2,16,2)])

pmf_btc_list <- c(pmf_btc_1,pmf_btc_2,pmf_btc_3)

round(pmf_btc_3,4)

           #######################################################

# Function to calculate the new value of val_net
PGF <- function(pmf_segment, val_net) {
  
  return(sum(pmf_segment[1:8] * val_net^(0:7)))
}

# Main function to iterate and compute the PGF values
PGFF <- function(pm) {
  
  l_net <- c()
  l_pos <- c()
  
  ######## NEGATIVE RETURNS   ######## 
  
  random_samp <- sample(1:3, 999,replace = TRUE,prob = c(0.1,0.2,0.7))  #,prob = c(0,0.1,0.9)
  
  # Initialize val_net with the first segment
  if (random_samp[1] == 1) {
    val_net <- pm[1]
    new_val_net <- PGF(pm[1:8], val_net)
  } else if (random_samp[1] == 2) {
    val_net <- pm[17]
    new_val_net <- PGF(pm[17:24], val_net)
  } else {
    val_net <- pm[33]
    new_val_net <- PGF(pm[33:40], val_net)
  }
  
  # Append the initial values
  
  l_net <- append(l_net,c(val_net,new_val_net))
  
  converged_net <- FALSE
  
  val_net <- new_val_net
  
  for (i in random_samp[-1]) {
    if (i == 1) {
      new_val_net <- PGF(pm[1:8], val_net)
    } else if (i == 2) {
      new_val_net <- PGF(pm[17:24], val_net)
    } else {
      new_val_net <- PGF(pm[33:40], val_net)
    }
    
    # if (abs(new_val_net - val_net) < 0.00001) {
    #   converged_net <- TRUE
    #   break
    # }
    
    l_net <- append(l_net, new_val_net)
    val_net <- new_val_net
  }
  
  ######## POSITIVE RETURNS   ######## 
  
  if (random_samp[1] == 1) {
    val_pos <- pm[9]
    new_val_pos <- PGF(pm[9:16], val_pos)
  } else if (random_samp[1] == 2) {
    val_pos <- pm[25]
    new_val_pos <- PGF(pm[25:32], val_pos)
  } else {
    val_pos <- pm[41]
    new_val_pos <- PGF(pm[41:48], val_pos)
  }
  
  # Append the initial values
  
  l_pos <- append(l_pos,c(val_pos,new_val_pos))
  
  converged_pos <- FALSE
  
  val_pos <- new_val_pos
  
  for (i in random_samp[-1]) {
    if (i == 1) {
      new_val_pos <- PGF(pm[9:16], val_pos)
    } else if (i == 2) {
      new_val_pos <- PGF(pm[25:32], val_pos)
    } else {
      new_val_pos <- PGF(pm[41:48], val_pos)
    }
    
    # if (abs(new_val_pos - val_pos) < 0.00001) {
    #   converged_pos <- TRUE
    #   break
    # }
    # 
    l_pos <- append(l_pos, new_val_pos)
    val_pos <- new_val_pos
  }
  # par(mfrow = c(2, 1))
  # plot(l_net,type = "l", col="blue4")
  # plot(l_pos,type = "l", col="blue4")
  return(list(round(l_net, 7),round(l_pos, 7)))
}

PGFF(pmf_btc_list)
#######

big_btc_sample <- replicate(100, PGFF(pmf_btc_list), simplify = FALSE)

results_btc_net <- sapply(big_btc_sample, `[[`, 1)
results_btc_pos <- sapply(big_btc_sample, `[[`, 2)

m_btc_net <- matrix(unlist(results_btc_net),100,1000, byrow = T)
m_btc_pos <- matrix(unlist(results_btc_pos),100,1000, byrow = T)

#View(m_btc_net)

vec_btc_net <- colMeans(m_btc_net)
vec_btc_pos <- colMeans(m_btc_pos)

mean(vec_btc_net)
mean(vec_btc_pos)


data_btc <- data.frame(
  time = 1:1000,
  Negative = vec_btc_net,
  Positive = vec_btc_pos
)

# Convert data to long format for ggplot2
library(tidyr)
data_long_btc <- pivot_longer(data_btc, cols = -time, names_to = "type", values_to = "value")

# Plotting the lines
ggplot(data_long_btc, aes(x = time, y = value, color = type, group = type)) +
  geom_line(size = 0.5) +           # Draw lines
  scale_color_manual(values = c("Negative" = "red4", "Positive" = "blue4")) +
  labs(y = "", x = "", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

length(vec_btc_net)

