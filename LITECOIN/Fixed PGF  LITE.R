
COMBINED_lite <- combined_data_lite_2018_2019+combined_data_lite_2020_2021+combined_data_lite_2022_2023

rownames(COMBINED_lite) <- c("Negative","Positive")

COMBINED_RF_lite <- COMBINED_lite/312

pgf_fixed <- function(x){
  
                   ##NEGATIVE RETURNS##
  l_net <- c(x[1])
  val_net <- x[1]
  
  repeat {
    new_val_net <- sum(x[seq(1,15,2)] * val_net^(0:7))  # Compute the new value based on the PGF formula
    if (abs(new_val_net - val_net) < 0.0001) break  # Check for convergence
    val_net <- new_val_net  # Update the value for the next iteration
    l_net <- append(l_net, val_net)  # Append the new value to the list
  }
  
                            ##POSITIVE RETURNS##
  
  l_pos <- c(x[2])
  val_pos <- x[2]
  
  repeat {
    new_val_pos <- sum(x[seq(2,16,2)] * val_pos^(0:7))  # Compute the new value based on the PGF formula
    if (abs(new_val_pos - val_pos) < 0.0001) break  # Check for convergence
    val_pos <- new_val_pos  # Update the value for the next iteration
    l_pos <- append(l_pos, val_pos)  # Append the new value to the list
  }
  
  plot(l_pos,type = "l", col="blue4")
  return(list(l_net,l_pos))
  
}

pgf_fixed(COMBINED_RF_lite)  



six_years_totals_lite <- cross_tab_with_totals_lite_2018_2019+cross_tab_with_totals_lite_2020_2021+cross_tab_with_totals_lite_2022_2023
