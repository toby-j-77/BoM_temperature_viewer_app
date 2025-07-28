#librarying packages


library("tidyverse")
library("forecast")
library("rlist")

#test_data

test_data <- read_csv("Input_data/test_data.csv") |> as_tibble()

test_data |> print(n = 48)

#creating chunk ends

#splitting chunks (#used AI for help)

na_indices <- which(is.na(test_data$test_2))


na_indices

na_consecutive <- vector(mode = "list", length = 0)

for (i in 2:length(na_indices)) {
  #checking consecutive indices
  if (na_indices[[i]] - na_indices[[i - 1]] == 1) {
    # #check first index
    # if (i == 2 & na_indices[[2]] - na_indices[[1]] == 1) {
    #   na_consecutive <- list.append(na_consecutive, na_indices[[1]])
    # }
    #append numbers
    na_consecutive <- list.append(na_consecutive, na_indices[[i-1]])
    na_consecutive <- list.append(na_consecutive, na_indices[[i]])
  }
}

#remove duplicates

na_consecutive <- unique(na_consecutive)

na_consecutive

#finding start and end indices

na_ends_list <- vector(mode = "list", length = 0)

segment_length <- 1

segment_indices <- vector(mode = "list", length = 0)


for (j in 1:(length(na_consecutive) - 1)) {
  #counting consecutive segments length
  
  if (na_consecutive[[j + 1]] - na_consecutive[[j]] == 1) {
    #increase count
    segment_length <- segment_length + 1
    
    #append both j + 1 and j to account for the last element in a sequence
    segment_indices <- list.append(segment_indices, na_consecutive[[j]])
    segment_indices <- list.append(segment_indices, na_consecutive[[j + 1]])
  }
  
  else{
    #deciding to append lists
    
    if (segment_length >= 5) {
      #adding first and last value of segmment_indices into na_ends_list
      na_ends_list <- list.append(na_ends_list, c(
        segment_indices[[1]], 
        segment_indices[[length(segment_indices)]]))
      
      
      #resetting variables
      
      segment_length <- 1
      segment_indices <- vector(mode = "list", length = 0)
    }
    
    else{
      #reset variables
      
      segment_length <- 1
      segment_indices <- vector(mode = "list", length = 0)
    }
  }
  
  #checking the last segment
  
  if (segment_length >= 5) {
    #adding first and last value of segmment indices into na_ends
    na_ends_list <- list.append(na_ends_list, c(
      segment_indices[[1]], 
      segment_indices[[length(segment_indices)]]))
    
    #update k
    
    k <- k + 1
    
    segment_length <- 1
    segment_indices <- vector(mode = "list", length = 0)
  }
}

na_ends_list
