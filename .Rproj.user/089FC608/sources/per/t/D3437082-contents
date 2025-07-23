#split fill

library("tidyverse")
library("forecast")
library("rlist")

#defining split_fill


split_fill <- function(df, column, new_column, NA_length, RA_length) {
  #finding consecutive NA indices
  
  nas <- df %>% select(column)
  
  na_indices <- which(is.na(nas))
  
  na_consecutive <- vector(mode = "list", length = 0)
  
  for (i in 2:length(na_indices)) {
    #checking consecutive indices
    if (na_indices[[i]] - na_indices[[i - 1]] == 1) {
      #append numbers
      na_consecutive <- list.append(na_consecutive, na_indices[[i - 1]])
      na_consecutive <- list.append(na_consecutive, na_indices[[i]])
    }
  }
  
  #remove duplicates
  
  na_consecutive <- unique(na_consecutive)
  
  #creating na chunk pairs
  
  #code amended by AI
  
  # Initialization
  na_ends_list <- list()
  segment_indices <- list()
  segment_length <- 1
  
  for (j in 2:length(na_consecutive)) {
    if (na_consecutive[[j]] - na_consecutive[[j - 1]] == 1) {
      segment_length <- segment_length + 1
      segment_indices <- append(segment_indices, na_consecutive[j - 1])
      
      # Add j if it's the last value
      if (j == length(na_consecutive)) {
        segment_indices <- append(segment_indices, na_consecutive[j])
        if (segment_length >= NA_length) {
          na_ends_list <- append(na_ends_list, list(c(
            segment_indices[[1]], segment_indices[[length(segment_indices)]]
          )))
        }
      }
      
    } else {
      # Check previous segment
      segment_indices <- append(segment_indices, na_consecutive[j - 1])
      if (segment_length >= NA_length) {
        na_ends_list <- append(na_ends_list, list(c(
          segment_indices[[1]], segment_indices[[length(segment_indices)]]
        )))
      }
      
      # Reset
      segment_length <- 1
      segment_indices <- list()
    }
  }
  
  #convert NA indices to data indices
  
  for (k in 1:length(na_ends_list)) {
    #subtracting one from the start index
    
    na_ends_list[[k]][1] <- na_ends_list[[k]][[1]] - 1
    
    #adding one to the end index
    
    na_ends_list[[k]][2] <- na_ends_list[[k]][[2]] + 1
  }
  
  data_indices_raw <- vector(mode = "list", length = 0)
  
  #adding values
  
  #adding first value
  
  data_indices_raw <- list.append(data_indices_raw, c(1, na_ends_list[[1]][1]))
  
  #adding subsequent values
  
  #for looping
  
  for (l in 2:length(na_ends_list)) {
    #creating pair of values
    
    data_ends <- c(na_ends_list[[l - 1]][2], na_ends_list[[l]][1])
    #appending value
    data_indices_raw <- list.append(data_indices_raw, data_ends)
  }
  
  #adding the last chunk
  
  l = length(na_ends_list)
  
  data_indices_raw <- list.append(data_indices_raw, c(na_ends_list[[l]][2], nrow(df)))
  
  
  #checking length of each chunk
  
  data_indices <- vector(mode = "list", length = 0)
  
  for (m in 1:length(data_indices_raw)) {
    index_chunk <- data_indices_raw[[m]]
    if (index_chunk[2] - index_chunk[1] > RA_length - 2) {
      data_indices <- list.append(data_indices, index_chunk)
    }
  }
  
  #performing RA
  
  #looping
  
  #creating a list of chunks
  
  data_chunks <- vector(mode = "list", length = 0)
  
  for (n in 1:length(data_indices)) {
    #saving each chunk into data_chunks
    
    data_chunks <- list.append(data_chunks,
                               df %>% slice(data_indices[[n]][1]:data_indices[[n]][2]))
  }
  
  #create an empty column for df
  
  df <- df %>%
    mutate(RA = NA)
  
  #calculating running average for each chunk and adding back to original dataset
  
  for (n in 1:length(data_chunks)) {
    #fill in NAs
    data_chunks[[n]] <- data_chunks[[n]] %>%
      mutate(column = zoo::na.approx(column)) %>%
      mutate(new_columm = TTR::SMA(column, 3))
  }
  
  
  #adding data back (AI generated and then I edited the code)
  
  for (n in 1:length(data_chunks)) {
    # Extract the row indices for the current chunk
    chunk_start <- data_indices[[n]][1]
    chunk_end <- data_indices[[n]][2]
    
    # Replace the RA column and column column
    df[chunk_start:chunk_end, deparse(substitute(new_column))] <- data_chunks[[n]]$new_column
    df[chunk_start:chunk_end, deparse(substitute(column))] <- data_chunks[[n]]$column
  }
  
  
}


#testing

test <- read_csv("Input_data/test_data.csv")

test_filled <- split_fill(test, "test_2", RA_3, 5, 3)