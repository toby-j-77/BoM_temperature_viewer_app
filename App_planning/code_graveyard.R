#testing something

#librarying packages

library("tidyverse")
library("RColorBrewer")
library("forecast")
library("ggthemes")
library("shiny")

#loading in broken hill data

BH_min <- read_csv("Input_data/Broken_hill_min.csv") |> as_tibble()
BH_max <- read_csv("Input_data/Broken_hill_max.csv") |> as_tibble()

#fixing up data structure


names(BH_min) <- c(
  "Product_code",
  "Station_number",
  "Year",
  "Month",
  "Day",
  "Min_T",
  "Days_of_accumulation_min_T",
  "Quality_min_T"
)

names(BH_max) <- c(
  "Product_code",
  "Station_number",
  "Year",
  "Month",
  "Day",
  "Max_T",
  "Days_of_accumulation_max_T",
  "Quality_max_T"
)

#correcting data structure

BH_min <- BH_min %>%
  mutate(Quality_min_T = as_factor(Quality_min_T)) %>%
  mutate(Date = paste(Year, Month, Day, sep = "-")) %>%
  mutate(Date = as_date(Date)) %>%
  mutate(Month = as.factor(Month))

BH_max <- BH_max %>%
  mutate(Quality_max_T = as_factor(Quality_max_T)) %>%
  mutate(Date = paste(Year, Month, Day, sep = "-")) %>%
  mutate(Date = as_date(Date)) %>%
  mutate(Month = as.factor(Month))

BH_temp <- right_join(BH_min, BH_max, by = "Date", keep = FALSE)

#removing extra columns

BH_temp <- BH_temp %>%
  select(-ends_with(".y"))

#removing .x at the end of columns

names(BH_temp) <- c(
  "Product_code",
  "Station_number",
  "Year",
  "Month",
  "Day",
  "Min_T",
  "Days_of_accumulation_min_T",
  "Quality_min_T",
  "Date",
  "Max_T",
  "Days_of_accumulation_max_T",
  "Quality_max_T"
)

#removing leading and trailing NAs

BH_temp <- BH_temp[min(which(!is.na(BH_temp$Min_T))):max(which(!is.na(BH_temp$Min_T))),]

#basic timeseries with moving average

BH_temp_MA <- BH_temp %>%
  mutate(Min_T = zoo::na.approx(Min_T)) %>%
  mutate(Max_T = zoo::na.approx(Max_T)) %>%
  mutate(MA_min_T = TTR::SMA(Min_T, 365)) %>%
  mutate(MA_max_T = TTR::SMA(Max_T, 365)) %>%
  select(Min_T, Max_T, MA_min_T, MA_max_T, Date) %>%
  pivot_longer(
    cols = c(Min_T, Max_T, MA_min_T, MA_max_T),
    names_to = "Min_or_max",
    values_to = "Temp"
  )
#plotting

timeseries_plot_MA <- ggplot(BH_temp_MA) +
  geom_line(aes(
    x = Date,
    y = Temp,
    col = Min_or_max,
    alpha = Min_or_max
  )) +
  labs(x = "Date",
       y = "Temperature (\u00b0C)",
       col = "Min/Max",
       alpha = "Min/Max") +
  theme_bw() +
  scale_colour_manual(
    values = c("red", "blue", "red", "blue"),
    labels = c(
      "Running average maximum",
      "Running average minimum",
      "Maximum",
      "Minimum"
    )
  ) +
  scale_alpha_manual(
    values = c(1, 1, 0.25, 0.25),
    labels = c(
      "Running average maximum",
      "Running average minimum",
      "Maximum",
      "Minimum"
    )
  )

timeseries_plot_MA

?na.approx

#code

#testing

# UI_test <- fluidPage(
#   downloadButton(outputId = "download", label = "download stuff"
# ))
# 
# UI_test

# chunk <- test_data$test_2[data_indices[[m]][1]:data_indices[[m]][2]]
# 
# #interpolating and using running average
# 
# chunk <- chunk %>% 
#   zoo::na.approx() %>%
#   TTR::SMA(3)
# 
# #adding data back
# 
# test_data$test_2[data_indices[[m]][1]:data_indices[[m]][2]] <- chunk

data_joined_RA <- data_joined %>%
  mutate(Min_T = zoo::na.approx(Min_T)) %>%
  mutate(Max_T = zoo::na.approx(Max_T)) %>%
  mutate(RA_min_T = TTR::SMA(Min_T, as.integer(RA_selection))) %>%
  mutate(RA_max_T = TTR::SMA(Max_T, as.integer(RA_selection))) %>%
  select(Min_T, Max_T, RA_min_T, RA_max_T, Date) %>%
  pivot_longer(
    cols = c(Min_T, Max_T, RA_min_T, RA_max_T),
    names_to = "Min_or_max",
    values_to = "Temp"
  )

data_joined_RA <- data_joined %>%
  mutate(RA_min_T = split_fill(Min_T, 
                               na_length = as.integer(RA_selection),
                               ra_length = as.integer(RA_selection))) %>%
  mutate(RA_max_T = split_fill(Max_T, 
                               na_length = as.integer(RA_selection),
                               ra_length = as.integer(RA_selection))) %>%
  select(Min_T, Max_T, RA_min_T, RA_max_T, Date) %>%
  pivot_longer(
    cols = c(Min_T, Max_T, RA_min_T, RA_max_T),
    names_to = "Min_or_max",
    values_to = "Temp"
  )
