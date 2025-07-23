#custom function for calculating the middle day of the month

library("tidyverse")

month_middle <- function(DATE) {
  if_else(condition = {
    (month(DATE) %in% c(1, 3, 5, 7, 8, 10, 12))
  },
  true = {
    Date_middle <- floor_date(DATE, "month") + days(15)
  },
  false = {
    if_else(condition = {
      (month(DATE) %in% c(4, 6, 9, 11))
    },
    true = {
      Date_middle <- floor_date(DATE, "month") + days(14)
    },
    false = {
      if_else(condition = {
        ((year(DATE) / 4) %% 1 == 0)
      },
      true = {
        Date_middle <- floor_date(DATE, "month") + days(14)
      },
      false = {
        Date_middle <- floor_date(DATE, "month") + days(13)
      })
    })
  })
}