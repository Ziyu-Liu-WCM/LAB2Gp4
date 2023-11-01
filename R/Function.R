
#' Function 1: Write a function that makes a boxplot of payments by DRG code.
#' Make it an option for your function to do this for either the average Medicare payments,
#' the average total payment, or the average covered charges.
#'
#' @param data a data frame
#' @param payment_type a string name for variable y and a string from c("mean","median","standard deviation")
#'
#' @return boxplots and numbers
#' @export
#'
#' @examples
#' create_boxplot(DRG_data, payment_type = "Average Total Payments")
#' create_boxplot(DRG_data, payment_type = "Average Medicare Payments")
#' create_boxplot(DRG_data, payment_type = "Average Covered Charges")
#'
#' Medipay_stat("mean")
#' Medipay_stat("standard deviation")
#' Medipay_stat("median")
#'

library(tidyverse)
library(readr)

create_boxplot <- function(data, payment_type = "Average Medicare Payments") {
  valid_payment_types <- c("Average Medicare Payments", "Average Total Payments", "Average Covered Charges")
  if (!payment_type %in% valid_payment_types) {
    stop("Invalid payment type. Please choose from: 'Average Medicare Payments', 'Average Total Payments', 'Average Covered Charges'")
  }

  ggplot(data, aes(x = `DRG Definition`, y = !!sym(payment_type))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", payment_type), x = "DRG Definition", y = payment_type)
}


Medipay_stat <- function(x){
  if(x == "mean"){
    mean <- round(mean(DRG_data$`Average Medicare Payments`), 2)
    print(paste("The mean of average Medicare payments is", mean))
  } else if(x == "median"){
    median <- round(median(DRG_data$`Average Medicare Payments`), 2)
    print(paste("The median of average Medicare payments is", median))
  } else if(x == "standard deviation"){
    sd <- round(sd(DRG_data$`Average Medicare Payments`), 2)
    print(paste("The standard deviation of average Medicare payments is", sd))
  } else print("Invalid input, you must choose from mean, median and standard deviation ")
}
