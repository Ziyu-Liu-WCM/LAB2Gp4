



create_boxplot <- function(data, payment_type = "Average Medicare Payments") {
  valid_payment_types <- c("Average Medicare Payments", "Average Total Payments", "Average Covered Charges")
  if (!payment_type %in% valid_payment_types) {
    stop("Invalid payment type. Please choose from: 'Average Medicare Payments', 'Average Total Payments', 'Average Covered Charges'")
  }

  ggplot(data, aes(x = `DRG Definition`, y = !!sym(payment_type))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", payment_type), x = "DRG Definition", y = payment_type)
}
