# bmi_calculator.R

calculate_bmi <- function(weight_kg, height_m) {
  weight_kg / (height_m^2)
}

calculate_bmi(70, 1.75)