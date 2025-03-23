# Load required libraries
library(ggplot2)

# Function to load and prepare data
load_data <- function() {
  data <- mtcars
  data$car_name <- rownames(mtcars)  # Adding a character column (datatype 1)
  data$hp_factor <- as.factor(ifelse(data$hp > 150, "High", "Low"))  # Factor variable (datatype 2)
  return(data)
}

# Function to fit linear regression model
fit_model <- function(data) {
  model <- lm(mpg ~ hp, data = data)
  return(model)
}

# Function to display model summary
display_summary <- function(model) {
  print(summary(model))  # Display output to screen
}

# Function to generate predictions for new horsepower values
generate_predictions <- function(model, hp_values) {
  new_data <- data.frame(hp = hp_values)  # Dataframe (datatype 3)
  predictions <- predict(model, newdata = new_data)
  return(data.frame(hp = hp_values, predicted_mpg = predictions))  # Dataframe as return
}

# Function to visualize data and regression model
plot_regression <- function(data, model) {
  ggplot(data, aes(x = hp, y = mpg)) +
    geom_point(aes(color = hp_factor)) +  # Visualization of data
    geom_smooth(method = "lm", formula = y ~ x, col = "blue") +  # Regression line
    labs(title = "Linear Regression: MPG vs. Horsepower", x = "Horsepower", y = "Miles Per Gallon") +
    theme_minimal()
}

# Main function to run the analysis
run_analysis <- function() {
  data <- load_data()
  model <- fit_model(data)
  
  display_summary(model)  # Print model results
  
  # Generate predictions for a sequence of horsepower values
  hp_values <- seq(50, 250, by = 10)  # Numeric sequence (datatype 4)
  predictions <- generate_predictions(model, hp_values)
  print(predictions)  # Display output to screen
  
  # Visualization
  print(plot_regression(data, model))
  
  # Example of using a list (datatype 5) and looping through it
  model_results <- list(
    Coefficients = coef(model),
    R_Squared = summary(model)$r.squared,
    P_Values = summary(model)$coefficients[,4]
  )
  
  for (name in names(model_results)) {
    cat("\n", name, ":\n", model_results[[name]], "\n")
  }
}

# Run the analysis
run_analysis()
