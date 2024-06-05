# Install and load required packages
install.packages(c("readr", "ggplot2", "forecast", "dplyr", "plotly"))

# Load the required library
library(readr)
library(ggplot2)
library(forecast)
library(plotly)
library(dplyr)

# Load weather data
weather_data <- read.csv("weather_data.csv")

# Display the data
print(weather_data)

# Descriptive statistics
IQR_temperature <- IQR(weather_data$temperature)
var_temperature <- var(weather_data$temperature)
sd_temperature <- sd(weather_data$temperature)

print(paste("IQR for Temperature:", IQR_temperature))
print(paste("Variance for Temperature:", var_temperature))
print(paste("Standard Deviation for Temperature:", sd_temperature))

# Covariance and correlation
cov_matrix <- cov(select(weather_data, humidity, temperature))
cor_temp_humidity <- cor(weather_data$temperature, weather_data$humidity)
cor_temp_rainfall <- cor(weather_data$temperature, weather_data$rainfall)

print("Covariance Matrix:")
print(cov_matrix)
print(paste("Correlation between Temperature and Humidity:", cor_temp_humidity))
print(paste("Correlation between Temperature and Rainfall:", cor_temp_rainfall))

#summarise
#Calculate the average temperature
avg_temp <- weather_data %>% summarise(AvgTemperature = mean(temperature, na.rm = TRUE))

# Calculate the total rainfall
total_rainfall <- weather_data %>% summarise(TotalRainfall = sum(rainfall, na.rm = TRUE))

print(avg_temp)
print(total_rainfall)

#filter
#Filter data for months with temperature above 28 degrees
high_temp_months <- weather_data %>% filter(temperature > 28)

#Filter data for months with rainfall greater than 200mm
heavy_rainfall_months <- weather_data %>% filter(rainfall > 200)

print(high_temp_months)
print(heavy_rainfall_months)

# Linear regression
input <- weather_data[, c("humidity", "rainfall")]
model <- lm(temperature ~ humidity + rainfall, data = weather_data)

cat("###The Coefficients Values###", "\n")
a <- coef(model)[1]
xhumi <- coef(model)[2]
xrain <- coef(model)[3]
print(a)
print(xhumi)
print(xrain)

# Summary of the regression model
summary(model)

# Assuming 'new_data' contains new humidity and rainfall values
new_data <- data.frame(humidity = c(75, 80), rainfall = c(150, 200))

# Predict temperature using the linear regression model
predicted_temperature <- predict(model, newdata = new_data)

# Display the predicted temperatures
print(predicted_temperature)

# Calculate the mean squared error
mse <- mean((weather_data$temperature - predicted_temperature)^2)
cat("Mean Squared Error:", mse, "\n")

# Assuming you want to predict 'rainfall' based on 'temperature'

lm_model <- lm(rainfall~temperature, data = weather_data)
summary(lm_model)

plot(weather_data$temperature,weather_data$rainfall, ylab = "Rainfall", xlab = "Temperature",
     main = "Temperature vs Rainfall")
abline(lm_model, col = "red")

# Add legend
legend("topright", legend = "Regression Line", col = "red", lty = 1)

# Line graph
weather_data$month <- as.Date(paste0(weather_data$month, "-01"))
line_plot <- ggplot(weather_data, aes(x = month)) +
  geom_line(aes(y = temperature, color = "Temperature")) +
  geom_line(aes(y = humidity, color = "Humidity")) +
  geom_line(aes(y = rainfall, color = "Rainfall")) +
  scale_color_manual(name = "Variables",
                     values = c("Temperature" = "blue", "Humidity" = "red", "Rainfall" = "green")) +
  labs(title = "Weather Trends Over Time",
       x = "Month",
       y = "Value") +
  theme_minimal()

# Pie chart
sum_temperature <- sum(weather_data$temperature)
sum_humidity <- sum(weather_data$humidity)
sum_rainfall <- sum(weather_data$rainfall)

pie_data <- data.frame(
  Variable = c("Temperature", "Humidity", "Rainfall"),
  Value = c(sum_temperature, sum_humidity, sum_rainfall)
)

pie_chart <- ggplot(pie_data, aes(x = "", y = Value, fill = Variable)) +
  geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.7) +
  coord_polar("y") +
  labs(title = "Proportion of Weather Variables",
       fill = "Variable") +
  theme_void()

# Bar graph
bar_plot <- ggplot(weather_data, aes(x = month)) +
  geom_bar(aes(y = temperature), stat = "identity", fill = "blue", alpha = 0.6) +
  geom_bar(aes(y = humidity), stat = "identity", fill = "red", alpha = 0.6) +
  geom_bar(aes(y = rainfall), stat = "identity", fill = "green", alpha = 0.6) +
  labs(title = "Weather Data",
       x = "Month",
       y = "Value") +
  theme_minimal()

#histogram for temperature
histogram<-ggplot(weather_data, aes(x = temperature)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Temperature Histogram", x = "Temperature (°C)", y = "Frequency")

# Scatter Plot for Temperature vs Rainfall
scatter_temp_rain<-ggplot(weather_data, aes(x = temperature, y = rainfall)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Temperature vs. Rainfall", x = "Temperature (°C)", y = "Rainfall (mm)")

#Scatter Plot for Humidity vs Rainfall
scatter_humi_rain<-ggplot(weather_data, aes(x = humidity, y = rainfall)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Humidity vs. Rainfall", x = "Humidity (%)", y = "Rainfall (mm)")


# Interactive line plot
interactive_line_plot <- plot_ly(weather_data, x = ~month) %>%
  add_lines(y = ~temperature, name = "Temperature", line = list(color = 'blue')) %>%
  add_lines(y = ~humidity, name = "Humidity", line = list(color = 'red')) %>%
  add_lines(y = ~rainfall, name = "Rainfall", line = list(color = 'green')) %>%
  layout(title = "Interactive Weather Trends Over Time",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Value"))

# Display the plots
print(bar_plot)
print(line_plot)
print(pie_chart)
print(histogram)
print(scatter_temp_rain)
print(scatter_humi_rain)
print(interactive_line_plot)

# Time series forecasting
weather_data$month <- as.Date(paste0(weather_data$month, "-01"))
weather_ts <- ts(weather_data$temperature, frequency = 12)
fit <- ets(weather_ts)
forecasted_values <- forecast(fit, h = 12)

print(forecasted_values)

plot(forecasted_values)
