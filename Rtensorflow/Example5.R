library(tensorflow)
library(keras)

# Load the Boston Housing dataset from the MASS package
data(Boston, package = "MASS")

# Normalize the input features to have zero mean and unit variance
x <- scale(Boston[, -14])
y <- Boston[, 14]

# Split the data into training and validation sets (80%/20%)
train_indices <- sample(1:nrow(x), size = 0.8 * nrow(x), replace = FALSE)
x_train <- x[train_indices, ]
y_train <- y[train_indices]
x_val <- x[-train_indices, ]
y_val <- y[-train_indices]

# Define the neural network architecture
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(13)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

# Compile the model with a mean squared error loss function and an Adam optimizer
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = 0.001)
)

# Train the model on the training data for 100 epochs with a batch size of 32
history <- model %>% fit(
  x_train, y_train,
  epochs = 100,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

# Plot the training and validation loss curves
plot(history)

# Make predictions on the validation data
y_pred <- model %>% predict(x_val)

# Evaluate the model performance on the validation data using mean squared error
mse <- mean((y_pred - y_val)^2)
cat("Validation MSE:", mse, "\n")

plot(y_pred)
plot(y, pch = 19, col = "darkred")
