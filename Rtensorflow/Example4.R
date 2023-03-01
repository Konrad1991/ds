library(keras)
library(tensorflow)
tensorflow::use_virtualenv("r-reticulate")
tf$constant("Hello Tensorflow!")

set.seed(123)
x <- matrix(runif(2000, -1, 1), nrow = 100, ncol = 20)
beta <- runif(20, -1, 1)
y <- x %*% beta + rnorm(100, 0, 0.2)

# Create a sequential model
model <- keras_model_sequential()

# Add a dense layer with 20 input units
model %>% 
  layer_dense(units = 1, input_shape = c(20)) 

# Add two hidden layers with ReLU activation
model %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(20)) %>%
  layer_dense(units = 16, activation = "relu") 
  
  # Add an output layer with a linear activation
  model %>%
  layer_dense(units = 1, activation = "linear")

# Compile the model with a mean squared error loss function and an Adam optimizer
model %>% 
  compile(loss = "mse",
          optimizer = optimizer_adam(lr = 0.1))

x_tensor <- as_tensor(x)
y_tensor <- as_tensor(y)

# Train the model for 100 epochs
history <- model %>% 
  fit(x_tensor, y_tensor,
      epochs = 100,
      verbose = 0)

# Generate some new data
x_new <- matrix(runif(200, -1, 1), nrow = 10, ncol = 20)

# Make predictions with the model
y_pred <- model %>% 
  predict(as_tensor(x_new))

# Print the predictions
print(y_pred)

# Plot the results
plot(y_pred, pch = 16)
lines(x_new, col = "red")

