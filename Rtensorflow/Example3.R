library(keras)
library(tensorflow)
tensorflow::use_virtualenv("r-reticulate")
tf$constant("Hello Tensorflow!")

set.seed(123)
x <- runif(100, -1, 1)
y <- 2*x + rnorm(100, 0, 0.2)

# Create a sequential model
model <- keras_model_sequential()

# Add a single input layer
model %>% 
  layer_dense(units = 1, input_shape = c(1)) 

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
x_new <- runif(10, -1, 1)

# Make predictions with the model
y_pred <- model %>% 
  predict(as_tensor(x_new))

# Plot the results
plot(x, y, pch = 16)
lines(x_new, y_pred, col = "red")
