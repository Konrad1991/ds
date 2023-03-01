# https://tensorflow.rstudio.com/tutorials/quickstart/beginner
library(keras)
library(tensorflow)
tensorflow::use_virtualenv("r-reticulate")
tf$constant("Hello Tensorflow!")

# import data
c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()
x_train <- x_train / 255
x_test <-  x_test / 255

# sequentiel model
model <- keras_model_sequential(input_shape = c(28, 28)) %>%
  layer_flatten() %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(10)

# using the untrained model
predictions <- predict(model, x_train[1:2, , ])
predictions

# softmax converts logits into probabilities
tf$nn$softmax(predictions)

# loss fct used during training
loss_fn <- loss_sparse_categorical_crossentropy(from_logits = TRUE)
# call loss fct
loss_fn(y_train[1:2], predictions)

# compile the model
model %>% compile(
  optimizer = "adam",
  loss = loss_fn,
  metrics = "accuracy"
)

# Training
model %>% fit(x_train, y_train, epochs = 5)

# evaluate checks the performance of the model
model %>% evaluate(x_test,  y_test, verbose = 2)

#If you want your model to return which class had the highest probability,
# you can reuse the trained model to define a new sequential model that also calls softmax and argmax:
probability_model <- keras_model_sequential() %>%
  model() %>%
  layer_activation_softmax() %>%
  layer_lambda(tf$argmax)

probability_model(x_test[1:5, , ])
