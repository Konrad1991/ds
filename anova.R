library(ggplot2)
grand_mean <- data.frame(y = mean(iris$Sepal.Length))

group_means <- sapply(unique(iris$Species), function(x) {
  temp <- iris[iris$Species == x, 1]
  mean(temp)
})
length_group_means <- sapply(unique(iris$Species), function(x) {
  temp <- iris[iris$Species == x, 1]
  length(temp) |> as.numeric()
})

ssw <- sapply(unique(iris$Species), function(x) {
  temp <- iris[iris$Species == x, 1]
  temp <- (temp - mean(temp))^2
})
ssw <- sum(ssw)

ssb <- Map(function(x, y) {
  (grand_mean$y - x)^2*y
}, group_means , length_group_means)
ssb <- sum(ssb |>  unlist())

sst <- sum( (iris$Sepal.Length - grand_mean$y)^2)

p1 <- ggplot() +
  geom_point(data = iris, aes(x = Species, y = Sepal.Length)) +
  geom_hline(data = grand_mean, aes(yintercept = y)) +
  geom_text(data = grand_mean, aes(0.65, y + .1, label = "grand mean") ) +
  stat_summary(fun = "mean", colour = "darkred", 
               data = iris, aes(x = Species, group = Species, y = Sepal.Length)) +
  geom_text(data = data.frame(y = group_means + 0.5, x = 1:3),
            aes(x = x, y = group_means, label = "group means"),
            colour = "darkred")
df <- iris
df$Species <- as.integer(df$Species) |>  as.numeric()

ssw_raw <- sapply(unique(iris$Species), function(x) {
  temp <- iris[iris$Species == x, 1]
  temp <- (temp - mean(temp))^2
})
ssw_raw <- data.frame(y = c(ssw_raw[,1],
                            ssw_raw[,2],
                            ssw_raw[,3]),
                      x = df$Species)
ssw_raw$y <- iris$Sepal.Length + ssw_raw$y 
ssw_raw$y2 <- iris$Sepal.Length
ssw_raw$y3 <- c(rep(group_means[1], 50),
                rep(group_means[2], 50),
                rep(group_means[3], 50))
  
ggplot(ssw_raw, aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = y3),
               position = position_dodge2(width = 0.3), colour = "darkred") +
  geom_point(position = position_dodge2(width = 0.3), colour = "darkblue") 

