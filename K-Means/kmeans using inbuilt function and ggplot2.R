library(datasets)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
set.seed(20)
Cluster <- kmeans(iris[, 3:4],iter.max=20,centers=3, nstart = 20)
Cluster$cluster <- as.factor(Cluster$cluster)
Cluster
table(Cluster$cluster, iris$Species)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Cluster$cluster)) + geom_point()
Cluster$centers
