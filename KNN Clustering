rm(list=ls())

data <- iris

#Assigning row labels
row_labels = data[,5]

#Encoding the target feature as factor
data$Species <- as.numeric(data$Species)

#We want to scale our data
data[,1:4] <- scale(data[,1:4])

#Set seed for splitting data 80/20
set.seed(123)

# Get the size to split, where size is the size of our training
# And, the negative split for our testing
size <- floor(0.8 * nrow(data))

# Obtain row numbers
train_ind <-sample(seq_len(nrow(data)),size =size)

# Training labels
train_labels <- data[train_ind, 5]
# Testing labels
test_labels <- row_labels[-train_ind]
# Training data
data_train <- data[train_ind,1:4]
# Testing data
data_test <- data[-train_ind,1:4]


# Model Rin
library(class)
predictions <- knn(train = data_train,
                  test = data_test,
                  cl = train_labels,
                  k =round(sqrt(nrow(data_train))))

# Let us focus on the subject of plotting our values
plot_predictions <- data.frame(
  data_test$Sepal.Length,
  data_test$Sepal.Width,
  data_test$Petal.Length,
  data_test$Petal.Width,
  predicted = predictions
)



# Renaming col name
colnames(plot_predictions) <- c("Sepal.Length",
                                "Sepal.Width",
                                "Petal.Length",
                                "Petal.Width",
                                "predicted"
                                )

# Let us plot our data
library(ggplot2)
library(gridExtra)

p1 <- ggplot(plot_predictions, aes(Sepal.Length,
                                   Sepal.Width,
                                   color = predicted, 
                                   fill = predicted)) +
geom_point(size  =5) +
geom_text(aes(label = test_labels), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between Sepal Length and Width") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

p2<- ggplot(plot_predictions, aes(Petal.Length,
                                  Petal.Width,
                                  color = predicted, 
                                  fill = predicted)) +
  geom_point(size  =5) +
  geom_text(aes(label = test_labels), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between Petal Length and Width") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

grid.arrange(p1,p2, ncol = 2)
