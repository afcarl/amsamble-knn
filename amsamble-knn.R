#!/usr/bin/R

library(class)

## read data
data <- read.csv("letter-recognition.data", header=FALSE)
data <- data[sample(nrow(data)), ]

sample.ratio <- 0.8
train.samples <- 1:(sample.ratio * nrow(data))
test.samples <- (tail(train.samples) + 1):nrow(data)

train <- data[train.samples, -1]
train.class <- data[train.samples, 1]
test <- data[test.samples, -1]
test.class <- data[test.samples, 1]

result <- knn(train, test, train.class, k=1)
result.merged <- data.frame(answer=test.class, predict=result)
hit <- apply(result.merged, 1, function(x){ifelse(x[1] == x[2], 1, 0)})
table(hit) / length(hit)

data.index <- seq(1, nrow(data), 0.01 * nrow(data))
test.samples <- (tail(data.index)+1):nrow(data)
test <- data[test.samples, -1]
test.class <- data[test.samples, 1]
results <- c()
for (i in 2:length(data.index)) {
  train.samples <- (data.index[i-1]):(data.index[i])
  train <- data[train.samples, -1]
  train.class <- data[train.samples, 1]
  result <- knn(train, test, train.class, k=1)
  result.merged <- data.frame(answer=test.class, predict=result)
  hit <- apply(result.merged, 1, function(x){ifelse(x[1] == x[2], 1, 0)})
  print(table(hit) / length(hit))
  results <- c(results, result)
}

results.mx <- matrix(results, nrow=length(result))
most_occur <- function(x) {
  .tbl <- table(x)
  .idx <- which(.tbl == max(.tbl))[1]
  return(as.integer(names(.tbl[.idx])))
}
result.amsamble <- apply(results.mx, 1, most_occur)
result.amsamble.labeled <- levels(data[, 1])[result.amsamble]
result.merged <- data.frame(answer=test.class, predict=result.amsamble.labeled)
hit <- apply(result.merged, 1, function(x){ifelse(x[1] == x[2], 1, 0)})
print(table(hit) / length(hit))
