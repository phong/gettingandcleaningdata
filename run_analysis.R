tmp1 <- read.table("data/train/X_train.txt")
tmp2 <- read.table("data/test/X_test.txt")
x <- rbind(tmp1, tmp2)
tmp1 <- read.table("data/train/subject_train.txt")
tmp2 <- read.table("data/test/subject_test.txt")
subject <- rbind(tmp1, tmp2)
tmp1 <- read.table("data/train/y_train.txt")
tmp2 <- read.table("data/test/y_test.txt")
y <- rbind(tmp1, tmp2)

features <- read.table("data/features.txt")
measurements <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x <- x[, measurements]
names(x) <- features[measurements, 2]
names(x) <- gsub("\\(|\\)", "", names(x))

activities <- read.table("data/activity_labels.txt")
activities[, 2] = gsub("_", "", as.character(activities[, 2]))
y[,1] = activities[y[,1], 2]
names(y) <- "activity"

names(subject) <- "subject"
new_data <- cbind(subject, y, x)

numActivity = length(activities[,1])
numCol = dim(new_data)[2]
uSubject = unique(subject)[,1]
numSubject = length(unique(subject)[,1])

result = new_data[1:(numSubject*numActivity), ]

row <- 1
for (s in 1:numSubject) {
  for (a in 1:numActivity) {
    result[row, 1] = uSubject[s]
    result[row, 2] = activities[a, 2]
    tmp <- new_data[new_data$subject==s & new_data$activity==activities[a, 2], ]
    result[row, 3:numCol] <- colMeans(tmp[, 3:numCol])
    row <- row+1
  }
}
write.table(result, "result.txt", row.names=FALSE)