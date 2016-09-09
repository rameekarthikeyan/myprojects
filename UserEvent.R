
#Loading the data.table and class libraries 
library(data.table)
library(class)
# Reading the training data and test data from amazon S3
rt <- fread("https://s3.amazonaws.com/checkindata/train/train.csv", colClasses = c("integer", "numeric", "numeric","integer", "integer", "character"))
testdata1 <- fread("https://s3.amazonaws.com/checkindata/test/test.csv",colClasses = c("integer", "numeric", "numeric","integer", "integer"))
# Splitting the given training data into internal training, Cross validation and test datasets
indx <- sample(3, nrow(rt), replace = TRUE, prob = c("0.6","0.2","0.2"))
traindata <- rt[indx == 1]
testdat <- rt[indx == 2]
cvdat <- rt[indx == 3]
indx <- NULL
rt <- NULL
setkeyv(traindata, "accuracy")
accuracylist <- unique(traindata$accuracy)
# Scaling the internal training data and converting to data table
traindat <- scale(traindata[, c("x", "y"), with = FALSE])
traindat <- as.data.table(traindat)
traindat$place_id <- traindata$place_id
traindat$accuracy <- traindata$accuracy
setkeyv(traindat, "accuracy")
# Scaling the internal cross validation data and converting to data table
cv <- scale(testdata1[, c("x", "y"), with = FALSE])
cv <- as.data.table(cv)
cv$accuracy <- testdata1$accuracy
cv$row_id <- testdata1$row_id
setkeyv(cv, "accuracy")
ll <- length(accuracylist)
pp <- NULL
for (i in 51:60)
{
ft <- traindat[.(accuracylist[i])]
nr <- nrow(ft)
ct <- cv[.(accuracylist[i])]
if (nr < 5) {
  k1 <- i
} else {
  k1 <- 5
}
if (is.na(ct[1]$x)) {
  knn.test <- traindat$place_id[1]
  pp[i] <- 0
} else {
# Running K nearest neighbors to training set with cross validation set as test set
knn.test <- knn(ft[, c(1:2), with = FALSE], ct[, c(1:2), with = FALSE], ft[, place_id], k = k1)
}
ct$predictedplace_id <- knn.test
filenm <- paste("myprediction_", i, ".csv", sep="")
write.csv(ct, filenm)
}
