
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
accuracylist <- as.list(accuracylist)
listfn <- function(z) {
 ft <- traindat[.(z)] 
 nr <- nrow(ft)
 ct <- cv[.(z)]
 if (nr < 5) {
   k1 <- nr
 } else {
   k1 <- 5
 }
 if (is.na(ct[1]$x)) {
   knn.test <- traindat$place_id[1]
    } else {
    # Running K- nearest neighbors to the training set 
   knn.test <- knn(ft[, c(1:2), with = FALSE], ct[, c(1:2), with = FALSE], ft[, place_id], k = k1)
 }
 ct$predictedplace_id <- knn.test
 return(ct)
}
nt <- lapply(accuracylist, listfn)
tryy <- rbindlist(nt)
nerr <- na.omit(tryy)
wrrr <- nerr[, c("row_id", "predictedplace_id"), with = FALSE]
setkeyv(wrrr, "row_id")
colnames(wrrr) <- c("row_id", "place_id")
filenm <- "oneprediction.csv"
write.csv(wrrr, filenm, row.names = FALSE)
#}
