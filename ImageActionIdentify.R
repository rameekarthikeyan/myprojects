# Loading the required packages
library(neuralnet)
library(data.table)
library(e1071)
library(randomForest)
library(ranger)
mydat <- NULL
# function to read data
readfiles <- function(filehead, c) {
  xn <- function(x) {
    nt <- unlist(strsplit(x, "data"))
  }
  listnn <- unlist(lapply(filehead, xn))
# function to read all the images from different directories in Amazon S3
  datcreate <- function(x) {
    circ <- function(y) {
      filenm <- paste("'https://s3.amazonaws.com/statefarmdata/train/", paste(x, "data", sep=""), as.character(y), ".csv'", sep="")
      dtnm <- paste("c", c, "train",x, sep="")
      exprnm <- paste(dtnm, "<-", "fread(", filenm, ")", sep="")
      eval(parse(text =exprnm))
    }
    aa <- lapply(c,circ)
  }
  
  ww <- lapply(listnn,datcreate)
  
}

grpp <- function(dd) {
  if (!require(abind)) {
    install.packages("abind") 
  }
  library(abind)
  mv <- function(x) {
    abind(x, along = 1)
  }
  mm <- lapply(dd, mv)
}
# Features were extracted from the images and saved into different categories
# head data refers to the image region where head of the person usually will be
# cupdata refers to the image region where cup is being held by the person
# wheeldata refers to the image region where steering wheel usually will be
# rightdata refers to the image region where right hand of the person usually will be
# leftdata refers to the image region where left hand of the person usually will be
# radiodata refers to the image region where radio of the vehicle usually will be
# turndata refers to the image region where if the person turns to back of the vehicle
filehead <- c("headdata", "cupdata", "wheeldata", "rightdata", "leftdata", "radiodata", "turndata")
c <- c(0,1,2,3,4,5,6,7,8,9)

mm <- readfiles(filehead, c)
rr <- grpp(mm)
# Training data is split into internal training, cross validation and test sets
splitt <- function(x) {
  x <- as.data.table(x)
  sn <- sample(3, nrow(x), replace = TRUE, prob = c('0.6','0.2','0.2'))
  x1 <- x[sn == 1]
  x2 <- x[sn == 2]
  x3 <- x[sn == 3]
  list(x1,x2,x3)
}

splitdat <- lapply(rr, splitt)

# function to run ranger algorithm to internal training data
myfn <- function(x,y) {
  tr <- x[[1]]
  tr$V1 <- as.factor(tr$V1)
  myranger <- ranger(V1 ~ ., data = tr, write.forest = TRUE)
  tstnm <- paste("https://s3.amazonaws.com/statefarmdata/test/test",y, "data0.csv", sep="")
  tstread <- read.csv(tstnm)
  tstread <- as.data.table(tstread)
  tstread <- tstread[1:79725,]
  seqz <- 1:ncol(tstread)
  concat <- function(x) {
    paste("V",x, sep="")
  }
  sg <- unlist(lapply(seqz, concat))
  colnames(tstread) <- sg
  # predictions are calculated
  mypred1 <- predict(myranger, tstread[1:20000,-"V1", with = FALSE],  classification = TRUE)
  mypred2 <- predict(myranger, tstread[20001:40000,-"V1", with = FALSE], classification = TRUE)
  mypred3 <- predict(myranger, tstread[40001:60000,-"V1", with = FALSE], classification = TRUE)
  mypred4 <- predict(myranger, tstread[60001:nrow(tstread),-"V1", with = FALSE], classification = TRUE)
  totalpred <- c(mypred1$predictions, mypred2$predictions, mypred3$predictions, mypred4$predictions)
  mytable <-data.table(tstread$V1, totalpred)
  return(mytable)
  }
yesmee <- mapply(myfn, splitdat,c("head", "cup", "wheel", "right", "left", "radio", "turn"))

headpr <- yesmee[[2]]
cuppr <- yesmee[[4]]
wheelpr <- yesmee[[6]]
rightpr <- yesmee[[8]]
leftpr <- yesmee[[10]]
radiopr <- yesmee[[12]]
turnpr <- yesmee[[14]]

subfn <- function(x) {
  x-1
  }

headpr <- subfn(headpr)
cuppr <- subfn(cuppr)
wheelpr <- subfn(wheelpr)
rightpr <- subfn(rightpr)
leftpr <- subfn(leftpr)
radiopr <- subfn(radiopr)
turnpr <- subfn(turnpr)

newr <- cbind(headpr, cuppr, wheelpr, rightpr, leftpr, radiopr, turnpr)
newr <- data.table(newr)

good0 <- c(0,0,0,0,0,0,0)
right1 <- c(0,0,1,0,0,0,0)
left1 <- c(4,0,2,0,0,0,0)
left2 <- c(0,0,2,0,0,0,0)
radio1 <- c(0,0,0,0,0,1,0)
radio2 <- c(0,0,1,0,0,1,0)
turn1 <- c(0,0,1,0,0,0,1)
turn2 <- c(3,0,1,0,0,0,1)
next9 <- c(5,0,0,0,0,0,0)
c0 = 0.1
c1 =0.1
c2 = 0.1
c3 = 0.1
c4 = 0.1
c5 = 0.1
c6 = 0.1
c7 = 0.1
c8 = 0.1
c9 = 0.1

eachone <- function(x) {
    gd <- x-good0
    hd <- x-right1
    id <- x-left1
    jd <- x-left2
    kd <- x-radio1
    ld <- x-radio2
    md <- x-turn1
    nd <- x-turn2
    od <- x-next9
  if (length(gd[gd == c(0,0,0,0,0,0,0)]) ==7) {
    c0 = 1
  }
    if (length(hd[hd == c(0,0,0,0,0,0,0)]) ==7) {
      c1 = 0.25
      c2 = 0.25
      c6 =0.25
      c8 = 0.25
    }
    if (length(id[id == c(0,0,0,0,0,0,0)]) ==7 || length(jd[jd == c(0,0,0,0,0,0,0)]) ==7 ) {
      c3 = 0.50
      c4 = 0.50
        }
    if (length(kd[kd == c(0,0,0,0,0,0,0)]) ==7 || length(ld[ld == c(0,0,0,0,0,0,0)]) ==7 ) {
      c5 = 1
    }
    if (length(md[md == c(0,0,0,0,0,0,0)]) ==7 || length(nd[nd == c(0,0,0,0,0,0,0)]) ==7 ) {
      c7 = 1
    }
    if (length(od[od == c(0,0,0,0,0,0,0)]) ==7 ) {
      c9 = 1
    }
      return(c(c0,c1,c2,c3,c4,c5,c6,c7,c8,c9))
   }

gg <-lapply(transpose(newr), eachone)
xd <- lapply(gg, as.list)
bg <- rbindlist(xd)
# Actual test data set is read
testim <- fread("https://s3.amazonaws.com/statefarmdata/test/testimgidd.csv")
testim <- testim[2:nrow(testim),]

# Formatting the data to match the submission file
pastefn <- function(x) {
  paste("img_", x, ".jpg", sep="")
}

mpaste <- unlist(lapply(testim, pastefn))
finalresu <- cbind(mpaste,bg)
colnames(finalresu) <- c("img","c0","c1","c2","c3","c4","c5","c6","c7","c8","c9")
# submission file is written to disk
write.csv(finalresu, "submission.csv", row.names = FALSE)

