rm(list = ls());
library(lattice);
library(ggplot2);
library(jpeg);

#read images
fileList <- list.files(path = "D:/MLN/Project1/Input/Image", full.names = TRUE);
numberOfImages <- length(fileList);

#a function to create a histogram of image "img"
createHistogram <- function(img, u1, v1, u2, v2, channel){
  #create 16 empty bins
  histogram <- 0 * c(1:16);
  #put the pixels from img into their own bins.
  for(i in u1:u2){
    for(j in v1:v2){
        x <- round( round(255 * img[i, j, channel] )/ 16 );
        histogram[x] <- histogram[x] + 1;
    }
  }
  return(histogram);
}

#Create our dataset
myData <- rbind();
#create each image's histogram
for (i in 1:numberOfImages){
  img<- readJPEG(fileList[i]);
  m <- nrow(img[ , , 1]);
  n <- ncol(img[ , , 1]);
  #center points
  m1 <- floor(m/2);
  n1 <- floor(n/2);
  
  x <- c();
  #with each channel
  for(channel in 1:3){
    x <- c(x, createHistogram(img, 1, 1, m, n, channel));
    x <- c(x, createHistogram(img, 1, 1, m1, n1, channel));
    x <- c(x, createHistogram(img, m1 + 1, n1 + 1, m, n, channel));
    x <- c(x, createHistogram(img, m1 + 1, 1, m, n1, channel));
    x <- c(x, createHistogram(img, 1, n1 + 1, m1, n, channel));
  }
  myData <- rbind(myData, x);
}

#set seed number
set.seed(1994);

#plot the sum of squares to decide k
myResults = 0 * c(1:30);
for(i in 1:30){
  myResults[i] <- kmeans(myData, i)$tot.withinss;
}
plot(myResults);

#Kmeans method with k =13
k <- 13;
maxstep <- 20;

result <- kmeans(myData, k);
#Export output to HTML
sink(file = "D:/MLN/Project1/output.html", type = "output");
cat("<p>");
cat("K = ", k);
cat("</p>");
for (i in 1 : k) {
  cat("<p>GROUP ", i, "</p>");
  for (j in 1 : numberOfImages) {
    if (result$cluster[j]==i) {
      cat("<img src = ", fileList[j], ">");
    }
  }
  cat("<p>--------------------------------------------------------------</p>");
}
sink(file = NULL, type = "output");