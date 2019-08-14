# train and test a ANN w/ the neuralnet package

install.packages('neuralnet')
library('neuralnet')

# Prepare datasets
sample_data <- read.table('sample2.txt',header=FALSE)
print(sample_data[,1]) # print 1st column
str(sample_data)

# Split data into training and test data
train_idx <- sample(nrow(sample_data), 2/3 * nrow(sample_data))
sample_train <- sample_data[train_idx, ]
sample_test <- sample_data[-train_idx, ]

# Set-up and train ANN
net <- neuralnet(V1~ V2+ V3+ V4+ V5+ V6, sample_train, c(5,8), stepmax=1e6, threshold = 0.1, lifesign = 'full', algorithm = 'backprop', learningrate = 0.05, linear.output = FALSE)
plot(net)

# Test and evaluate ANN
pred <- predict(net, sample_test)
head(round(pred))

# Do a ROC plot
Nsample <- dim(sample_test)[1]
plot(c(0,1),c(0,1))
v <- predict(net, sample_test)
p <- sample_test[order(v),1]
nc <- sum(sample_test[,1]==0)
nd <- Nsample-nc
nnc <- sum(round(v[,1])==0)
nnd <- Nsample - nnc
for (i in 1:length(p)) {
  if(p[i]==1) {
    nd <- nd-1
  } 
  else {
    nc <- nc-1
  }
  points(nc/nnc,nd/nnd,pch='.') 
}

