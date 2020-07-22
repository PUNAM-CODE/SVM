###############forestfires svm ########3
library(readxl)
library(readr)
 forestfires <- read_csv("E:/data science r studio/Assignment code 1/svm/forestfires.csv")
 update.packages(ask=FALSE, checkBuilt=TRUE)
 View(forestfires)
 #svn train and test
forestfires_train<-forestfires[1:361,]
forestfires_test<-forestfires[362:517,]
View(forestfires)
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 


# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(size_category ~.,data = forestfires_train,kernel = "vanilladot")
model1
attr(,"RStudio")
help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(size_category ~.,data =forestfires_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forestfires_test)
mean(pred_rfdot==forestfires$size_category) 

# kernel = vanilladot
model_vanilla<-ksvm(size_category ~.,data = forestfires_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=forestfires_test)
mean(pred_vanilla==forestfires$size_category)


# kernal = besseldot
model_besseldot<-ksvm(size_category ~.,data = forestfires_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forestfires_test)
mean(pred_bessel==forestfires$size_category)

# kernel = polydot

model_poly<-ksvm(size_category ~.,data = forestfires_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = letters_test)
mean(pred_poly==forestfires$size_category)




