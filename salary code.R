#########3#####################SVM#############
###############salary train data###########
library(readr)
SalaryData_Train_1_ <- read_csv("E:/data science r studio/Assignment code 1/svm/SalaryData_Train(1).csv")
View(SalaryData_Train_1_)
head(SalaryData_Train_1_)

write.csv(Salary,file="SalaryData_Train_1_.csv",col.names = F,row.names = F)

# For SVM all the features must be in numeric 
# All the feature values should be in same range 
# If not we should normalize 
# SVM model will perform Rescalling automatically
##svm train and test
Salary_train<-SalayData_Train_1_[1:21112,]
Salary_test<-SalaryData_Train_1_[21113:30161,]
View(SalaryData_Train_1_$Salary)

# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package

# kvsm() function uses gaussian RBF kernel 

# Building model 
 install.packages("klaR")
 install.packages("kernlab")
library(kernlab)
library(caret)
model1<-ksvm(Salary ~.,data = Salary_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = Salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=Salary_test)
mean(pred_rfdot==SalaryData_Train_1_$Salary) # 64.88

# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = Salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=Salary_test)
mean(pred_vanilla==SalaryData_Train_1_$Salary) #62.12


# kernal = besseldot
model_besseldot<-ksvm(Salary ~.,data = Salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=Salary_test)
mean(pred_bessel==SalaryData_Train_1_$Salary)#63.16

# kernel = polydot

model_poly<-ksvm(Salary ~.,data = Salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = Salary_test)
mean(pred_poly==SalaryData_Train_1_$Salary) # 65.12





