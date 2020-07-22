###################Salary of test data##########
library(readr)
SalaryData_Test_1_ <- read_csv("E:/data science r studio/Assignment code 1/svm/SalaryData_Test(1).csv")
View(SalaryData_Test_1_)
head(SalaryData_Test_1_)

##svm train and test
Salary_train<-SalaryData_Test_1_[1:10541,]
Salary_test<-SalaryData_Test_1_[10542:15060,]
View(SalaryData_Test_1_$Salary)

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
mean(pred_rfdot==SalaryData_Test_1_$Salary) # 66.03

# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = Salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=Salary_test)
mean(pred_vanilla==SalaryData_Test_1_$Salary) #63.39


# kernal = besseldot
model_besseldot<-ksvm(Salary ~.,data = Salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=Salary_test)
mean(pred_bessel==SalaryData_Test_1_$Salary)#64.70

# kernel = polydot

model_poly<-ksvm(Salary ~.,data = Salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = Salary_test)
mean(pred_poly==SalaryData_Test_1_$Salary) # 65.39





