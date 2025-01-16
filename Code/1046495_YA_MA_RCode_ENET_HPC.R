set.seed(42)

library(readxl)
library(Matrix)
library(glmnet)
library(dplyr,warn.conflicts = FALSE)
library(tidyr,warn.conflicts = FALSE)
library(ggplot2)
library(writexl)


index <- Sys.getenv(c("SLURM_ARRAY_TASK_ID")) 
lower = seq(10,10100,by=10)-9
upper = seq(10,10100,by=10)
boundaries = data.frame(cbind(lower,upper))
lower = boundaries[index,1]
upper = boundaries[index,2]
name_rmse = paste0("rmse_",index,".xlsx")

manipulated_x_2 <- as.data.frame(read_excel("manipulated_x_2.xlsx"))
data_principal_components <- as.data.frame(read_excel("data_principal_components.xlsx"))
approval_q <- as.data.frame(read_excel("approval_q.xlsx"))



y=approval_q$Approval[15:nrow(approval_q)]
x = manipulated_x_2
x=na.omit(x)
x = data.frame(cbind(x,lag(x,n = 1L),lag(x,n = 2L),lag(x,n = 3L),lag(x,n = 4L),lag(y,n = 1L),lag(y,n = 2L),lag(y,n = 3L),lag(y,n = 4L)))
colnames(x)=c(rep(colnames(data_principal_components), times = 5),"Lag Y_1","Lag Y_2","Lag Y_3","Lag Y_4")
data = data.frame(cbind(x,y))
data=na.omit(data)
x_m = as.matrix(data[,-ncol(data)])
y_m=data[,ncol(data)]
colnames(x_m)=c(rep(colnames(data_principal_components), times = 5),"Lag Y_1","Lag Y_2","Lag Y_3","Lag Y_4")


alpha_seq <- seq(0, 1, by = 0.01)  # Alpha values from 0 to 0.99 (excluding 1)
lambda_seq <- seq(0, 1, by = 0.01)  # Lambda value of 0.3 (you can add more values)
param_grid <- expand.grid(alpha = alpha_seq, lambda = lambda_seq)
param_grid = param_grid[c(lower:upper),]
train_begin=floor(0.7*nrow(x_m))
test_size=nrow(x_m)-(floor(0.7*nrow(x_m))+1)

RMSE=c()
parameters_tot=matrix(NA,ncol=length(param_grid$alpha),nrow=ncol(x_m))
for (j in 1:nrow(param_grid)) {
  #Initialize alpha and lambda values
  alpha <- param_grid$alpha[j]
  lambda <- param_grid$lambda[j]
  print(c(alpha,lambda))
  
  #Create new vector and matrices that are only used in the i for loop.
  y_pred <- c()
  parameters_estimated <- matrix(NA,ncol=test_size+1,nrow=ncol(x_m))
  
  for (i in 1:(test_size+1)) {
    
    #One step ahead training length
    train_length=train_begin+i-1
    
    #Train model with combinations of alpha,lambdas, and store the one step ahead prediction.
    reg=glmnet(x_m[1:train_length,],y_m[1:train_length],alpha = alpha,lambda = lambda)
    y_pred[i] = predict(reg,x_m[train_length+1,])
    parameters_estimated[,i]=as.matrix(reg$beta)[,1]
    print(c(i,train_length,train_length+1))
  }
  #Calculate the RMSE and store the mean values of the estimated parameters 
  RMSE[j]=sqrt((sum((y_pred-y_m[(train_begin+1):length(y_m)]))^2)*(1/length(y_pred)))
  parameters_tot[,j]=rowMeans(parameters_estimated)
  print(RMSE[j])
}


write_xlsx(data.frame(RMSE), name_rmse)







