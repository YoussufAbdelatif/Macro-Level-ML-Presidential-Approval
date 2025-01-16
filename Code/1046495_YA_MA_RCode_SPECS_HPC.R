set.seed(42)  # For reproducibility
run = 2


library(specs)
library(readxl)
library(Matrix)
library(glmnet)
library(dplyr,warn.conflicts = FALSE)
library(tidyr,warn.conflicts = FALSE)
library(ggplot2)
library(writexl)


manipulated_x_2 <- as.data.frame(read_excel("manipulated_x_2.xlsx"))
data_principal_components <- as.data.frame(read_excel("data_principal_components.xlsx"))
approval_q <- as.data.frame(read_excel("approval_q.xlsx"))


y <- ts(approval_q$Approval[13:nrow(approval_q)])
x <- ts(as.data.frame(data_principal_components))
# Combine y and x into a single data frame or matrix
data <- cbind(y, x)

# Determine the number of rows for the training set (70%)
train_rows <- round(0.7 * nrow(data))

# Split into training and testing sets
train_data <- data[1:train_rows, ]
test_data <- data[(train_rows + 1):nrow(data), ]

# Separate y and x in training and testing sets
train_y <- train_data[, 1]  # Assuming y is the first column
train_x <- train_data[, -1] # Assuming x is from the second column onwards

test_y <- test_data[, 1]
test_x <- test_data[, -1]
test_data_os_diff=diff(test_y)

x=ts(data[,-1])
y=ts(data[,1])

index <- Sys.getenv(c("SLURM_ARRAY_TASK_ID")) 


# Create a grid around lambda_g and lambda_i
lambda_g_grid <-  seq(0, 4, by = 0.1)
lambda_i_grid <-  seq(0, 4, by = 0.1)  # Change 0.1 to suit your precision
lag_grid = c(0,1,2,3,4)
det_grid = c("none","constant","both")

param_grid <- expand.grid(alpha = lambda_g_grid , lambda = lambda_i_grid, det = det_grid,lag=lag_grid)



lower = seq(1681,25215,by=1681)-1680
upper = seq(1681,25215,by=1681)
boundaries = data.frame(cbind(lower,upper))

lower_run = boundaries[run,1]
upper_run= boundaries[run,2]

param_grid = param_grid[c(lower_run:upper_run),]

lambda_g_grid <-  param_grid[index,1]
lambda_i_grid <- param_grid[index,2]
lag_grid = param_grid[index,4]
det_grid = param_grid[index,3]

param_grid = param_grid[index,]

name_parameter = paste0("parameter_SPECS_1",rownames(param_grid)[index],".xlsx")
name_rmse = paste0("rmse_SPECS_1",rownames(param_grid)[index],".xlsx")

lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }

pad  <- function(x,k) {
  x=lagpad(x,k=k)
  c(diff(x),NA)
} 
d_x  = function(k){
  apply(data[,-1],MARGIN = 2,FUN = pad,k=k)
}
d_y  = function(k){
  pad(data[,1],k=k)
}

iteration_count <- 0  # Initialize the iteration count
RMSE_total=c()
length_m = length(lambda_g_grid)*length(lambda_i_grid)*length(lag_grid)*length(det_grid)
combination=matrix(NA,ncol=4,nrow=length_m)
estimates=matrix(NA,ncol=length_m,nrow=(ncol(test_x)*(length(lag_grid)+1)+length(lag_grid)))
# Loop over all combinations
for (j in 1:nrow(param_grid)) {
  lambda_g <-  param_grid$alpha[j]
  lambda_i <- param_grid$lambda[j]
  lag = param_grid$lag[j]
  det = as.character(param_grid$det[j])
        iteration_count <- iteration_count + 1  # Increment the iteration count
        # Do something with the combination
        combination[iteration_count,1]=lambda_g
        combination[iteration_count,2]=lambda_i
        combination[iteration_count,3]=lag
        combination[iteration_count,4]=det
        RMSE=c()
        gamma_estimates=matrix(NA,nrow=(ncol(test_x)*(lag+2)+(lag+1)),ncol=length(test_y))
        for (i in 1:(length(test_y)-1)) {
          train_data_os=data[1:((length(train_y)+i)-1),]
          test_data_os=data[(length(train_y)+i),]
          test_data_os_lead=diff(lead(data))[(length(train_y)+i),]
          train_data_y <- train_data_os[, 1] 
          train_data_x <- train_data_os[, -1]
          test_data_y <- test_data_os[1]
          test_data_x <- test_data_os[-1]
          fit = specs(train_data_y,train_data_x,p=lag,deterministics = det,ADL = FALSE,weights = "none",
                      lambda_g = lambda_g,lambda_i = lambda_i)
          if (lag==0) {dat_new=(c((test_data_y),test_data_x,(test_data_os_lead)[-1]))}
          else if(lag==1){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),]))}
          else if(lag==2){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),]))}
          else if(lag==3){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),],d_y(1)[(length(train_y)+i)][1],d_x(2)[(length(train_y)+i),]))}
          else if(lag==4){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),],d_y(1)[(length(train_y)+i)][1],d_x(2)[(length(train_y)+i),],d_y(2)[(length(train_y)+i)][1],d_x(3)[(length(train_y)+i),]))}
          else if(lag==5){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),],d_y(1)[(length(train_y)+i)][1],d_x(2)[(length(train_y)+i),],d_y(2)[(length(train_y)+i)][1],d_x(3)[(length(train_y)+i),],d_y(3)[(length(train_y)+i)][1],d_x(4)[(length(train_y)+i),]))}
          else if(lag==6){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),],d_y(1)[(length(train_y)+i)][1],d_x(2)[(length(train_y)+i),],d_y(2)[(length(train_y)+i)][1],d_x(3)[(length(train_y)+i),],d_y(3)[(length(train_y)+i)][1],d_x(4)[(length(train_y)+i),],d_y(4)[(length(train_y)+i)][1],d_x(5)[(length(train_y)+i),]))}
          else if(lag==7){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),],d_y(1)[(length(train_y)+i)][1],d_x(2)[(length(train_y)+i),],d_y(2)[(length(train_y)+i)][1],d_x(3)[(length(train_y)+i),],d_y(3)[(length(train_y)+i)][1],d_x(4)[(length(train_y)+i),],d_y(4)[(length(train_y)+i)][1],d_x(5)[(length(train_y)+i),],d_y(5)[(length(train_y)+i)][1],d_x(6)[(length(train_y)+i),]))}
          else if(lag==8){dat_new=(c((test_data_y),(test_data_x),(test_data_os_lead)[-1],(test_data_os_lead)[1],d_x(0)[(length(train_y)+i),],d_y(0)[(length(train_y)+i)][1],d_x(1)[(length(train_y)+i),],d_y(1)[(length(train_y)+i)][1],d_x(2)[(length(train_y)+i),],d_y(2)[(length(train_y)+i)][1],d_x(3)[(length(train_y)+i),],d_y(3)[(length(train_y)+i)][1],d_x(4)[(length(train_y)+i),],d_y(4)[(length(train_y)+i)][1],d_x(5)[(length(train_y)+i),],d_y(5)[(length(train_y)+i)][1],d_x(6)[(length(train_y)+i),],d_y(6)[(length(train_y)+i)][1],d_x(7)[(length(train_y)+i),]))}
          fit$gammas[is.nan(fit$gammas)]=0
          y_hat = dat_new%*%fit$gammas
          result=as.matrix(cbind(test_data_os_diff[1],y_hat))
          result=na.omit(result)
          RMSE[i]=sqrt((1/length(result[,1]))*sum((result[,1]-result[,2])^2))
          print(paste("For the given set, one-step iteration:",i, "of",(length(test_y)),"iterations with RMSE of",RMSE[i]))
          gamma_estimates[,i] = fit$gammas
        }
        
        gamma_estimates=t(na.omit(t(data)))
        gamma_estimates=t(gamma_estimates)
        gamma_est = rowMeans(gamma_estimates)
        length(gamma_est)=nrow(estimates)
        estimates[,iteration_count]=gamma_est
        RMSE=na.omit(RMSE)
        RMSE_total[iteration_count]=mean(RMSE)
        print(paste("Iteration:", iteration_count, "| lambda_g:", lambda_g, "lambda_i:", lambda_i, "lag:", lag,"Det:",det,"RMSE:",RMSE_total
                    
                    
                    [iteration_count]))
      }


write_xlsx(data.frame(RMSE_total), name_rmse)
write_xlsx(data.frame(gamma_est), name_parameter)