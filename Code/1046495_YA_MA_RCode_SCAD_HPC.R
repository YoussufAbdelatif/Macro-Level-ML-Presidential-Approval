library(readxl)
library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(ncvreg)

set.seed(42)

missing = c(100 , 101 , 199 , 298 , 397 , 496 , 595 , 596 , 694 , 793 , 794 , 892 , 991 ,1090 ,1189 ,1288 ,1387 ,1486 ,1585)
index <- Sys.getenv(c("SLURM_ARRAY_TASK_ID")) 
lower = seq(401,793980,by=401)-400
upper = seq(401,793980,by=401)
boundaries = data.frame(cbind(lower,upper))


for ( i in 1:length(missing)) {
  index = missing[i]
  lower = boundaries[index,1]
  upper = boundaries[index,2]
  print(paste0(lower,":",upper))
}


name_rmse = paste0("rmse_SCAD_",index,".xlsx")


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



# Define alpha, lambda, and gamma sequences
alpha_seq <- seq(0.01, 0.99, by = 0.01)
lambda_seq <- seq(0, 4, by = 0.01)
gamma_seq <- seq(2.1, 4, by = 0.1)

# Create parameter grid
param_grid <- expand.grid(alpha = alpha_seq, lambda = lambda_seq, gamma = gamma_seq)
param_grid = param_grid[c(lower:upper),]

param_grid =param_grid[c(1:401,674884:675284,714583:714983,754282:754682,754683:755083),]
# Set up data splitting
train_begin <- floor(0.7 * nrow(x_m))
test_size <- nrow(x_m) - (floor(0.7 * nrow(x_m)) + 1)

RMSE <- vector()
parameters_tot <- matrix(NA, ncol = length(param_grid$alpha), nrow = ncol(x_m))

for (j in 1:nrow(param_grid)) {
  # Extract alpha, lambda, and gamma values
  alpha <- param_grid$alpha[j]
  lambda <- param_grid$lambda[j]
  gamma <- param_grid$gamma[j]
  
  # Initialize storage vectors/matrices
  y_pred <- vector()
  parameters_estimated <- matrix(NA, ncol = test_size + 1, nrow = ncol(x_m))
  
  for (i in 1:(test_size + 1)) {
    # One step ahead training length
    train_length <- train_begin + i - 1
    
    # Train model with combinations of alpha, lambdas, gammas using ncvfit
    reg <- ncvfit(x_m[1:train_length, ], y_m[1:train_length],
                  penalty = "SCAD", gamma = gamma, alpha = alpha, lambda = lambda)
    
    # Make predictions using matrix multiplication
    y_pred[i] <- as.vector(reg$beta) %*% as.vector(x_m[train_length + 1, ])
    parameters_estimated[, i] <- as.vector(reg$beta)
  }
  
  # Calculate RMSE and store mean values of estimated parameters
  RMSE[j] <- sqrt((sum((y_pred - y_m[(train_begin + 1):length(y_m)]))^2)*(1/length(y_pred)))
  parameters_tot[, j] <- rowMeans(parameters_estimated)
  print(c(alpha,lambda,gamma,RMSE[j]))
}


write_xlsx(data.frame(RMSE), name_rmse)




###################WDH##########################################################


#RMSE_length = c()
#RMSE_SCAD_wdh <- c()
#for (i in 1:401) {
#  filename <- paste0("rmse_SCAD_Wdh_", i, ".xlsx")
#  path <- file.path("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniVWL/Projekt/SCAD Results/subresults/", filename)  
#  rmse_df <- data.frame(read_xlsx(path))
#  RMSE_length[i] = length(rmse_df$RMSE)
#  RMSE_SCAD_wdh <- c(RMSE_SCAD_wdh, rmse_df$RMSE)
#}
#
#
#


#lower = seq(401,2005,by=401)-400
#upper = seq(401,2005,by=401)
#boundaries = data.frame(cbind(lower,upper))
#lower = boundaries[index,1]
#upper = boundaries[index,2]



#missing = c(1,1684,1783,1882,1883)

#for (i in 1:nrow(boundaries)) {
#  rmse_SCAD = RMSE_SCAD_wdh[boundaries[i,1]:boundaries[i,2]]
#  print(length(rmse_SCAD))
#  name = paste0("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniVWL/Projekt/SCAD Results/subresults/rmse_SCAD_",missing[i],".xlsx")
#  write_xlsx(data.frame(rmse_SCAD), name)
#}

#RMSE_length = c()
#RMSE_SCAD_wdh1 <- c()
#for (i in 1:401) {
#  filename <- paste0("rmse_SCAD_Wdh1_", i, ".xlsx")
#  path <- file.path("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniVWL/Projekt/SCAD Results/subresults/", filename)  
#  rmse_df <- data.frame(read_xlsx(path))
#  RMSE_length[i] = length(rmse_df$RMSE)
#  RMSE_SCAD_wdh1 <- c(RMSE_SCAD_wdh1, rmse_df$RMSE)
#}




#lower = seq(401,7619,by=401)-400
#upper = seq(401,7619,by=401)
#boundaries = data.frame(cbind(lower,upper))
#lower = boundaries[index,1]
#upper = boundaries[index,2]



#missing = c(100 , 101 , 199 , 298 , 397 , 496 , 595 , 596 , 694 , 793 , 794 , 892 , 991 ,1090 ,1189 ,1288 ,1387 ,1486 ,1585)

#for (i in 1:nrow(boundaries)) {
#  rmse_SCAD = RMSE_SCAD_wdh1[boundaries[i,1]:boundaries[i,2]]
#  print(length(rmse_SCAD))
#  name = paste0("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniVWL/Projekt/SCAD Results/subresults/rmse_SCAD_",missing[i],".xlsx")
#  write_xlsx(data.frame(rmse_SCAD), name)
#}


write_xlsx(data.frame(parameters_optimal_df_x), "parameters_optimal_df_x.xlsx")
write_xlsx(data.frame(parameters_optimal_df_y), "parameters_optimal_df_y.xlsx")
