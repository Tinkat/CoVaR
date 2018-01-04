library(quantreg)

###############################################
#setwd("C:/Users/Documents/2_Program/R-Code/Quantile_Lasso")
source("C:/Users/Documents/02_Program/R-Code/Quantile_Lasso/qunatile_lasso.r")

###################### Data Source  ##################################
setwd("C:/Users/Documents/02_Program/data")
        MarketData_FULL = read.table("MarketData_CDS-G14.txt")
        CDS_FULL = read.table("CDS_G14.txt")

        MarketData_PRE = read.table("MarketData_CDS-G14_PreCrisis.txt")
        CDS_PRE = read.table("CDS_G14_PreCrisis.txt")

        MarketData_CRISIS = read.table("MarketData_CDS-G14_Crisis.txt")
        CDS_CRISIS = read.table("CDS_G14_Crisis.txt")

        MarketData_POST = read.table("MarketData_CDS-G14_PostCrisis.txt")
        CDS_POST = read.table("CDS_G14_PostCrisis.txt")

        MarketData_ALL = read.table("Market_CDS_Return_Vola_Data.txt")
        CDS_ALL = read.table("CDS_ALL.txt")

####### Data ###############
###################################################
        #CDS 2 --> Citi
        #CDS 3 --> BoA
        #CDS 4 --> Barclay
        #CDS 5 --> BNP
        #CDS 6 --> CreditSuise
        #CDS 7 --> DB
        #CDS 8 --> GS
        #CDS 9 --> HSBC
        #CDS 10 --> JPM
        #CDS 11 --> MS
        #CDS 12 --> RBS
        #CDS 13 --> SocGen
        #CDS 14 --> UBS
        ###################################################
        #market data
        #VAR1 = MarketData[,2]         # Market Data VIX
        #VAR1 = MarketData[,3]         # Market Data Difference 3month Repo and 3month TreasutyBill
        #VAR1 = MarketData[,4]         # Market Data changes in 3month TreasutyBill
        #VAR1 = MarketData[,5]         # Market Data changes in the slope of the yield curve
        #VAR1 = MarketData[,6]         # Market Data changes in credit spread bw BAA-rated bonds and Treasury rate (same maturity 10yr)
        #VAR1 = MarketData[,7]         # Market Data weekly equity market return from CRSP
        #VAR1 = MarketData[,8]         # Market Data weekly real estate sector return in excess of market return
        #9-21: CDS Spread log returns
        #22-34: Stock log returns
        #35-47: Vola log return
        ###################################################

 #============= Variables to ADJUST =======================
        #Choice of y variable == choice of FI
        #n_y = 3
        #=============Static variables==========================
        #number of total x-variables
        N_X = 45

        #=======================================================
        #choice of length of data
        start_value = 1
        end_value = 250
        total_length =nrow(MarketData_ALL)
        days_rest =   total_length - end_value
        q=0.01



#======== Variables ========================
n_y_start = 1
count = 0
var_est <- matrix(nrow = days_rest, ncol = 13)
lambda_array<- matrix(nrow = days_rest, ncol = 13)
beta <-   matrix(nrow = days_rest, ncol = N_X)
beta_i <- array(0,N_X)
hit_sum <- array(0,13)
var_backend <- array(0,days_rest)
var_diff <- array(0,days_rest)
hit_seq <- array(0,days_rest)

file_path <- c("C:/Users/")

#======== main calculation ========================
for(j in 1:13){
        n_y= n_y_start + j
for(i in 1: days_rest){

        VAR_Y <- CDS_ALL[(start_value+i-1):(end_value+i-1),n_y]
        # 2-8: Market Value,
        # 9-21: CDS Spreads,
        # 22-34: equity log return,
        # 35-47: vola log return,
        VARMARKET_X1 <- MarketData_ALL[(start_value+i-1):(end_value+i-1),2:(6+n_y)]
        VARMARKET_X2 <- MarketData_ALL[(start_value+i-1):(end_value+i-1),(8+n_y):47]
        VARMARKET_X <- cbind(VARMARKET_X1, VARMARKET_X2)


#====== Separierungscode =====================
#VARMARKET_X <- MarketData_ALL[(start_value+i-1):(end_value+i-1),2:8]
#VARMARKET_X2 <- MarketData_ALL[(start_value+i-1):(end_value+i-1),23:34]


x <- as.matrix(VARMARKET_X)
y <- as.matrix(VAR_Y)

#n <- ncol(x)

### Cross Validation from Yuan (2006): GACV for quantile smoothing splines###

lambda <- optimize(GACV.qr,x=x,y=y,p=q,interval=c(0,50),maximum = FALSE,tol = 0.01)

lambda # Take a look of the results

lam <- lambda$minimum # The resulting value of lambda
lambda_array[i,j] <-  lam
fit <- rq.fit.lasso(x,y,tau=q,lambda=lam) # Use the resulting lambda to fit the model

attributes(fit) # see what commands you can use

beta_i = as.array(fit$coefficients)
for (m in 1:N_X){
  if (abs(beta_i[m]) < 10e-6){
    beta_i[m] = 0
  }
}
beta[i,] <- beta_i
var_est[i,j] <- x[250,] %*% beta[i,]

#=========== Calculation of hit sequence ========================
        var_backend[i] <- CDS_ALL[end_value+i,n_y]
        var_diff[i] = abs(var_backend[i]) - abs(var_est[i,j])

        if(var_diff[i]>0 ){hit_seq[i] = 1}
        else {hit_seq[i] = 0}

        #var_gesamt <- t(rbind(as.numeric(var), var_backend, var_diff, hit_seq))
}
        hit_sum[j] = sum(hit_seq)
#if (n_y == 2){
#  file_name <-  paste (file_path, "02_Citi_", sep="")
#} else if (n_y == 3){
#  file_name <-  paste (file_path, "03_BoA_", sep="")
#} else if (n_y == 4){
#  file_name <-  paste (file_path, "04_Barclays_", sep="")
#} else if (n_y == 5){
#  file_name <-  paste (file_path, "05_BNP_", sep="")
#} else if (n_y == 6){
#  file_name <-  paste (file_path, "06_CreditSuisse_", sep="")
#} else if (n_y == 7){
#  file_name <-  paste (file_path, "07_DB_", sep="")
#} else if (n_y == 8){
#  file_name <-  paste (file_path, "08_GS_", sep="")
#} else if (n_y == 9){
#  file_name <-  paste (file_path, "09_HSBC_", sep="")
#} else if (n_y == 10){
#  file_name <-  paste (file_path, "10_JPM_", sep="")
#} else if (n_y == 11){
#  file_name <-  paste (file_path, "11_MS_", sep="")
#} else if (n_y == 12){
#  file_name <-  paste (file_path, "12_RBS_", sep="")
#} else if (n_y == 13){
#  file_name <-  paste (file_path, "13_SocGen_", sep="")
#} else if (n_y == 14){
#  file_name <-  paste (file_path, "14_UBS_", sep="")
#}
#file_name_lasso <- paste( file_name,"lasso-qr_para45.csv",sep="")
#
#regress_para <- beta
#write.csv2(regress_para, file = file_name_lasso)
#
#write.csv2(var_gesamt, file = "C:/Users/Hien Pham Thu/Documents/Doktorarbeit/02_Program/Results/Quantile_Regression_VaR/02_Citi_VaR_Vola+Equity26.csv")
#
}
