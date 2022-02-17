install.packages("readxl")
library("readxl")

install.packages('jrvFinance')
library('jrvFinance')

bonds <- read_excel("/Users/user/Desktop/assignment1.xlsx")
all_date <- c("2022-01-10","2022-01-11","2022-01-12","2022-01-13","2022-01-14","2022-01-17","2022-01-18","2022-01-19","2022-01-20","2022-01-21")
View(date)
matrix_data <- matrix(c(bonds$'10', bonds$'11',bonds$'12',bonds$'13',bonds$'14',bonds$'17',bonds$'18',bonds$'19',bonds$'20',bonds$'21'), ncol = 10,nrow = 10, byrow = TRUE)
maturity <- bonds$'maturity date'
View(maturity)
View(matrix_data)
View(bonds$'1')
#question 4
all_coupon <- as.numeric(bonds$'coupon')
View(bonds$'coupon')
View(all_coupon)
yield_matrix = matrix('numeric',nrow = 10, ncol = 10)
for(x in c(1:10)){
  price_data = matrix_data[,x]
  for(y in c(1:10)){
    yield_matrix[y,x] <- bond.yield(settle = all_date[y], mature = maturity[x], coupon = all_coupon[x], freq = 2, price_data[y], convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"), comp.freq = 2, redemption_value = 100)
  }
}
View(yield_matrix)

fraction_year = matrix('numeric', nrow = 10, ncol = 10)
for (x in c(1:10)) {
  for(y in c(1:10)){
    fraction_year[x,y] = yearFraction(all_date[x], maturity[y], freq = 2, convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"))
  }
}
View(fraction_year)

range_year <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(range_year, yield_matrix[1,],type = "o", main = '5-year yield curve', col = 'black', xlab = "5 Year", ylab = "yield", lwd= 1.0)
all_colors = c("blue","red","purple","chocolate","yellow","deepskyblue","gray","gold","palegreen","green")
for (i in c(2:10)) {
  lines(range_year, yield_matrix[i,],type = "o", col=all_colors[i-1],lwd=1.0)
}

#4.b
cash_flow <- list()
for (x in 1:10){
  cash_flow = bond.TCF(all_date[x], maturity[x],coupon = all_coupon[x], freq = 2, redemption_value = 100)$cf
  print(cash_flow)
}

dirty_price <- matrix('numeric', nrow = 10, ncol = 10)
for (x in 1:10){
  for (y in 1:10) {
    dirty_price[x,y] = bond.TCF(settle = all_date[x],mature = maturity[y],coupon = all_coupon[y],freq = 2,redemption_value = 100)$accrued + matrix_data[x,y]
  }
}
View(dirty_price)

matrix_spot <- matrix(ncol = 10, nrow = 10)
for (i in 1:10){
  time_1 = as.numeric(fraction_year[i,1])
  spot_function_1 = function(x) as.numeric(dirty_price[i,1]) - 0.75*(1+x/2)^(-2*(time_1-0.5*8)) - 100.75*(1+x/2)^(-2*time_1)
  spot_1 = uniroot(spot_function_1, c(0,1))$root
  
  time_2 = as.numeric(fraction_year[i,2])
  spot_function_2 = function(x) as.numeric(dirty_price[i,2]) - 0.5*(1+spot_1/2)^(-2*(time_2-0.5*10)) - 100.5*(1+x/2)^(-2*time_2)
  spot_2 = uniroot(spot_function_2, c(0,1))$root
  
  time_3 = as.numeric(fraction_year[i,3])
  spot_function_3 = function(x) as.numeric(dirty_price[i,3]) - 1*(1+spot_1/2)^(-2*(time_3-0.5*11)) - 1*(1+spot_2/2)^(-2*(time_3-0.5*10)) - 101*(1+x/2)^(-2*time_3)
  spot_3 = uniroot(spot_function_3, c(0,1))$root
  
  time_4 = as.numeric(fraction_year[i,4])
  spot_function_4 = function(x) as.numeric(dirty_price[i,4]) - 0.625*(1+spot_1/2)^(-2*(time_4-0.5*6)) - 0.625*(1+spot_2/2)^(-2*(time_4-0.5*5))- 0.625*(1+spot_3/2)^(-2*(time_4-0.5*4)) - 100.625*(1+x/2)^(-2*time_4)
  spot_4 = uniroot(spot_function_4, c(0,1))$root
  
  time_5 = as.numeric(fraction_year[i,5])
  spot_function_5 = function(x) as.numeric(dirty_price[i,5]) - 0.25*(1+spot_1/2)^(-2*(time_5-0.5*17)) - 0.25*(1+spot_2/2)^(-2*(time_5-0.5*16))- 0.25*(1+spot_3/2)^(-2*(time_5-0.5*15))- 0.25*(1+spot_4/2)^(-2*(time_5-0.5*14)) - 100.25*(1+x/2)^(-2*time_5)
  spot_5 = uniroot(spot_function_5, c(0,1))$root
  
  time_6 = as.numeric(fraction_year[i,6])
  spot_function_6 = function(x) as.numeric(dirty_price[i,6]) - 0.125*(1+spot_1/2)^(-2*(time_6-0.5*8)) - 0.125*(1+spot_2/2)^(-2*(time_6-0.5*7))- 0.125*(1+spot_3/2)^(-2*(time_6-0.5*6))- 0.125*(1+spot_4/2)^(-2*(time_6-0.5*5)) - 0.125*(1+spot_5/2)^(-2*(time_6-0.5*4)) - 100.125*(1+x/2)^(-2*time_6)
  spot_6 = uniroot(spot_function_6, c(0,1))$root
  
  time_7 = as.numeric(fraction_year[i,7])
  spot_function_7 = function(x) as.numeric(dirty_price[i,7]) - 0.625*(1+spot_1/2)^(-2*(time_7-0.5*10)) - 0.625*(1+spot_2/2)^(-2*(time_7-0.5*9))- 0.625*(1+spot_3/2)^(-2*(time_7-0.5*8))- 0.625*(1+spot_4/2)^(-2*(time_7-0.5*7))- 0.625*(1+spot_5/2)^(-2*(time_7-0.5*6))- 0.625*(1+spot_6/2)^(-2*(time_7-0.5*5)) - 100.625*(1+x/2)^(-2*time_7)
  spot_7 = uniroot(spot_function_7, c(0,1))$root
  
  time_8 = as.numeric(fraction_year[i,8])
  spot_function_8 = function(x) as.numeric(dirty_price[i,8]) - 0.625*(1+spot_1/2)^(-2*(time_8-0.5*10)) - 0.625*(1+spot_2/2)^(-2*(time_8-0.5*9))- 0.625*(1+spot_3/2)^(-2*(time_8-0.5*8))- 0.625*(1+spot_4/2)^(-2*(time_8-0.5*7))- 0.625*(1+spot_5/2)^(-2*(time_8-0.5*6))- 0.625*(1+spot_6/2)^(-2*(time_8-0.5*5)) - 0.625*(1+spot_7/2)^(-2*(time_8-0.5*4))- 100.625*(1+x/2)^(-2*time_8)
  spot_8 = uniroot(spot_function_8, c(0,1))$root
  
  time_9 = as.numeric(fraction_year[i,9])
  spot_function_9 = function(x) as.numeric(dirty_price[i,9]) - 0.5*(1+spot_1/2)^(-2*(time_9-0.5*10)) - 0.5*(1+spot_2/2)^(-2*(time_9-0.5*9))- 0.5*(1+spot_3/2)^(-2*(time_9-0.5*8))- 0.5*(1+spot_4/2)^(-2*(time_9-0.5*7))- 0.5*(1+spot_5/2)^(-2*(time_9-0.5*6))- 0.5*(1+spot_6/2)^(-2*(time_9-0.5*5)) - 0.5*(1+spot_7/2)^(-2*(time_9-0.5*4))- 0.5*(1+spot_8/2)^(-2*(time_9-0.5*3))- 100.5*(1+x/2)^(-2*time_9)
  spot_9 = uniroot(spot_function_9, c(0,1))$root
  
  time_10 = as.numeric(fraction_year[i,10])
  spot_function_10 = function(x) as.numeric(dirty_price[i,10]) - 1.125*(1+spot_1/2)^(-2*(time_10-0.5*6)) - 1.125*(1+spot_2/2)^(-2*(time_10-0.5*5))- 1.125*(1+spot_3/2)^(-2*(time_10-0.5*4)) - 1.125*(1+spot_4/2)^(-2*(time_10-0.5*3)) - 1.125*(1+spot_5/2)^(-2*(time_10-0.5*2))- 1.125*(1+spot_6/2)^(-2*(time_10-0.5*1))- 112.5*(1+x/2)^(-2*time_10)
  spot_10 = uniroot(spot_function_10, c(0,1))$root
  
  all_s = rbind(spot_1, spot_2, spot_3, spot_4, spot_5, spot_6, spot_7, spot_8, spot_9, spot_10)
  matrix_spot[i,] <- all_s
}
View(matrix_spot)
range_year <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(range_year, spot_matrix[1,],type = "o", main = '5-year spot curve', col = 'black', xlab = "5 Year", ylab = "spot rate", lwd= 1.0)
all_colors = c("blue","red","purple","chocolate","yellow","deepskyblue","gray","gold","palegreen","green")
for (i in c(2:10)) {
  lines(range_year, spot_matrix[i,],type = "o", col=all_colors[i-1],lwd=1.0)
}

#4c
forward_spot <- c()
forward_years <- c(yearFraction('2022-08-01','2022-02-01'),yearFraction('2023-08-01','2023-02-01'),yearFraction('2024-08-01','2024-02-01'),yearFraction('2025-08-01','2025-02-01'),yearFraction('2026-08-01','2026-02-01'))
small_forward <- c(yearFraction('2022-08-01','2022-01-01'),yearFraction('2023-08-01','2023-01-01'),yearFraction('2024-08-01','2024-01-01'),yearFraction('2025-08-01','2025-01-01'),yearFraction('2026-08-01','2026-01-01'))
for(y in c(1:10)){
  first_half_spot <- c(matrix_spot[y,][2],matrix_spot[y,][4],matrix_spot[y,][6],matrix_spot[y,][8],matrix_spot[y,][10])
  second_half_spot <- c(matrix_spot[y,][1],matrix_spot[y,][3],matrix_spot[y,][5],matrix_spot[y,][7],matrix_spot[y,][9])
    for(x in c(1:5)){
      forward_spot[x] = first_half_spot[x]+ (second_half_spot[x]-first_half_spot[x])*(small_forward[x]/forward_years[x])
  print(forward_spot)
    }
}

forward_1 <- c(0.0020631291, -0.0005533026,  0.0136623237,  0.0263103761,  0.0098601952)
forward_2 <- c(0.001950944, -0.000601257, 0.012472382,  0.025004331,  0.008985248)
forward_3 <- c(0.0020639023, -0.0007725573,  0.0123051870,  0.0248569097,  0.0088681099)
forward_4 <- c(0.0020786453, -0.0006261499,  0.0121320279,  0.0250027074 , 0.0089524696)
forward_5 <- c(0.0023259794, -0.0004559045,  0.0121320279,  0.0249707693,  0.0088561142)
forward_6 <- c(0.003014544, 0.000173991, 0.012398120, 0.025253259, 0.009108052)
forward_7 <- c(0.0032792125, 0.0007238717, 0.0131809409, 0.0260901337, 0.0099122180)
forward_8 <- c(0.0036043278, 0.0008190723, 0.0140081158, 0.0267086968, 0.0104109495)
forward_9 <- c(0.0035683852, 0.0007316722, 0.0140081158, 0.0268211729, 0.0105270319)
forward_10 <- c(0.003077570, 0.000102736, 0.013959144, 0.026770475, 0.010439581)

five_year_forward <- rbind(forward_1,forward_2,forward_3,forward_4,forward_5,forward_6,forward_7,forward_8,forward_9,forward_10)
View(five_year_forward)

matrix_forward = matrix( nrow =10, ncol = 4)
for(i in c(1:10)){
  for(j in c(1:4)){
    forward_function = function(x) ((1+five_year_forward[i,1]/2)^2)*((1+x/2)^(2*j)) - (1+five_year_forward[i,j+1]/2)^(2*(j+1))
    matrix_forward[i,j] <- uniroot(forward_function,c(-10,-1))$root
  }
}
View(matrix_forward)
range_year <- c(2,3,4,5)
plot(range_year, matrix_forward[1,],type = "o", main = 'forward curve', col = 'black', xlab = "Years", ylab = "forwar rate", lwd= 1.0)
all_colors = c("blue","red","purple","chocolate","yellow","deepskyblue","gray","gold","palegreen","green")
for (i in c(2:10)) {
  lines(range_year, matrix_forward[i,],type = "o", col=all_colors[i-1],lwd=1.0)
}
View()
#5
new_yield_matrix = matrix(ncol = 5, nrow = 10)
for (x in c(1:10)){
  new_yield_matrix[x,1] = five_year_forward[x,1]
  for (y in c(2,3,4,5)){
    new_yield_matrix[x,y] = as.numeric(yield_matrix[x,y*2])
  }
}
View(new_yield_matrix)
log_yield <- matrix(nrow = 9 , ncol = 5)
for (y in c(1:5)){
  for (x in c(1:9)){
    log_yield[x,y] = log(new_yield_matrix[(x+1),y]/new_yield_matrix[x,y])
  }
}
View(log_yield)

covariance_yield = cov(log_yield,log_yield)
View(covariance_yield)
eigenvalue_yield <-eigen(covariance_yield)$values
View(eigenvalue_yield)
eigenvetors_yield <-eigen(covariance_yield)$vectors
View(eigenvetors_yield)

#forward cov and eigen
log_forward = matrix(nrow = 9, ncol =4)
for(x in c(1:4)){
  for (y in c(1:9)){
    log_forward[y,x] = log(matrix_forward[(y+1),x]/matrix_forward[y,x])
  }
}
View(log_forward)

covariance_forward = cov(log_forward,log_forward)
View(covariance_forward)
eigenvalue_forward <-eigen(covariance_forward)$values
View(eigenvalue_forward)
eigenvetors_forward <-eigen(covariance_forward)$vectors
View(eigenvetors_forward)
