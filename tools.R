library(MASS)
library(ggplot2)
library(dplyr)

library(tidyverse)
## make budget constraint calc that:
# 2. reads in variables
#goodX, qtyX, goodY, qtyY
# 3. prints out every step
# 4. determine whether fractions are involved
# age3 <- strsplit(as.character(testthing), "/")
# age3[[1]][1] numerator
# age3[[1]][2] demoninator

#goodX <- strsplit(as.character(readline(prompt="Enter goodX price: ")), "/")

# split if needed then execute
#find Zeros 

budgetConstraint <- function(kind) {
  
  ##  if you only have prices and income
  if(kind == "pricesonly") {
    income <- as.double(readline(prompt="Enter income: "))
    
    #labels to avoid paste isses
    labelx <- readline(prompt="Enter the label of the X axis (example: X, Y, Z, etc): ")
    labely <- readline(prompt="Enter the label of the Y axis (example: X, Y, Z, etc): ")
    goodX <- as.double(readline(prompt="Enter the price of the good X: "))
    goodY <- as.double(readline(prompt="Enter the price of the good Y: "))
   
   
    
    # set up budget constraint by looking for good Y first.
    
    
    # set 1 of 2 income / priceY 
    # test vals : 50 / 5 , test value 10
    partialOne <- income / goodY
    #if its longer than 3 characters implying its a decimal
   
    # set 2 of 2 (making slope) PriceX / PriceY , test value 0.2
    partialTwo <- goodX / goodY
   
   
    #find zeros of Y
    zerosofY <- partialOne / partialTwo
  
  
    
    #print out main equation:
    maineq <- paste(income, " = (", goodX, " * ", labelx, ") + (", goodY, " * ", labely, ")")
    
    #print out set 1 & 2:
    set1and2 <- paste("-", labely," = -(", income, "/", goodY, ") + (", goodX, " / ", goodY, ")", labelx)
    
    # print out budget constraint
    budgetconst <- paste(labely, "=", partialOne, "-", fractions(partialTwo), labelx,"(Budget Constraint)")
    
    #print zeros of Y
    workfory <- paste(partialOne, "/", partialTwo, "=", zerosofY)
    workfory2 <- paste(zerosofY, "=", labelx)
    workfory3 <- paste("Zero", labely, "(Y-axis) = (", zerosofY, ", 0 )")
    workfory4 <- paste("Zero", labelx, "(X-axis) = ( 0, ", partialOne, ")")
    
    #big paste: 
   cat(paste(maineq, set1and2, budgetconst, workfory, workfory2, workfory3, workfory4, sep = "\n"))
   # Income = PriceX * QTYX + PriceY * QTYY
   #test numbers are 50 = (1 * qtyx) + (5 * qtyy)
   #graph it 
   demand <- Hmisc::bezier(c(0, partialOne),
                           c(zerosofY, 0)) %>%
     as_data_frame()
   
   ggplot(mapping = aes(x = x, y = y)) +
     xlab(labelx) + ylab(labely) +
   geom_path(data = demand, color = "#FF4036", size = 1.5) +
    theme_grey() +
   coord_equal()
  }
  
  
  
  
}

#find price descrimination
# r(q)  = r * q

# Given:
#  P = 3 - 0.02q

eqp1 <- 3

eqp2 <- -0.02

# r(q)= 3q - 0.02q^2

# First derivative is MR.
# MR = 3 - 0.04q


#bertrand equilibirum table
# params are price, quantity and costs respectively. returns console output.
bertrandeq <- function(p, q, c) {
  profit <- (p - c) * q 
  perfitperfirm <- profit/ 2
  paste("Price:", p, "Quantity:", q, "| Cost:", c, "| Total Market Profit:", profit, "| Split profit per firm:", perfitperfirm  )
}
#bertrand reaction calc
# takes in params from the inverse demand function.
# this equation: P = a - b * (q1 + q2)
# C = MC = costs associated.
# find the revenue function first, then this.
bertrandreaction <- function(a, b, c) {
  finala <- ( (a - c) / (2*b) )
  finalb <- ( (b) / (2*b) )
  paste(finala, "-", finalb, "q")
  
  }

#bertrand MRMC calc display MR equation and reaction function, all relative to Q1.
# takes in params from the inverse demand function.
# this equation: P = a - b * (q1 + q2)
# C = MC = costs associated.
# find the revenue function first, then this.
bertrandMrMc <- function(a, b, c) {
  finala <- ( (a - c) / (2*b) )
  finalb <- ( (b) / (2*b) )
  mr <- paste(a, "-", (2*b), "q1 -", b, "q2   | MR equation"  )
  last <- paste(finala, "-", finalb, "q2   |  Reaction function")
  cat(paste(mr , last, sep = "\n" ))
  
}
# q is whether q1 or q2, limited to 2 firms.
bertrandprofiteq <- function(a, b, c, q) {
  # if its q1 else q2
  if( q == 1) {
    qfinal <- "q1"
    qopp <- "q2"
  }
  else {
    qfinal <- "q2"
    qopp <- "q1"
  }
  
  finala <- a
  finalb <- (2*b )
  finalc <- b
  paste(finala, qfinal, "-", finalc, qfinal, "^2 -", finalc, qfinal, qopp)
  
}
# cobb douglas function that needs alpha, technology, capital and labor.

cobbdouglassimple <- function() {
  
  #labels to avoid paste issues. test vals are 1, .5, 100, 100
  technology <- as.double(readline(prompt="Enter the A value: "))
  alpha      <- as.double(readline(prompt="Enter the alpha value: "))
  capital    <- as.double(readline(prompt="Enter the capital amount: "))
  labor      <- as.double(readline(prompt="Enter the labor amount: "))
  
  
  #find MPL, MPK, share of output to the respective factors
  
  # set up internal variables that are used.
  alphaMinusOne  <-(alpha - 1)
  oneMinusAlpha <- (1 - alpha)
  negativeAlpha <- (alpha * -1)
  
  MPKvalue <- alpha * technology * capital^alphaMinusOne  * labor^oneMinusAlpha
  MPKprintval <- paste("MPK, aka rental capital =", alpha, "*", technology, "*", capital, "^", alphaMinusOne , "*", labor, "^", oneMinusAlpha, "=====>", MPKvalue)
  
  #set up MPL
  MPLvalue <- oneMinusAlpha * technology * capital^alpha * labor^negativeAlpha 
  MPLprintval <- paste("MPL, aka wages = ", oneMinusAlpha, "*", technology, "*", capital, "^", alpha , "*", labor, "^", negativeAlpha, "=====>", MPLvalue )
  
  # share of output to the respective factors is determined by output * respective marginal product
  # output * MPL = share of output for the labor sector
  laborSharevalue <- labor * MPLvalue
  laborShareprintval <- paste("Share of output respective to labor =", labor,"*", MPLvalue, "=======>", laborSharevalue)
  
  capitalSharevalue <- capital * MPKvalue
  capitalShareprintval <- paste("Share of output respective to capital = ", capital,"*", MPKvalue, "=======>", capitalSharevalue)
  
  
  #big paste: 
  cat(paste(MPKprintval, MPLprintval, laborShareprintval, capitalShareprintval, sep = "\n"))
  
}
# solves if you have all variables, eventually build out to handle multiple scenarios
# uses wages, Labor, Rental Rate of Capital, and K for capital.
costminimizing <- function() {
  #labels to avoid paste issues. test vals are 1, .5, 100, 100
  labor      <- as.double(readline(prompt="Enter (L) Labor amount: "))
  wages      <- as.double(readline(prompt="Enter (w) wages amount: "))
  capital    <- as.double(readline(prompt="Enter (K) capital amount: "))
  rentalcap  <- as.double(readline(prompt="Enter (v) rental rate of capital: "))
  qty        <- 
  
  #set up MPK
  MPKvalue <- alpha * technology * capital^alphaMinusOne  * labor^oneMinusAlpha
  MPKprintval <- paste("MPK, aka rental capital =", alpha, "*", technology, "*", capital, "^", alphaMinusOne , "*", labor, "^", oneMinusAlpha, "=====>", MPKvalue)
  
  #set up MPL
  MPLvalue <- oneMinusAlpha * technology * capital^alpha * labor^negativeAlpha 
  MPLprintval <- paste("MPL, aka wages = ", oneMinusAlpha, "*", technology, "*", capital, "^", alpha , "*", labor, "^", negativeAlpha, "=====>", MPLvalue )
  
}
 
hw2fonevariantc <- function(q) {
  ans <- ( (2 * q) / 500 ) + 100
  output <- "TC = ( (2 * q) / 500 ) + 100 , "
  atc = ans / q
  paste(output, "q = ", q, ", TC = ", ans, ", AC = (TC/q) =  ", atc)
}
 
hw2fonevariant <- function(q) {
  ans <- ( (3 * q) / 500 ) + 200
  output <- "TC = ( (3 * q) / 500 ) + 200 , "
  atc = ans / q
  paste(output, "q = ", q, ", TC = ", ans, ", AC = (TC/q) =  ", atc)
}

hw2fone <- function(q) {
  ans <- ( (3 * q) / 500 ) + 100
  output <- "TC = ( (3 * q) / 500 ) + 100 , "
  atc = ans / q
  paste(output, "q = ", q, ", TC = ", ans, ", AC = (TC/q) =  ", atc)
}

hw2ftwo <- function(q) {
  ans <-( 3 * q^2 ) / 2500  + 100
  output <- "TC = ( 3 * q^2 ) / 2500  + 100, "
  atc = ans / q
  paste(output, "q = ", q, ", TC = ", ans, ", AC = (TC/q) =  ", atc)
}

hw2ftwovariant <- function(q) {
  ans <-( 2 * q^2 ) / 2500  + 100
  output <- "TC = ( 2 * q^2 ) / 2500  + 100, "
  atc = ans / q
  paste(output, "q = ", q, ", TC = ", ans, ", AC = (TC/q) =  ", atc)
}

# hw2ftwo test val if q = 1
# 1/5 = .2
# .2 ^2 = .04
# .04 * 3 = .12
# .12 / 100 = 0.0012
# 0.0012 + 100 = 100.0012
# TC = 100.0012
#ATC = TC /q = 100.0012 / 1 = 100.0012
q <- 25

b <- c()
d <- c()
e <- c()
f <- c()
for (i in a) {
       b[i] <- hw2ftwo(i)    
}
for (i in a) {
 d[i] <- hw2fone(i)    
}

for (i in a) {
  e[i] <- hw2fonevariant(i)    
}

for (i in a) {
  f[i] <- hw2ftwovariant(i)    
}

for (i in a) {
  e[i] <- hw2fonevariantc(i)    
}
# set prices as vector
testvve <- c(30,25,20,15,10,5,4,0)
mcsr1 <- function(vforVector){
 
  price <- quantity <- atc <- c()
  i <- 1
  for (val in vforVector) {
    pp <- val
    q <- (pp - 4)/2
    
    if (q == 0) {
      atcq <-  4 
    } else {
      atcq <- q + 4 + (16/q)
    }
    cat(paste("p =", pp, "," , q, "= q", atcq, "= ATC \n")) 
    
    #push into loops
    price[i] <- pp
    quantity[i] <-q
    atc[i] <- atcq
    i <- i + 1
  }
  cat("n=", length(vforVector))
  pandq <- data.frame(
    price,
    quantity,
    atc
    
  )
  print(pandq)
  write.csv(pandq,"C:\\Users\\nicka\\Desktop\\printdirectory\\ps3-3a.csv", row.names = FALSE)
}

findatcv <- c(13,10.5,8,5.5,3,0.5,0)

findatc <- function(vforVector){
  
  #qs1
 
  
 
  
  
  
  
  price <- quantity <- c()
  i <- 1
  for (val in vforVector) {
    
    # set atc
    if (val == 0) {
      atc <-  4 
    } else {
      atc <- val + 4 + (16/val)
    }
    
    
   
    cat(paste("p =", pp, "," , q, "= q \n")) 
    
    #push into loops
    price[i] <- pp
    quantity[i] <-q
    i <- i + 1
  }
  cat("n=", length(vforVector))
  pandq <- data.frame(
    price,
    quantity
    
  )
  print(pandq)
  write.csv(pandq,"C:\\Users\\x\\Desktop\\x\\ps3-3a.csv", row.names = FALSE)
}

