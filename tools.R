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
