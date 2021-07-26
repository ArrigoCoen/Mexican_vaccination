#-------------------------------------------------------------------------#
#                                                                         #
#                           Fn epidemiologic                              #
#                                                                         #
#-------------------------------------------------------------------------#

# V1 26 Jan 2021

#-------------------------------------------------------------------------#
# OBSERVACIONES:

# 1. https://www.youtube.com/watch?v=1opjnegd_hA
# https://www.youtube.com/watch?v=NPrCEgKSy5U
# https://www.youtube.com/watch?v=LGfCurzDJPE 
# https://www.idrisstsafack.com/post/how-to-test-the-stationarity-of-a-time-series-with-r-software
# 
# 

# Names of deseases -------------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

keywords

# setwd("/Users/arrigocoen/Dropbox/1 Proyectos/2020/Asthma/R")
# Checking a disease ------------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))
sub_keywords
(keyword <- sub_keywords[5])

anhos <- 2003:2019 

i_Influenza_vec <- 1:4


x <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)[,3]
id_x <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)[,1]
plot(id_x,x,type="l")
adf.test(x)
keyword


# sathao ------------------------------------------------------------------
library("aTSA")
# ADF test for AR(1) process
x <- arima.sim(list(order = c(1,0,0),ar = 0.2),n = 100)
adf.test(x)
# ADF test for co2 data
adf.test(co2)
# sathao ------------------------------------------------------------------


B<-decompose(ts(x,frequency = 3))
birthstimeseriescomponents <- decompose(B)

# sathao ------------------------------------------------------------------



# sathao ------------------------------------------------------------------



# sathao ------------------------------------------------------------------



# sathao ------------------------------------------------------------------


