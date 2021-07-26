

# Porcentajes de casos




# Extraccion de datos -----------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo

source("Fn_epidemiologic.R")

load(file="Variables_Vaccine.RData")


(keyword <- sub_keywords[3])

i_Influenza_vec <- 1:4 

m_enfer_per = extrae_m_enfer_per(keyword,i_Influenza_vec,anhos,l_PDF)
m_enfer_per





# satoehu -----------------------------------------------------------------


