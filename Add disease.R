#-------------------------------------------------------------------------#
#                                                                         #
#                           Fn epidemiologic                              #
#                                                                         #
#-------------------------------------------------------------------------#

# V1 26 Jan 2021

#-------------------------------------------------------------------------#
# OBSERVACIONES:

# 1. Only work with 
#   1-Difteria, 
#   2-Influenza, 
#   3-Paratiditis, 
#   4-Rubeola, 
#   5-Sarampion, 
#   6-Tetanos, 
#   7-Tos ferina, 
#   8-Tuberculosis resp
# 
#   in total 8.


## add Meningitis tuberculosa

# Name of tuberculosa:
# 2019-2017 Meningitis tuberculosa
# 2016-2004, 2002 Tuberculosis men??ngea
# 2003 Tuberculosis men??nge (ie. mal escrita)


# Tuberculosis men??ngea


 
# 2016 Meningitis 
  # Meningitis meningoc??cica 
  # Meningitis por Haemophilus Influenzae 
  # Meningoencefalitis amebiana primaria




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
(keyword <- sub_keywords[3])

anhos <- 2003:2019 

keyword <- "Poliomielitis aguda           "

GEN_m_enfermedad(keyword,anhos,l_PDF)

# Searching the 8 diseases ------------------------------------------------
names(l_PDF)

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")


# keywords = sort(keywords)


subset_disease <- c(28,75,91,104,110,111,120,124,130)

# Adding polio
subset_disease <- c(28,75,91,104,110,111,120,124,130,107)
(sub_keywords <- keywords[subset_disease])






# Save added desease ------------------------------------------------------

keywords_options_Influenza 
save(m_enfermedad,anhos,keyword,keywords,sub_keywords,l_PDF,keywords_options_Influenza,file="Variables_Vaccine.RData")
load(file="Variables_Vaccine.RData")


# Correcting the data -----------------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[3])

anhos <- 2003:2019 

keyword <- "Tuberculosis men??nge         "


m_enfermedad_name_2003 <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad_name_2003

keyword <- "Tuberculosis men??ngea         "


m_enfermedad_name_MT <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad_name_MT

keyword <- "Meningitis tuberculosa        "


m_enfermedad_name_TM <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad_name_TM


rbind(m_enfermedad_name_2003[1,],m_enfermedad_name_MT[2:14,], m_enfermedad_name_TM[15:17,])


# Creating a function to add a new disease with special problems t --------


ADD_GEN_m_enfermedad_Only_Meningitis(anhos,l_PDF) 


# To add a new disease to the app server ----------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
print(m_enfermedad)

# satohu ------------------------------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
load(file="Variables_Vaccine.RData")
source("Fn_epidemiologic.R")

m_enfermedad <- -2

# set.seed(42)
# (three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[3])
i_Influenza_vec <- 1:4

# GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)


ADD_GEN_m_enfermedad_Only_Meningitis(anhos,l_PDF)



# Correcting rows of a disease --------------------------------------------


(x <- GEN_m_enfermedad(keyword,anhos,l_PDF))
# 2005
x[3,-1] <- 0

# 2006
x[4,-1] <- 0
x[4,c(2,4)] <- 1
# 2007
x[5,-1] <- 0
# 2007
x[6,-1] <- 0

x




# satoeh ------------------------------------------------------------------


