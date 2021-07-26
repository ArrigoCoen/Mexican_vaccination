#-------------------------------------------------------------------------#
#                                                                         #
#                         EXTRACCION DE INFLUENZA                         #
#                                                                         #
#-------------------------------------------------------------------------#

# V1 FECHA

#-------------------------------------------------------------------------#
# OBSERVACIONES:

# 1. Bla bla bla
# 2. Bla bla bla
# 3. Bla bla bla
#
#-------------------------------------------------------------------------#

# INFLUENZA
c("En 2002 a 2008 se tiene 1 registro:
  1) Influenza",
  "En 2009 y 2011 se tienen 4 registros: 
  1) Influenza, 
  2) Influenza A(H1N1), 2009 identificado 
  3) Influenza debida a virus de la influenza identificado, 
  4) Influenza debida a virus no identificado",
  "En 2012 y 2013 se tienen 3 registros: 
  1) Influenza A(H1N1), 2009 identificado 
  2) Influenza debida a virus de la influenza identificado,
  3) Influenza debida a virus no identificado",
  "En 2014 a 2019 se tiene 1 registro:
  1) Influenza")


# Packages ----------------------------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))

three_years <- c(2006,2010,2019)
(keyword <- sub_keywords[2])

m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad
i <- 1
year <- three_years[i]
(datos_anho <- m_enfermedad[m_enfermedad[,1]==year,4:(ncol(m_enfermedad)-1)])


vec_partition_bars <- 1:11
vec_partition_bars <- c(1:3, 7:8,10:11)
vec_partition_bars <- c(1:8,10:11)

length(datos_anho)


my_barplot_age_blocks(datos_anho,year,vec_partition_bars) 

# ahsoethu ----------------------------------------------------------------

load(file="Variables_Vaccine.RData")

# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[3])

m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad


vec_partition_bars <- c(1:8,10:11)
vec_partition_bars <- 1:11
vec_partition_bars <- c(1:3, 7:8,10:11)

plot_3years_barplot(three_years,m_enfermedad,vec_partition_bars) 
  
  
  
# With histogram ----------------------------------------------------------

rep_data <- rep(.5,datos_anho[1])
vec_mean_age <- c(.5,2,6,11,16,21,26,46,51,61,66)
for(j in 2:length(datos_anho)) {
  rep_data <- c(rep_data,rep(vec_mean_age[j],datos_anho[j]))
}
my_breaks <- c(0,1,4,9,14,19,24,44,49,59,64,67)
datos_anho
hist(rep_data)
hist(rep_data,breaks = my_breaks)

barplot(rep_data )


# satheu ------------------------------------------------------------------



# barplot(datos_anho, main=year)


# plot_3years_barplot(three_years,m_enfermedad)


# snathu ------------------------------------------------------------------


m_enfermedad_only_GROUPS <- m_enfermedad[,c(-1:-3,-15)]
colnames(m_enfermedad_only_GROUPS)


vec_partition_bars



# Adding Influenza stuff to GEN_m_enfermedad ------------------------------




cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

m_enfermedad <- -1

# set.seed(42)
# (three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[2])
i_Influenza_vec <- 1:2

GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)


# oaesntuh ----------------------------------------------------------------
cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[2])


colnames_m_enfermedad <- c("Año","Total","Tasa","<1","1-4","5-9","10 - 14","15 - 19","20 - 24","25 - 44","45 - 49","50 - 59","60 - 64","65 y +","Ign.")
length(colnames_m_enfermedad)

m_enfermedad <- matrix(NA,length(anhos),length(colnames_m_enfermedad))
colnames(m_enfermedad) <- colnames_m_enfermedad

# Extra variable for testing
print_line_year = T
i <- 1
for(i in 1:nrow(m_enfermedad)) {
  i
  anho <- anhos[i]
  text_one_line <- extract_one_line(anho,keyword,l_PDF)
  
  if(print_line_year) {
    cat("Year", anho,"\n")
    cat(text_one_line,"\n")
    cat(length(text_one_line),"\n") # 16 = l_PDF$ncol_m_enfermedad + 1 
  }
  m_enfermedad[i,1] <- anho
  m_enfermedad[i,-1] <- text_one_line[-1:-2]
}


# osauotnh ----------------------------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")



# aouanteoh ---------------------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# anhos_elegidos <- 2005:2019
anhos <- 2005:2019


# i_Influenza <- 4


# GEN_m_enfermedad_INFLUENZA(i_Influenza,anhos,l_PDF)




i_Influenza_vec <- 1:4
i_Influenza_vec <- 1
(m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))
i_Influenza_vec <- 2
(m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))

i_Influenza_vec <- c(1,2)
(m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))




# asehu -------------------------------------------------------------------



imprime_primeras_letras_cada_renglon(2010,l_PDF,150)


# esantoeuh ---------------------------------------------------------------
anho <- 2009

keywords_Influenza <- c("Influenza                                         ",
                        "Influenza A(H1N1), 2009 identificado              ",
                        "Influenza debida a virus de la influenza",
                        "                                                  ",
                        "identificado",
                        "Influenza debida a virus no identificado          ")

keyword <- keywords_Influenza[6]

text_one_line <- extract_one_line(anho,keyword,l_PDF)
text_one_line


# oaesntuh ----------------------------------------------------------------


names(l_PDF)

l_PDF

# Extraccion entre lineas -------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")
anho <- 2009
anho <- 2010
anho <- 2011
anho <- 2005

keyword_1 <- "Influenza debida a virus de la influenza"
keyword_2 <- "identificado"
(text_one_line <- extract_between_lines(anho,keyword_1,keyword_2,l_PDF))
length(text_one_line)


# satuh -------------------------------------------------------------------



text_vec <- PDF[idx_first_word]

# santuh ------------------------------------------------------------------


if(length(text_vec)==0) text_one_line <- rep(NA,l_PDF$ncol_m_enfermedad + 1) else {
  text_one_line <- correccion_espacios_en_lineas(text_vec)
}
#   return(text_one_line)
# }

# oaesntuh ----------------------------------------------------------------

# Ejemplo de extraccion influenza -----------------------------------------

# 93   Infecciones respiratorias agudas                                                                    28 366 695 26 169.45 2 629 152 6 202 461 4 082 598 
# 94                                                                          J02.0 y J03.0 
# 95   Influenza                                                                   J10-J11                      16 193           14.94            856         
# 96   Influenza A(H1N1), 2009 identificado                                          J09                          2 370            2.19             63        
# 97   Influenza debida a virus de la influenza 
# 98                                                                                 J10                          1 799            1.66           110         
# 99   identificado 
# 100   Influenza debida a virus no identificado                                      J11                        15 557           14.35          1 001         
# 101   Intoxicación aguda por alcohol º                                             F10.1                       40 044           37.60           N.A.         
# 102 






# oaesntuh ----------------------------------------------------------------







# oaesntuh ----------------------------------------------------------------







# oaesntuh ----------------------------------------------------------------




