#-------------------------------------------------------------------------#
#                                                                         #
#                           NOMBRE DE ARCHIVO                             #
#                                                                         #
#-------------------------------------------------------------------------#

# V1 FECHA

#-------------------------------------------------------------------------#
# OBSERVACIONES:

# 1. Los boletines fueron extraidos de:
# https://www.gob.mx/salud/acciones-y-programas/historico-boletin-epidemiologico

# 2. Parkinson se tiene desde 2014-2 hasta 2019-52 
# 3. El Boletin es semanal a partir de 1995 semana 26
# - 2017 tiene muchos archivos extra que fueron borrados
# - 2011 HAY QUE HACER LA CORRECIION DE LOS ARCHIVOS ZIP PARA ESTE ANHO
# - 2011 HAY QUE HACER LA CORRECIION DE LOS ARCHIVOS ZIP PARA ESTE ANHO
# - 2011 HAY QUE HACER LA CORRECIION DE LOS ARCHIVOS ZIP PARA ESTE ANHO
# - 2011 HAY QUE HACER LA CORRECIION DE LOS ARCHIVOS ZIP PARA ESTE ANHO

# - EN 2003 pasaron a formato electronico

# - Faltan cuadros en 2004 semana 7 (falta cuadro 3.2) preguntar a Betty

#-------------------------------------------------------------------------#


# Packages ----------------------------------------------------------------


library(pdftools)
library(manipulate)
library(tidyverse)


#install.packages("manipulate")

# Carpeta del proyecto ----------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo

# 1. Home
# 2. Work
# 3. Laptop
# 4. Other
rutas_prueba <- c("/Users/arrigocoen/Dropbox","/Users/useradmin/Dropbox","","")
eleccion_compu <- which(file.exists(rutas_prueba))

# Direcciones de cada computadora
source_raiz <- c("/Users/arrigocoen/Dropbox/1 Proyectos/2020/Asthma/R",# 1. Home
                 "",# 2. Work
                 "",# 3. Laptop
                 "")[eleccion_compu]# 4. Other

setwd(source_raiz)

source("Fn_epidemiologic.R")
# library()


# Function ----------------------------------------------------------------


# Example of extraction ---------------------------------------------------


# De
# https://rstudio-pubs-static.s3.amazonaws.com/415060_553527fd13ed4f30aae0f1e4483aa970.html

cat("\014") # borra consola 
rm(list=ls())  # Borra todo

PDF <- pdf_text("oregon_grass_and_legume_seed_crops_preliminary_estimates_2017.pdf") %>%
  readr::read_lines() #open the PDF inside your project folder

PDF.grass <-PDF[-c(1:3,6:8,20:35)] # remove lines
PDF.grass

all_stat_lines <- PDF.grass[3:13] %>%
  str_squish() %>%
  strsplit(split = " ")# remove empty spaces

var_lines <- c("Species", "Acreage", "Yield", "Production", "Price", "Value") # create your variable names

var_lines

# The next line is for some kind of correction that our file doesn't need, that's why I
# commented it
# all_stat_lines[[6]] <- c("Orchard", "grass","15,190","1,046","15,889","225.00","35,750") #change the line 6
df <- plyr::ldply(all_stat_lines) #create a data frame
head(df)

# asoetuh -----------------------------------------------------------------

# My data -----------------------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
pdf_name <- "sem01.pdf"
pdf_name <- "boletin 2019/sem01.pdf"

PDF <- pdf_text(pdf_name) %>%
  readr::read_lines() #open the PDF inside your project folder
PDF

length(PDF)
# asoetuh -----------------------------------------------------------------


texto <- "Nonsense?  kiss off, geek. what I said is true.  I'll have your account terminated."
grep(pattern ="bajo",PDF[1]) # encuentra una palabra en un texto
grep(pattern ="Tatata",texto) # encuentra una palabra en un texto
grep(pattern ="ru",texto) # encuentra una palabra en un texto

i <- 1
linea <- PDF[i]
linea

grep(pattern ="de",linea)


# asueth ------------------------------------------------------------------

i <- 2
linea <- PDF[i]
linea

grep(pattern ="de",linea)

if(length(grep(pattern ="Parkinson",linea))==1) print(i)
# asoetuh -----------------------------------------------------------------

cat("\014") # borra consola 
# rm(list=ls())  # Borra todo

intervalo <- 1:1000
intervalo <- 1:length(PDF)
intervalo <- 2621+ 0:1000 # CUADRO  DE Parkinson en i=2622



for(i in intervalo) {
  linea <- PDF[i]
  print(linea)
  
  # if(length(grep(pattern ="CUADRO",linea))==1) {
  #   print(linea)
  #   print(i)
  # }
}



# asoetuh -----------------------------------------------------------------


anho <- 2018
semana <- 16
semana <- 1
pdf_path <- genera_ruta_boletin(anho,semana)


intervalo <- 1:1000
intervalo <- 2621+ 0:1000 # CUADRO  DE Parkinson en i=2622
intervalo <- 1:length(PDF)



for(i in intervalo) {
  linea <- PDF[i]
  # print(linea)
  
  if(length(grep(pattern ="Neurológicas",linea))==1) {
    print(linea)
    print(i)
    break
  }
}
i

text_table <- PDF[i+10:41]
text_table




# soeuh -------------------------------------------------------------------



all_stat_lines <- text_table %>%
  str_squish() %>%
  strsplit(split = " ")# remove empty spaces

all_stat_lines

i <- 2
n_palabras <- 2
all_stat_lines



# nauht -------------------------------------------------------------------


all_stat_lines <- text_table %>%
  str_squish() %>%
  strsplit(split = " ")# remove empty spaces

# var_lines <- c("Species", "Acreage", "Yield", "Production", "Price", "Value") # create your variable names
# var_lines

# The next line is for some kind of correction that our file doesn't need, that's why I
# commented it
# all_stat_lines[[6]] <- c("Orchard", "grass","15,190","1,046","15,889","225.00","35,750") #change the line 6
df <- plyr::ldply(all_stat_lines) #create a data frame
head(df)




# Revision de correccion de espacios en lineas ----------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

# set.seed(42)

# Variables pdf
anho <- 2015
anho <- 2017
semana <- 16
semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
semana <- sample(1:52,1)

# Variables tabla
palabra_clave1 <- "Neurológicas"
palabra_clave1 <- "CUADRO 17" # el cuadro 17 es de enfermedades neurologicas
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 21:27
semanas <- 24
semanas <- 1
semanas <- 1:52
semanas <- 40:52

# Error en 2019 sem 24 "PDF error: Invalid Font Weight" pero los valores estan bien salvados
# Error: 2016 sem1 = tabla con longitudes incorrectas
# Error: 2016 = MUCHAS TABLAS con longitudes incorrectas 24-29

anhos <- 2017:2019 # ya fueron revisados para enfermedades neurologicas
anhos <- 2019 
anhos <- 2016 # ya fueron revisados para enfermedades neurologicas
anhos <- 2015 # ya fueron revisados para enfermedades neurologicas


correcciones_a_mano_CUADRO_17 <- function(text_table,semana,anho) {
  source("Fn_Correcciones_CUADRO_17_anho2016.R")
  source("Fn_Correcciones_CUADRO_17_anho2015.R")
  if(anho==2015) text_table <- fun_corrige_CUADRO_17_anho2015(text_table,semana,anho)
  if(anho==2016) text_table <- fun_corrige_CUADRO_17_anho2016(text_table,semana,anho)
  
  return(text_table)
}

for(anho in anhos) for(semana in semanas) {
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  cat("Trabajando: ",pdf_path,"\n")
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  # Extrayendo encabezados
  text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
  # Correccion cuadro
  text_table <- correcciones_a_mano_CUADRO_17(text_table,semana,anho)
  # print(text_table)
  all_stat_lines <- correccion_espacios_en_lineas(text_table)
  print(lengths(all_stat_lines))
  
  # Extrayendo encabezados
  encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(encabezado_table)
}




# otnh --------------------------------------------------------------------


# Correcciones de extraccion de informacion -------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")


# Variables tabla
palabra_clave1 <- "Neurológicas"
palabra_clave1 <- "CUADRO 17" # el cuadro 17 es de enfermedades neurologicas
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 
longitud_esperada <- 13 # longitud de numero de columnas

semanas <- 2
semanas <- 24:26
semanas <- c(1:3, 20:28)
semanas <- 25:52
semanas <- 1
semanas <- 24
semanas <- 20:30
semanas <- 2:52


anhos <- 2017:2019 # 2017:2019 ya fueron revisados para enfermedades neurologicas f tienen columnas y datos bien
anhos <- 2014
anhos <- 2015:2016 # 
anhos <- 2014 # de 2014 de la semana 25 a 52 no hay problema



for(anho in anhos) {
  if(anho %in% c(2015,2016)) break # Tablas ya corregidas
  texto <- paste0("#--------------------------------------------------------\n",
                  "#-- fun_corrige_CUADRO_17_anho",anho," --\n",
                  "#--------------------------------------------------------\n",
                  "#-- This function was generated by Fn_epidemiologic",
                  "\n#-- Author = Arrigo Coen \n\n\n",
                  "\nfun_corrige_CUADRO_17_anho",anho," <- function(text_table,semana,anho) {\n")
  for(semana in semanas) {
    longitud_esperada <- fun_longitud_esperada(semana,anho)
    pdf_path <- genera_ruta_boletin(anho,semana)
    cat("Trabajando: ",pdf_path,"\n")
    # Raw text of pdf
    PDF <- pdf_to_text(pdf_path)
    # Extrayendo encabezados
    text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
    print(text_table)
    all_stat_lines <- correccion_espacios_en_lineas(text_table)
    if(T) {
      df <- plyr::ldply(all_stat_lines) #create a data frame
      print(head(df))
    }
    idx_mal <- which(lengths(all_stat_lines)!=longitud_esperada)
    if(length(idx_mal)!=0) {
      texto <- paste0(texto,"\tif(semana==",semana," && anho==",anho,") {\n")
      for(i in idx_mal) {
        for(k in 1:10) text_table[i] <- gsub('     ', '    ', text_table[i]) # elimina el signo $
        texto <- paste0(texto,"\t\ti <- ",i,
                        ";\n\t\tx <- '",text_table[i],"'\n\t\ttext_table[i] <- x\n")
      }
      texto <- paste0(texto,"\t}\n")
    }
  }
  texto <- paste0(texto,"\treturn(text_table)\n}")
  name_file <- paste0(c("Fn_Correcciones_CUADRO_17_anho",anho,".R"),collapse = "")
  fileConn<-file(name_file)
  writeLines(texto, fileConn)
  close(fileConn)
  cat("Se genero el archivo ",name_file,"\n")
}

# cat(texto)


if(F) {
  df <- plyr::ldply(all_stat_lines) #create a data frame
  head(df)
}

# Indices de todos los cuadros --------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

semanas <- 25:52
semanas <- 52
semanas <- 1:2
semanas <- 1:52
semanas <- 1

anhos <- 2017:2019
anhos <- 2013


imprime_CUADRO_anho_semana(anhos,semanas)

# Cuadros un año ----------------------------------------------------------





# Revisando Titulo general ------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

# set.seed(42)

# Variables pdf
anho <- 2015
anho <- 2017
semana <- 16
semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
semana <- sample(1:52,1)

# Variables tabla
palabra_clave1 <- "Neurológicas"
palabra_clave1 <- "CUADRO 17" # el cuadro 17 es de enfermedades neurologicas
palabra_clave1 <- "CUADRO 3" # el cuadro 17 enfermedades prevenibles
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 21:27
semanas <- 24
semanas <- 1
semanas <- 1:52

# Error en 2019 sem 24 "PDF error: Invalid Font Weight" pero los valores estan bien salvados
# Error: 2016 sem1 = tabla con longitudes incorrectas
# Error: 2016 = MUCHAS TABLAS con longitudes incorrectas

anhos <- 2015 # ya fueron revisados para enfermedades neurologicas
anhos <- 2016 # ya fueron revisados para enfermedades neurologicas
anhos <- 2019
anhos <- 2015:2018 # ya fueron revisados para enfermedades neurologicas
anhos <- 2014:2015 # ya fueron revisados para enfermedades neurologicas

for(anho in anhos) for(semana in semanas) {
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  cat("Trabajando: ",pdf_path,"\n")
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  
  # Extrayendo encabezados
  text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(text_table)
  all_stat_lines <- correccion_espacios_en_lineas(text_table)
  print(lengths(all_stat_lines))
  
  # Extrayendo encabezados
  # encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(encabezado_table)
}


# astohu ------------------------------------------------------------------


# Correccion nombres 2013 -------------------------------------------------


# Revisor de primera linea ------------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

semanas <- 21:27
semanas <- 24
semanas <- 1
semanas <- 1:52

# Error en 2019 sem 24 "PDF error: Invalid Font Weight" pero los valores estan bien salvados
# Error: 2016 sem1 = tabla con longitudes incorrectas
# Error: 2016 = MUCHAS TABLAS con longitudes incorrectas

anhos <- 2015 # ya fueron revisados para enfermedades neurologicas
anhos <- 2018
anhos <- 2016:2019 # ya fueron revisados para enfermedades neurologicas
anhos <- 2015 # ya fueron revisados para enfermedades neurologicas
anhos <- 2014


revisa_linea_1_sem(anhos,semanas) 

# asotnu ------------------------------------------------------------------


colnames(df) <- c("Estado","Sem","Acum M","Acum F")


# asoetuh -----------------------------------------------------------------

for(i in 1:length(all_stat_lines)) print(length(all_stat_lines[[i]]))


# Analisis de encabezados de tablas ---------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

# set.seed(42)

# Variables pdf
anho <- 2015
anho <- 2017
semana <- 16
semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
semana <- sample(1:52,1)

# Variables tabla
palabra_clave1 <- "Neurológicas"
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

# Path of pdf
pdf_path <- genera_ruta_boletin(anho,semana)
pdf_path

# Raw text of pdf
PDF <- pdf_to_text(pdf_path)
# text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 


encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)

for(semana in 1:52) {
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  # Extrayendo encabezados
  encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  print(encabezado_table)
}


# asothe ------------------------------------------------------------------


# Extraccion de un cuadro -------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

# set.seed(42)

# Variables pdf
anho <- 2015
anho <- 2017
semana <- 16
semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
semana <- sample(1:52,1)

# Variables tabla

cuadro <- 2
cuadro <- 3.1

palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 7
semanas <- 1
semanas <- 1:4

# Error en 2019 sem 24 "PDF error: Invalid Font Weight" pero los valores estan bien salvados
# Error: 2016 sem1 = tabla con longitudes incorrectas
# Error: 2016 = MUCHAS TABLAS con longitudes incorrectas

anhos <- 2015 # ya fueron revisados para enfermedades neurologicas
anhos <- 2016 # ya fueron revisados para enfermedades neurologicas
anhos <- 2019
anhos <- 2015:2018 # ya fueron revisados para enfermedades neurologicas
anhos <- 2005 # ya fueron revisados para enfermedades neurologicas
anhos <- 2005:2019 # ya fueron revisados para enfermedades neurologicas

# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS

for(anho in anhos) for(semana in semanas) {
  
  n_col_esperado <- n_col_esperado_X_CUADRO(cuadro,semana,anho)
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  cat("Trabajando: ",pdf_path,"\n")
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  
  # Extrayendo encabezados
  text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(text_table)
  all_stat_lines <- correccion_espacios_en_lineas(text_table)
  # print(lengths(all_stat_lines))
  
  if(any(lengths(all_stat_lines)!=n_col_esperado)) {
    print("Posible error en el numero columnas -----------------")
  }
  
  # Extrayendo encabezados
  # encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(encabezado_table)
}


# Extraccion un cuadro ----------------------------------------------------





cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
set.seed(42)

# Variables pdf
anho <- 2015
anho <- 2017
semana <- 16
semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
semana <- sample(1:52,1)

semana <- 1
anho <- 2018

# Variables tabla

cuadro <- 2
cuadro <- 3.2
cuadro <- 3.1

palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 7
semanas <- 1
semanas <- 1:4

# Error en 2019 sem 24 "PDF error: Invalid Font Weight" pero los valores estan bien salvados
# Error: 2016 sem1 = tabla con longitudes incorrectas
# Error: 2016 = MUCHAS TABLAS con longitudes incorrectas

anhos <- 2015 # ya fueron revisados para enfermedades neurologicas
anhos <- 2016 # ya fueron revisados para enfermedades neurologicas
anhos <- 2019
anhos <- 2015:2018 # ya fueron revisados para enfermedades neurologicas
anhos <- 2005 # ya fueron revisados para enfermedades neurologicas
anhos <- 2010:2019 # ya fueron revisados para enfermedades neurologicas

# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS
# CORRER ESTA PARTE PARA DESCUBRIR CUALES SON LOS CUADROS CON NUMER DE COLUMNAS INCORRECTOS

for(anho in anhos) for(semana in semanas) {
  
  (n_col_esperado <- n_col_esperado_X_CUADRO(cuadro,semana,anho))
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  cat("Trabajando: ",pdf_path,"\n")
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  
  # Extrayendo encabezados
  text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(text_table)
  all_stat_lines <- correccion_espacios_en_lineas(text_table)
  # print(lengths(all_stat_lines))
  
  if(any(lengths(all_stat_lines)!=n_col_esperado)) {
    print("Posible error en el numero columnas -----------------")
  }
  
  # Extrayendo encabezados
  encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  print(encabezado_table)
}


# asoetuh -----------------------------------------------------------------

all_stat_lines <- correccion_espacios_en_lineas(text_table)

zero_value_character <- "-"

# text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 

from_text_to_numeric_matrix(all_stat_lines,anho,semana,longitud_tabla,zero_value_character)

# satoeh ------------------------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
set.seed(42)



cuadro <- 4.2; ncol_mat_cuadro <- ncol_CUADRO_X(cuadro,semana=1,anho=2019)
cuadro <- 3.1; 
# cuadro <- 3.6; ncol_mat_cuadro <- 11
palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 1:4
semanas <- 33
semanas <- 1:52


# ERROR EN Boletines/2010/sem36.pdf CUADRO 3.1
anhos <- 2017
anhos <- 2019
anhos <- 2012:2019

zero_value_character <- "-"

mat_cuadro <- matrix(0,length(anhos)*length(semanas)*longitud_tabla,ncol_mat_cuadro+2) # +2 because the columns year week

anho <- anhos[1]
semana <-  semanas[1]



l_main <- list_useful_var(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2)


name_file_All_Dat <- paste0(c("Data/All_Dat_",l_main$text_cuadro_guion,".RData"),collapse = "")

load(name_file_All_Dat)

head(BIG_mat_CUADRO)
dim(BIG_mat_CUADRO)

idx_mat_cuadro <- 1


for(anho in anhos) for(semana in semanas) {
  
  index_key <- find_index_key(anho,semana,BIG_mat_CUADRO)
  
  if(length(index_key)!=0) {
    cat("Datos de ",l_main$text_cuadro_guion,"anho=",anho," semana ",semana," ya obtenidos\n")
  } else {
    (n_col_esperado <- ncol_CUADRO_X(cuadro,semana=1,anho=2019))
    # Path of pdf
    pdf_path <- genera_ruta_boletin(anho,semana)
    cat("Trabajando: ",pdf_path,"\n")
    # Raw text of pdf
    PDF <- pdf_to_text(pdf_path)
    
    # Extrayendo encabezados
    text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
    # print(text_table)
    
    text_table <- Correcting_text_table(text_table,l_main)
    
    all_stat_lines <- correccion_espacios_en_lineas(text_table)
    
    mat_dat <- from_text_to_numeric_matrix(all_stat_lines,anho,semana,longitud_tabla,zero_value_character)
    BIG_mat_CUADRO <- rbind(BIG_mat_CUADRO,mat_dat)
    save(BIG_mat_CUADRO,file=name_file_All_Dat)
    cat("Datos de anho-semana-cuadro ACTUALIZADOS")
  }
  # mat_cuadro[idx_mat_cuadro:(idx_mat_cuadro+longitud_tabla-1),] <- mat_dat
  # idx_mat_cuadro <- idx_mat_cuadro + longitud_tabla
  # print(mat_dat)
}
s


# aseuh -------------------------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
set.seed(42)



cuadro <- 4.2; ncol_mat_cuadro <- ncol_CUADRO_X(cuadro,semana=1,anho=2019)
cuadro <- 3.1; 
# cuadro <- 3.6; ncol_mat_cuadro <- 11
palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 1:52


# ERROR EN Boletines/2010/sem36.pdf CUADRO 3.1
anhos <- 2017
anhos <- 2019
anhos <- 2012:2019

zero_value_character <- "-"

mat_cuadro <- matrix(0,length(anhos)*length(semanas)*longitud_tabla,ncol_mat_cuadro+2) # +2 because the columns year week

anho <- anhos[1]
semana <-  semanas[1]



l_main <- list_useful_var(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2)
names(l_main)

name_file_All_Dat <- paste0(c("Data/All_Dat_",l_main$text_cuadro_guion,".RData"),collapse = "")

load(name_file_All_Dat)
cuadro
estados_to_plot <- 9
estados_to_plot <- "Todos"
col_plot <- 7 # 7 equivale a columna 4, y 11 equivale a 8
col_plot <- 11 # 7 equivale a columna 4, y 11 equivale a 8
head(BIG_mat_CUADRO)
plot_state_info(estados_to_plot,col_plot,BIG_mat_CUADRO)
# imprime_CUADRO_anho_semana(anhos=2019,semanas=1)


# sauhteo -----------------------------------------------------------------


Unirse a la reunión Zoom
https://uammx.zoom.us/j/84954674369

ID de reunión: 849 5467 4369
Código de acceso: 330103

# Relacion estados numero -------------------------------------------------

l_main$state_numbers

# saoethu -----------------------------------------------------------------


imprime_CUADRO_anho_semana(anhos=2010,semanas=52)

# sutha -------------------------------------------------------------------




# Saving all data for a CUADRO --------------------------------------------



cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

cuadro <- 3.1; ncol_mat_cuadro <- ncol_CUADRO_X(cuadro,semana=1,anho=2019)

Initialize_All_Dat_RData_file(cuadro)

# fun_corrige_CUADRO_4_1_anho2016(text_table,semana,anho)


# all cuadros inizialized -------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

l_main <- Simple_list_useful_var()

for(cuadro in l_main$all_CUADRO_numbers_no_points) {
  cat("cuadro",cuadro,"\n")
  Initialize_All_Dat_RData_file(cuadro)
}

# astoeuh -----------------------------------------------------------------

load("Data/Dat CUADRO 3_6 2012-2019.RData")
col_plot <- 8 #posibles valores entre 3 y 11 ncol(mat_cuadro)
estados_to_plot <- 7
estados_to_plot <- 1:10
estados_to_plot <- "Todos"
estados_to_plot <- c(1,2,7,16)

# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
# CORREGIR ERROR AL EMPEZAR AQUI
plot_state_info(estados_to_plot,col_plot,mat_cuadro)


# Number of columns for 2019 any Cuadro -----------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
set.seed(42)
cuadro <- 4.1
semana <- 52
anho <- 2019
# imprime_CUADRO_anho_semana(anhos,semanas)
ncol_CUADRO_X(cuadro,semana,anho)

# Number of columns for 2019 any Cuadro -----------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
set.seed(42)

cuadro <- 3.2
cuadro <- 4.1

anhos <- c(2005:2010,2012:2019)
anhos <- 2015:2019
semanas <- 1
# imprime_CUADRO_anho_semana(anhos,semanas)

semana <- 52
anho <- 2019


for(anho in anhos) for(semana in semanas) {
  cat("Anho",anho," semana ",semana,"CUADRO ",cuadro," num. col.", ncol_CUADRO_X(cuadro,semana,anho),"\n")
}


# Correccion de numeros con espacios --------------------------------------


# Correcciones de extraccion de informacion -------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")


# Variables tabla

cuadro <- 3.1
cuadro <- 4.1
# cuadro <- 3.6; ncol_mat_cuadro <- 11
palabra_clave1 <- paste0("CUADRO ",as.character(cuadro))
palabra_clave2 <- "Aguascalientes"
longitud_esperada <- 13 # longitud de numero de columnas
longitud_tabla <- 32 # numero de estados 

remplaza_file <- F

semanas <- 2
semana <- 14:20
semanas <- 14:20

anhos <- 2018


genera0_imprime1 <- 0

anho <- anhos[1]
for(anho in anhos) {
  texto <- GEN_file_fun_corrige(cuadro,anho,palabra_clave1,palabra_clave2,remplaza_file)
}

cat(texto)
# MAT cuadros con errores -------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")

initialize_mat_errores_cuadro()

# asuth -------------------------------------------------------------------


semana <- 1
cuadro <- 1
anho <- 1981


load(file = "Data/Mat_errores_cuadro.RData")
head(mat_errores_cuadro)
mat_errores_cuadro[mat_errores_cuadro[,1]==anho & 
                     mat_errores_cuadro[,2]==semana &
                     mat_errores_cuadro[,3]==cuadro, ] <- c(T,"ncol wrong")

save(mat_errores_cuadro,file = "Data/Mat_errores_cuadro.RData")

# which(mat_errores_cuadro[,3]==cuadro)

# astoueh -----------------------------------------------------------------

load(file = "Data/Mat_errores_cuadro.RData")
head(mat_errores_cuadro)
which(apply(mat_errores_cuadro[,1:3] == c("2019","4","1"),1,all))
idx <- which(apply(mat_errores_cuadro[,1:2] == c(2019,4),1,all))
idx <- which(apply(mat_errores_cuadro[,1:3] == c(2019,4,13.4),1,all))
idx
mat_errores_cuadro[idx,]


unique(mat_errores_cuadro[,1])
# sotuh -------------------------------------------------------------------

# Works
mat_errores_cuadro <- matrix(1:9,3)
apply(mat_errores_cuadro[,1:3] == c(1,4,7),1,all)

# Correcciones de extraccion de informacion -------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")


# Variables tabla

cuadro <- 3.1; ncol_mat_cuadro <- 11
cuadro <- 4.1
# cuadro <- 3.6; ncol_mat_cuadro <- 11
palabra_clave1 <- paste0("CUADRO ",as.character(cuadro))
palabra_clave2 <- "Aguascalientes"
longitud_esperada <- 13 # longitud de numero de columnas
longitud_tabla <- 32 # numero de estados 

remplaza_file <- T

semanas <- 2
semana <- 14:20
semanas <- 14:20

anhos <- 2014

anho <- 2016

genera0_imprime1 <- 0



GEN_file_fun_corrige(anho,remplaza_file)



# Revisa correcciones de cuadro -------------------------------------------

review_correcciones_a_mano_CUADRO_17 <- function(text_table,semana,anho) {
  source("Fn_Correcciones_CUADRO_17_anho2016.R")
  source("Fn_Correcciones_CUADRO_17_anho2015.R")
  if(anho==2015) text_table <- fun_corrige_CUADRO_17_anho2015(text_table,semana,anho)
  if(anho==2016) text_table <- fun_corrige_CUADRO_17_anho2016(text_table,semana,anho)
  
  return(text_table)
}

for(anho in anhos) for(semana in semanas) {
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  cat("Trabajando: ",pdf_path,"\n")
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  # Extrayendo encabezados
  text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
  # Correccion cuadro
  text_table <- correcciones_a_mano_CUADRO_17(text_table,semana,anho)
  # print(text_table)
  all_stat_lines <- correccion_espacios_en_lineas(text_table)
  print(lengths(all_stat_lines))
  
  # Extrayendo encabezados
  encabezado_table <- extrae_encabezado_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(encabezado_table)
}

# stoehu ------------------------------------------------------------------



if(F) {
  df <- plyr::ldply(all_stat_lines) #create a data frame
  head(df)
}


# stnheu ------------------------------------------------------------------



# satoeh ------------------------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
set.seed(42)



cuadro <- 3.1; ncol_mat_cuadro <- 11
cuadro <- 4.1; ncol_mat_cuadro <- ncol_CUADRO_X(cuadro,semana=1,anho=2019)
# cuadro <- 3.6; ncol_mat_cuadro <- 11
palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
palabra_clave2 <- "Aguascalientes"
longitud_tabla <- 32 # numero de estados 

semanas <- 1:4
semanas <- 33
semanas <- 1:52


# ERROR EN Boletines/2010/sem36.pdf CUADRO 3.1
anhos <- 2012:2019
anhos <- 2017
anhos <- 2017

zero_value_character <- "-"

mat_cuadro <- matrix(0,length(anhos)*length(semanas)*longitud_tabla,ncol_mat_cuadro+2) # +2 because the columns year week

anho <- anhos[1]
semana <-  semanas[1]

l_main <- list_useful_var(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2)


idx_mat_cuadro <- 1
for(anho in anhos) for(semana in semanas) {
  
  index_key <- find_index_key(anho,semana,BIG_mat_CUADRO)
  
  if(length(index_key)!=0) {
    cat("Datos de anho-semana-cuadro ya obtenidos")
  } else {
    (n_col_esperado <- ncol_CUADRO_X(cuadro,semana=1,anho=2019))
    # Path of pdf
    pdf_path <- genera_ruta_boletin(anho,semana)
    cat("Trabajando: ",pdf_path,"\n")
    # Raw text of pdf
    PDF <- pdf_to_text(pdf_path)
    
    # Extrayendo encabezados
    text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
    # print(text_table)
    
    text_table <- Correcting_text_table(text_table,l_main)
    
    all_stat_lines <- correccion_espacios_en_lineas(text_table)
    
    mat_dat <- from_text_to_numeric_matrix(all_stat_lines,anho,semana,longitud_tabla,zero_value_character)
    BIG_mat_CUADRO <- rbind(BIG_mat_CUADRO,mat_dat)
  }
  # mat_cuadro[idx_mat_cuadro:(idx_mat_cuadro+longitud_tabla-1),] <- mat_dat
  # idx_mat_cuadro <- idx_mat_cuadro + longitud_tabla
  # print(mat_dat)
}

(correction_text_cuadro <- gsub('\\.', '_', palabra_clave1)) 
(text_andos <- paste0(as.character(range(anhos)),collapse = "-"))

(name_file_save <- paste0(c("Data/Dat ",correction_text_cuadro," ",text_andos,".RData"),collapse = ""))
save(mat_cuadro,anhos,cuadro,ncol_mat_cuadro,palabra_clave1,palabra_clave2,longitud_tabla,file=name_file_save)

ncol(mat_dat)
ncol(mat_cuadro)
















# nsotauh -----------------------------------------------------------------


