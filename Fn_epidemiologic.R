#-------------------------------------------------------------------------#
#                                                                         #
#                           Fn epidemiologic                              #
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


# Packages ----------------------------------------------------------------


library(pdftools)
library(tidyverse)

#install.packages("")



# Functions ---------------------------------------------------------------



# Corrigiendo nombres de más de una palabra

#' Corrige nombres de mas de una palabra
#'
#' @param all_stat_lines 
#' @param i 
#' @param n_palabras 
#'
#' @return
#' @export
#'
#' @examples
corrige_nombre_estado <- function(all_stat_lines,i,n_palabras) {
  valores <- all_stat_lines[[i]][(n_palabras+1):length(all_stat_lines[[i]])]
  nuevo_nombre <- paste0(all_stat_lines[[i]][1:n_palabras],collapse = " ")
  all_stat_lines[[i]] <- c(nuevo_nombre,valores)
  return(all_stat_lines)
}

#' Genera la ruta del archivo pdf
#'
#' @param anho 
#' @param semana 
#'
#' @return
#' @export
#'
#' @examples
#' anho <- 2018
#' semana <- 16
#' semana <- 1
#' pdf_path <- genera_ruta_boletin(anho,semana)
genera_ruta_boletin <- function(anho,semana) {
  if(semana<10) txt_semana <- paste0("0",semana) else txt_semana <- semana
  pdf_path <- paste0("Boletines/",anho,"/sem",txt_semana,".pdf")
  return(pdf_path)
}

#' Extraccion bruta de texto de un pdf 
#'
#' @param pdf_path 
#'
#' @return
#' @export
#'
#' @examples
pdf_to_text <- function(pdf_path) {
  PDF <- pdf_text(pdf_path) %>%
    readr::read_lines() #open the PDF inside your project folder
  return(PDF)
}


#' Extraccion como vector de texto de una tabla
#'
#' @param PDF archivo pdf
#' @param palabra_clave1 primera palabra clave: esta palabra debe solo aparecer una vez
#' en el documento
#' @param palabra_clave2 esta palabra debe ser la primera vez que aparece despues de
#' la palabra_clave1, y funciona como referencia para saber donde empezar a formar la tabla
#' @param longitud_tabla numero de renglones después de la palabra_clave2
#'
#' @return
#' @export
#'
#' @examples
#' PDF <- pdf_to_text("Boletines/2017/sem01.pdf")
#' palabra_clave1 <- "Neurológicas"
#' palabra_clave2 <- "Aguascalientes"
#' longitud_tabla <- 32
#' text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
#'  
extrae_text_tabla <- function(PDF,palabra_clave1,palabra_clave2,longitud_tabla) {
  
  # Finding 'Neurologicas' table
  intervalo <- 1:length(PDF)
  for(i in intervalo) {
    linea <- PDF[i]
    # print(linea)
    if(length(grep(pattern =palabra_clave1,linea))==1) {
      # print(linea)
      # print(i)
      break
    }
  }
  if(i==length(PDF)) for(j in 1:10) print("Posible error reporta funcion extrae_text_tabla")
  
  PDF[2000 + 0:1000]
  
  PDF[i+0:(longitud_tabla-1)]
  
  # Finding the first state (Aguascalientes)
  for(ii in i:length(PDF)) {
    linea <- PDF[ii]
    if(length(grep(pattern =palabra_clave2,linea))==1) {
      break
    }
  }
  
  
  text_table <- PDF[ii+0:(longitud_tabla-1)]
  return(text_table)
}


#' Extrae los encabezados de una tabla
#'
#' @param PDF 
#' @param palabra_clave1 
#' @param palabra_clave2 
#' @param longitud_tabla 
#'
#' @return
#' @export
#'
#' @examples
#' #' 
#' # source("Fn_epidemiologic.R")
#' # 
#' # # set.seed(42)
#' # 
#' # # Variables pdf
#' # anho <- 2015
#' # anho <- 2017
#' # semana <- 16
#' # semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
#' # semana <- sample(1:52,1)
#' # 
#' # # Variables tabla
#' # palabra_clave1 <- "Neurológicas"
#' # palabra_clave2 <- "Aguascalientes"
#' # longitud_tabla <- 32 
#' #  
#' # Path of pdf
#' pdf_path <- genera_ruta_boletin(anho,semana)
#' pdf_path
#' 
#' # Raw text of pdf
#' PDF <- pdf_to_text(pdf_path)
#' text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
#' 
extrae_encabezado_tabla <- function(PDF,palabra_clave1,palabra_clave2,longitud_tabla) {
  
  # Finding 'Neurologicas' table
  intervalo <- 1:length(PDF)
  for(i in intervalo) {
    linea <- PDF[i]
    # print(linea)
    if(length(grep(pattern =palabra_clave1,linea))==1) {
      # print(linea)
      # print(i)
      break
    }
  }
  
  # Finding the first state (Aguascalientes)
  for(ii in i:length(PDF)) {
    linea <- PDF[ii]
    if(length(grep(pattern =palabra_clave2,linea))==1) {
      break
    }
  }
  encabezado_table <- PDF[i:ii]
  return(encabezado_table)
}


#' correcion de espacios en lineas
#'
#' @param text_table 
#'
#' @return
#' @export
#'
#' @examples
#' #' "Fn_epidemiologic.R")
#' 
#' # set.seed(42)
#' 
#' # Variables pdf
#' anho <- 2015
#' anho <- 2017
#' semana <- 16
#' semana <- 25 # en 2019 semana 25 BC tiene dos lugares de numeros grandes 
#' semana <- sample(1:52,1)
#' 
#' # Variables tabla
#' palabra_clave1 <- "Neurológicas"
#' palabra_clave2 <- "Aguascalientes"
#' longitud_tabla <- 32 # numero de estados 
#' 
#' # Path of pdf
#' pdf_path <- genera_ruta_boletin(anho,semana)
#' pdf_path
#' 
#' 
#' # Raw text of pdf
#' PDF <- pdf_to_text(pdf_path)
#' text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
#'  
correccion_espacios_en_lineas <- function(text_table) {
  
  all_stat_lines <- list()
  for(i in 1:length(text_table)) {
    linea <- text_table[i]
    length(linea)
    s1 = unlist(strsplit(linea, split='  ', fixed=TRUE))
    s1 <- s1[s1!=""]
    all_stat_lines[[i]] <- c(s1[1],gsub(' ', '', s1[-1]))
  }
  
  if(!all(lengths(all_stat_lines)==length(all_stat_lines[[1]]))){
    cat("The line lenghts are ",lengths(all_stat_lines),"\n")
    getmode(lengths(all_stat_lines))
    not_equal_to_mode <- which(lengths(all_stat_lines)!=getmode(lengths(all_stat_lines)))
    all_stat_lines[not_equal_to_mode]
    for(i in 1:10) print("Error of lengths function correccion_espacios_en_lineas")
  }
  return(all_stat_lines[[1]])
}

#' get the mode of the vector
#' from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
#'
#' @param v 
#'
#' @return
#' @export
#'
#' @examples
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Esta funcion se utilizo para corregir los nombres de 2016
#'
#' @return
#' @export
#'
#' @examples
correcion_nombres_2016 <- function() {
  anho <- 2016
  
  semanas <- 1:52
  
  for(semana in semanas) {
    nombre_mal <- paste0("Correccion 2016/2016/BOL-EPID-",anho,"-SE",semana,".pdf")
    
    if(semana<10) txt_semana <- paste0("0",semana) else txt_semana <- semana
    if(file.exists(nombre_mal)){
      cat(nombre_mal,"  ",file.exists(nombre_mal),"\n")
      nuevo_nombre <- paste0("Correccion 2016/2016 corregidos/sem",txt_semana,".pdf")
      cat(nuevo_nombre,"\n")
      file.rename(nombre_mal,nuevo_nombre)
      # file.remove(nombre_mal)
    }
  }
}


#' Toma la primera linea de un pdf y regresa el numero, volumen y semana
#'
#' @param linea_1_pdf 
#'
#' @return
#' @export
#'
#' @examples
#' linea_1_pdf <- "Número 36 | Volumen 34 | Semana 36 | Del 3 al 9 de septiembre del 2017"
#' num_vol_sem_linea_1_pdf(linea_1_pdf)
num_vol_sem_anho_linea_1_pdf <- function(linea_1_pdf) {
  # if(length(linea_1_pdf)!=17) for(i in 1:10) print("Error en la longitud de linea_1_pdf")
  aux <- unlist(strsplit(linea_1_pdf, split=' ', fixed=TRUE))
  aux <- aux[aux!=""]
  return(aux[c(2,5,8,17)])
}


#' Revisor de la primera linea de varios archivos
#'
#' @param anhos 
#' @param semanas 
#'
#' @return
#' @export
#'
#' @examples
revisa_linea_1_sem <- function(anhos,semanas) {
  for(anho in anhos) {
    contador_errores <- 0
    for(semana in semanas) {
      # Path of pdf
      nuevos_errores <- 0
      pdf_path <- genera_ruta_boletin(anho,semana)
      # cat("Trabajando: ",pdf_path,"\n")
      # Raw text of pdf
      PDF <- pdf_to_text(pdf_path)
      
      (linea_1_pdf <- PDF[1])
      (num_vol_sem_anho <- num_vol_sem_anho_linea_1_pdf(linea_1_pdf))
      print(num_vol_sem_anho)
      
      if(is.na(num_vol_sem_anho[3])) {
        cat(pdf_path,"Error en semana; se extrajo NA\n")
        nuevos_errores <- nuevos_errores+1
        print(linea_1_pdf)
      } else if(num_vol_sem_anho[3]!=semana) {
        cat(pdf_path,"Error en semana; se extrajo del texto",num_vol_sem_anho[3],"\n")
        nuevos_errores <- nuevos_errores+1
        print(linea_1_pdf)
      }
      if(nuevos_errores>0) {
        contador_errores <- contador_errores+nuevos_errores
        cat(pdf_path,"\n")
      }
      # El anho es muy complejo ya que a veces ponen un periodo, por eso no lo reviso
      # if(num_vol_sem_anho[4]!=anho) {
      #   cat(pdf_path,"Error en anho\n")
      
      #   print(linea_1_pdf)
      # }
    }
    if(contador_errores==0) cat("No hay errores en",anho,"\n") else 
      cat("Se encontraron ",contador_errores,"errores en el anho",anho,"!!!!\n")
  }
}



#' Correcciones de los nombres de los archivos
#'
#' @param anho 
#'
#' @return
#' @export
#'
#' @examples
#' anho <- 2004
#' correcion_nombres_x_anho(anho)
#' for(anho in c(2016,2013,2012,2009:2006,2004:1995)) correcion_nombres_x_anho(anho)
correcion_nombres_x_anho <- function(anho) {
  if(!(anho %in% c(2016,2013,2012,2009:1995))) {
    print("Error reporta correcion_nombres_x_anho, no se tiene ese anho")
    return(F)
  }
  semanas <- 1:53
  if(anho==2016) {
    for(semana in semanas) {
      nombre_mal <- paste0("Correccion 2016/2016/BOL-EPID-",anho,"-SE",semana,".pdf")
      if(semana<10) txt_semana <- paste0("0",semana) else txt_semana <- semana
      if(file.exists(nombre_mal)){
        cat(nombre_mal,"  ",file.exists(nombre_mal),"\n")
        nuevo_nombre <- paste0("Correccion 2016/2016 corregidos/sem",txt_semana,".pdf")
        cat(nuevo_nombre,"\n")
        file.rename(nombre_mal,nuevo_nombre)
        # file.remove(nombre_mal)
      }
    }
  }
  
  if(anho==2013) {
    for(semana in semanas) {
      if(semana<10) txt_semana <- paste0("0",semana) else txt_semana <- semana
      nombre_mal <- paste0("Boletines/",anho,"/",anho,"_",txt_semana,".pdf")
      if(file.exists(nombre_mal)) {
        nuevo_nombre <- paste0("Boletines/",anho,"/sem",txt_semana,".pdf")
        cat("Antes era ",nombre_mal," ahora es ",nuevo_nombre,"\n")
        file.rename(nombre_mal,nuevo_nombre)
      }
    }
  }
  if(anho %in% c(2012,2009:2006,2004:1995)) {
    for(semana in semanas) {
      if(semana<10) txt_semana <- paste0("0",semana) else txt_semana <- semana
      nombre_mal <- paste0("Boletines/",anho,"/",anho,"_sem",txt_semana,".pdf")
      if(file.exists(nombre_mal)) {
        nuevo_nombre <- paste0("Boletines/",anho,"/sem",txt_semana,".pdf")
        cat("Antes era ",nombre_mal," ahora es ",nuevo_nombre,"\n")
        file.rename(nombre_mal,nuevo_nombre)
      }
      
      nombre_mal <- paste0("Boletines/",anho,"/",anho,"_sem",semana,".pdf")
      if(file.exists(nombre_mal)) {
        nuevo_nombre <- paste0("Boletines/",anho,"/sem",txt_semana,".pdf")
        cat("Antes era ",nombre_mal," ahora es ",nuevo_nombre,"\n")
        file.rename(nombre_mal,nuevo_nombre)
      }
    }
  }
  if(anho %in% c(2005)) {
    for(semana in 1:9) {
      if(semana<10) txt_semana <- paste0("0",semana) else txt_semana <- semana
      nombre_mal <- paste0("Boletines/",anho,"/sem",semana,".pdf")
      if(file.exists(nombre_mal)) {
        nuevo_nombre <- paste0("Boletines/",anho,"/sem",txt_semana,".pdf")
        cat("Antes era ",nombre_mal," ahora es ",nuevo_nombre,"\n")
        file.rename(nombre_mal,nuevo_nombre)
      }
    }
  }
}


#' numero de columnas esperadas para el CUADRO 17
#'
#' @param semana 
#' @param anho 
#'
#' @return
#' @export
#'
#' @examples
fun_longitud_esperada <- function(semana,anho) {
  # Recuerda que los nombres de los estados son la primera columna
  if(anho>2015 || (anho==2015 && semana>1) ) return(12+1) # se esperan 13 columnas desdo 2015-2 a 2019-52
  if(anho==2015 && semana==1) return(9+1) # se esperan 9 columnas para 2015-1
  if(anho==2014 && semana>1) return(9+1) # se esperan 9 columnas para 2015-1
  # if(anho<=2015 ) return(9) # falta revisar este caso
  for(i in 1:10) print("No se ha visto el numero de columnas para este caso, reporta funcion fun_longitud_esperada")
}



#' Numero de columnas esperadas por cada cuadro
#'
#' @param cuadro 
#' @param semana 
#' @param anho 
#'
#' @return
#' @export
#'
#' @examples
n_col_esperado_X_CUADRO <- function(cuadro,semana,anho) {
  if(cuadro == 3.1) {
    if(anho>=2005) return(9)
  }
  
  if(cuadro == 3.2) {
    if(anho>=2005) return(9)
  }
  if(cuadro == 17) {
    # Recuerda que los nombres de los estados son la primera columna
    if(anho>2015 || (anho==2015 && semana>1) ) return(12+1) # se esperan 13 columnas desdo 2015-2 a 2019-52
    if(anho==2015 && semana==1) return(9+1) # se esperan 9 columnas para 2015-1
    if(anho==2014 && semana>1) return(9+1) # se esperan 9 columnas para 2015-1
  }
  
  # if(anho<=2015 ) return(9) # falta revisar este caso
  for(i in 1:10) print("No se ha visto el numero de columnas para este caso, reporta funcion fun_longitud_esperada")
}

#' Transforma texto a matriz de datos 
#'
#' @param all_stat_lines 
#' @param anho 
#' @param semana 
#' @param longitud_tabla 
#' @param zero_value_character 
#'
#' @return
#' @export
#'
#' @examples
from_text_to_numeric_matrix <- function(all_stat_lines,anho,semana,longitud_tabla,zero_value_character) {
  # Extracting the values
  df <- plyr::ldply(all_stat_lines) #create a data frame
  # Eliminating state names, since we use the numbers 1:32
  mat_dat <- as.matrix(df)[,-1]
  # Substituing the entries with zero observations to the value 0
  mat_dat[mat_dat==zero_value_character] <- 0
  # Transforming into a matrix
  dimesiones <- dim(mat_dat)
  mat_dat <- matrix(as.numeric(mat_dat),dimesiones[1])
  # Generating the needed matrix
  cols_anho_semana_estado <- matrix(c(rep(anho,longitud_tabla),
                                      rep(semana,longitud_tabla),
                                      1:longitud_tabla),ncol = 3)
  mat_dat <- cbind(cols_anho_semana_estado,mat_dat)
  return(mat_dat)
}




plot_state_info <- function(estados_to_plot,col_plot,mat_cuadro) {
  
  # Acumulate of all states: in this casse we plot the addition of all values
  if(all(estados_to_plot=="Todos")) {
    all_states <- T 
    estados_to_plot <- 1:32
  } else all_states <- F
  
  # First plot
  n_estado <- estados_to_plot[1]
  info_estado <- mat_cuadro[mat_cuadro[,3]==n_estado,]
  vec_to_plot <- info_estado[,col_plot]
  startDate <- paste0(min(anhos),"-01-01")
  endDate <- paste0(max(anhos),"-12-31")
  
  dates_weeks <- seq(as.Date(startDate), as.Date(endDate), by="weeks")
  dates_weeks <- dates_weeks[1:length(vec_to_plot)]
  
  if(length(estados_to_plot)==1) {
    plot(dates_weeks,vec_to_plot,type="l",col="blue",main=palabra_clave1,ylab="",xlab="")
  } else {
    
    mat_dat_estados <- matrix(0,length(estados_to_plot),length(dates_weeks))
    mat_dat_estados[1,] <- vec_to_plot
    
    for(i in 2:length(estados_to_plot)) {
      info_estado <- mat_cuadro[mat_cuadro[,3]==estados_to_plot[i],]
      vec_to_plot <- info_estado[,col_plot]
      mat_dat_estados[i,] <- vec_to_plot
    }
    if(all_states) {
      plot(dates_weeks,colSums(mat_dat_estados),type="l",col="blue",main=palabra_clave1,ylab="",xlab="")
    } else {
      matplot(dates_weeks,t(mat_dat_estados),type="l",main=palabra_clave1,ylab="",xlab="")
    }
  }
}



#' Dados año y semana imprime los cuadros encontrados en el PDF
#'
#' @param anhos 
#' @param semanas 
#'
#' @return
#' @export
#'
#' @examples
#' #' semanas <- 1
#' anhos <- 2017
#' imprime_CUADRO_anho_semana(anhos,semanas)
#' 
#' # Also works for vectors
#' #' semanas <- 1
#' anhos <- 2017:2019
#' imprime_CUADRO_anho_semana(anhos,semanas)
#' 
imprime_CUADRO_anho_semana <- function(anhos,semanas) {
  palabra_clave1 <- "CUADRO"
  for(anho in anhos) for(semana in semanas) {
    cat("\n------ CUADROS ANHO ",anho," SEMANA",semana," ------ \n")
    pdf_path <- genera_ruta_boletin(anho,semana)
    cat("Trabajando: ",pdf_path,"\n")
    # Raw text of pdf
    PDF <- pdf_to_text(pdf_path)
    
    intervalo <- 1:length(PDF)
    n_cuadros <- 0
    for(i in intervalo) {
      linea <- PDF[i]
      # print(linea)
      if(length(grep(pattern =palabra_clave1,linea))==1) {
        n_cuadros <- n_cuadros +1
        cat(n_cuadros," ",trimws(linea),"\n") # trimws quita espacios iniciales y finales
        # break
      }
    }
    # cat("El total de cuadros fue ",n_cuadros,"\n")
  }
}


#' Extrae el numero de columnas de un cuadro con respecto a la semana y anho
#'
#' @param cuadro 
#' @param semana 
#' @param anho 
#'
#' @return
#' @export
#'
#' @examples
#' cuadro <- 4.1
#' semana <- 52
#' anho <- 2019
ncol_CUADRO_X <- function(cuadro=3.1,semana=1,anho=2019) {
  # cuadro <- 3.6; ncol_mat_cuadro <- 11
  palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
  palabra_clave2 <- "Aguascalientes"
  longitud_tabla <- 32 # numero de estados 
  idx_mat_cuadro <- 1
  # (n_col_esperado <- n_col_esperado_X_CUADRO(cuadro,semana,anho))
  # Path of pdf
  pdf_path <- genera_ruta_boletin(anho,semana)
  # cat("Trabajando: ",pdf_path,"\n")
  # Raw text of pdf
  PDF <- pdf_to_text(pdf_path)
  # Extrayendo encabezados
  text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla)
  # print(text_table)
  all_stat_lines <- correccion_espacios_en_lineas(text_table)
  df <- plyr::ldply(all_stat_lines) 
  return(ncol(df))
}


#' A list with useful variables
#'
#' @param cuadro number of cuadro: eg 3.4
#' @param anho number of year
#' @param semana number of week
#' @param anhos vector with years
#' @param semanas vector with weeks
#' @param palabra_clave1 
#' @param palabra_clave2 
#'
#' @return
#' @export
#'
#' @examples
list_useful_var <- function(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2) {
  l_main <- list(cuadro=cuadro,anhos=anhos,semanas=semana,anho=anho,semana=semana)
  (l_main$text_cuadro_guion <- paste0(c("CUADRO_",gsub('\\.', '_', cuadro)),collapse=""))
  l_main$current_time <-  format(Sys.time(), "%a %b %d %X %Y")
  
  l_main$n_states <- 32
  l_main$longitud_tabla <- 32 # numero de estados 
  
  l_main$all_CUADRO_numbers <- c("1.",
                                 "2. ",
                                 "3. ","3.1 ","3.2 ","3.3 ","3.4 ","3.5 ","3.6 ","3.7 ",
                                 "4. ","4.1 ","4.2 ","4.3 ","4.4 ",
                                 "5. ","5.1 ",
                                 "6. ","6.1 ","6.2 ","6.3 ",
                                 "7. ","7.1 ","7.2 ","7.3 ","7.4 ",
                                 "8. ","8.1 ",
                                 "9. ",
                                 "10. ","10.1","10.2","10.3","10.4",
                                 "11. ","11.1",
                                 "12. ","12.1",
                                 "13. ","13.1","13.2","13.3","13.4","13.5","13.6","13.7","13.8",
                                 "14. ","14.1",
                                 "15. ","15.1",
                                 "16. ","16.1",
                                 "17. ",
                                 "18. " ,"18.1","18.2")
  
  l_main$all_CUADRO_numbers_no_points <- c("1",
                                           "2",
                                           "3","3.1","3.2 ","3.3 ","3.4 ","3.5 ","3.6 ","3.7 ",
                                           "4","4.1","4.2 ","4.3 ","4.4 ",
                                           "5","5.1",
                                           "6","6.1","6.2 ","6.3 ",
                                           "7","7.1","7.2 ","7.3 ","7.4 ",
                                           "8","8.1",
                                           "9",
                                           "10","10.1","10.2","10.3","10.4",
                                           "11","11.1",
                                           "12","12.1",
                                           "13","13.1","13.2","13.3","13.4","13.5","13.6","13.7","13.8",
                                           "14","14.1",
                                           "15","15.1",
                                           "16","16.1",
                                           "17",
                                           "18" ,"18.1","18.2")
  # State names in order  
  l_main$state_numbers <- c("Aguascalientes","Baja California",
                            "Baja California Sur","Campeche",
                            "Coahuila","Colima",
                            "Chiapas","Chihuahua",
                            "Ciudad de México","Durango",
                            "Guanajuato","Guerrero",
                            "Hidalgo","Jalisco",
                            "México","Michoacán",
                            "Morelos","Nayarit",
                            "Nuevo León","Oaxaca",
                            "Puebla","Querétaro",
                            "Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
                            "Tabasco","Tamaulipas",
                            "Tlaxcala","Veracruz",
                            "Yucatán","Zacatecas")
  l_main$all_years <- 1981:2020
  
  l_main$all_years <- l_main$all_years[l_main$all_years!=2011]
  l_main$possible_weeks <- 1:53
  return(l_main)
}


Simple_list_useful_var <- function() {
  
  cuadro <- 3.1
  palabra_clave1 <- paste0("CUADRO ",as.character(cuadro))
  palabra_clave2 <- "Aguascalientes"
  
  semana <- 1
  semanas <- 1:52
  
  anhos <- 2019
  
  anho <- 2019
  l_main <- list_useful_var(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2)
  return(l_main)
}


#' Genera achivos para corregir manualmente renglones con errores
#'
#' @param anho año que se quiere corregir
#' @param remplazar_Fn_Correcciones_existente si se substituye archivo existente
#' @param cuadro 
#' @param palabra_clave1 
#' @param palabra_clave2 
#'
#' @return
#' @export
#'
#' @examples
GEN_file_fun_corrige <- function(cuadro,anho,palabra_clave1,palabra_clave2,
                                 remplaza_file=F) {
  
  
  semanas <- 1:2 # ONLY FOR TESTING
  semanas <- 1:52
  
  anhos <- anho
  
  l_main <- list_useful_var(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2)
  
  name_function <- paste0(c("fun_corrige_",l_main$text_cuadro_guion,"_anho",l_main$anho),collapse = "")
  longitud_esperada <- ncol_CUADRO_X(cuadro,semana=1,anho=2019)
  
  name_file <- paste0(c("Fn_Correcciones/Fn_Correcciones_",l_main$text_cuadro_guion,"_anho",l_main$anho,".R"),collapse = "")
  
  if(file.exists(name_file) & !remplaza_file) {
    cat("The file ",name_file, " ALREADY EXISTS.\n")
    return(NULL)
  }
  
  # Indicator if there is any line with problems
  problem_in_year <- F
  
  texto <- paste0("#--------------------------------------------------------\n",
                  "#-- fun_corrige_",l_main$text_cuadro_guion,"_anho",l_main$anho," --\n",
                  "#--------------------------------------------------------\n",
                  "#-- This function was generated by Fn_epidemiologic",
                  "\n#-- Created on: ",l_main$current_time,
                  "\n#-- Author = Arrigo Coen \n\n\n",
                  "\n# Current expected number of columns is: ",longitud_esperada,"\n",
                  "\n# Number of weeks consider from ",min(semanas)," to ",max(semanas),"\n",
                  "\n# Obs: The commented values are the original, and they are as a reference.\n",
                  "\n",name_function," <- function(text_table,semana,anho) {\n")
  
  
  # cat(texto)
  semana <- semanas[1]
  for(semana in semanas) {
    # We take as reference the 2019
    
    pdf_path <- genera_ruta_boletin(anho,semana)
    cat("Trabajando: ",pdf_path,"\n")
    # Raw text of pdf
    PDF <- pdf_to_text(pdf_path)
    # Extrayendo encabezados
    text_table <- extrae_text_tabla(PDF,palabra_clave1,palabra_clave2,longitud_tabla) 
    # print(text_table)
    all_stat_lines <- correccion_espacios_en_lineas(text_table)
    if(F) {
      df <- plyr::ldply(all_stat_lines) #create a data frame
      # print(head(df))
    }
    idx_mal <- which(lengths(all_stat_lines)!=longitud_esperada)
    
    if(length(idx_mal)!=0) {
      # If this happen there is at least one row with weird lenght
      problem_in_year <- T
      
      load(file = "Data/Mat_errores_cuadro.RData")
      head(mat_errores_cuadro)
      mat_errores_cuadro[mat_errores_cuadro[,1]==anho & 
                           mat_errores_cuadro[,2]==semana &
                           mat_errores_cuadro[,3]==cuadro, ] <- c(T,"ncol wrong")
      
      save(mat_errores_cuadro,file = "Data/Mat_errores_cuadro.RData")
      
      texto <- paste0(texto,"\tif(semana==",semana," && anho==",anho,") {\n")
      for(i in idx_mal) {
        for(k in 1:10) text_table[i] <- gsub('     ', '    ', text_table[i]) # elimina el signo $
        texto <- paste0(texto,"\t\ti <- ",i,";\n",
                        "\t\t#",text_table[i],"\n",
                        "\t\ttext_table[i] <- '",text_table[i],"'\n")
      }
      texto <- paste0(texto,"\t}\n")
    } else cat("No problem with the any column.\n")
  }
  texto <- paste0(texto,"\treturn(text_table)\n}")
  # name_file <- paste0(c("Fn_Correcciones/Fn_Correcciones_",l_main$text_cuadro_guion,"_anho",l_main$anho,".R"),collapse = "")
  
  if(problem_in_year) {
    if(!file.exists(name_file)) {
      fileConn<-file(name_file)
      writeLines(texto, fileConn)
      close(fileConn)
      cat("Se genero el archivo ",name_file,"\n")
      return(texto)
    } 
    # Archivo existe y se remplaza
    if(file.exists(name_file) & !remplaza_file) {
      fileConn<-file(name_file)
      writeLines(texto, fileConn)
      close(fileConn)
      cat("Se genero el archivo ",name_file,"\n")
      return(texto)
    } 
    # Archivo existe pero no se remplaza
    if(file.exists(name_file) & remplaza_file) {
      cat("The file ",name_file, " ALREADY EXISTS and was'nt change.")
      return(texto)
    }
    
    # if(genera0_imprime1) cat(texto)
  }
  return(texto)
}


#' Save file with a matrix with the errors
#'
#' @return
#' @export
#'
#' @examples
initialize_mat_errores_cuadro <- function() {
  l_main <- Simple_list_useful_var()
  anhos_weeks_n_Cuadro <- matrix(as.numeric(as.matrix(expand.grid(l_main$all_years, l_main$possible_weeks,l_main$all_CUADRO_numbers ) )),ncol=3)
  col_names_mat_errores_cuadro <- c("Anho","Week","Cuadro num.","Error","Description","Corrected")
  mat_errores_cuadro <- matrix(0,nrow(anhos_weeks_n_Cuadro),length(col_names_mat_errores_cuadro))
  colnames(mat_errores_cuadro) <- col_names_mat_errores_cuadro
  mat_errores_cuadro[,1:3] <- anhos_weeks_n_Cuadro
  mat_errores_cuadro[,4:6] <- NA
  
  
  save(mat_errores_cuadro,file = "Data/Mat_errores_cuadro.RData")
  head(mat_errores_cuadro)
}


#' Calling the Fn_Correcciones_ functions
#'
#' @param text_table 
#' @param l_main 
#'
#' @return
#' @export
#'
#' @examples
Correcting_text_table <- function(text_table,l_main){
  name_file <- paste0(c("Fn_Correcciones/Fn_Correcciones_",l_main$text_cuadro_guion,"_anho",l_main$anho,".R"),collapse = "")
  if(!file.exists(name_file)) return(text_table)
  source(name_file)
  text_eval <- paste0(c("text_table <- fun_corrige_",l_main$text_cuadro_guion,"_anho",l_main$anho,
                        "(text_table,semana,anho)"),collapse = "")
  
  eval(parse(text=text_eval))
  return(text_table)
}


#' Inicializa el salvado de variables
#'
#' @param cuadro 
#'
#' @return
#' @export
#'
#' @examples
Initialize_All_Dat_RData_file <- function(cuadro) {
  anho <- 2019
  semana <-  1
  longitud_tabla <- 32
  anhos <- 2017
  semanas <- 1
  zero_value_character <- "-"
  
  # cuadro <- 3.6; ncol_mat_cuadro <- 11
  palabra_clave1 <- paste0("CUADRO ",as.character(cuadro)) # el cuadro 17 enfermedades prevenibles
  palabra_clave2 <- "Aguascalientes"
  
  l_main <- list_useful_var(cuadro,anho,semana,anhos,semanas,palabra_clave1,palabra_clave2)
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
  
  
  BIG_mat_CUADRO <- mat_dat
  vec_extra <- gsub("\\s+", "", str_trim(paste("Dat",1:(ncol_CUADRO_X(cuadro,1,2019)-1))))
  colnames(BIG_mat_CUADRO) <- c("Anho","Semana","Estado",vec_extra)
  
  name_file <- paste0(c("Data/All_Dat_",l_main$text_cuadro_guion,".RData"),collapse = "")
  
  if(!file.exists(name_file)) save(BIG_mat_CUADRO,file=name_file) else cat("The file ",name_file,"ALREADY EXISTS!!!\n")
  
}

find_index_key <- function(anho,semana,BIG_mat_CUADRO) {
  index_key <- which(BIG_mat_CUADRO[,1]==anho & BIG_mat_CUADRO[,2]==semana)
  return(index_key)
}


#' Extraction of the info of one line
#'
#' @param anho 
#' @param keyword 
#'
#' @return
#' @export
#'
#' @examples
#' anho <- 2019
#' keyword <- "Fiebre tifoidea"
extract_one_line <- function(anho,keyword,l_PDF) {
  
  PDF <- one_PDF_fast(anho,l_PDF)
  
  # Extraction of the line with respect to the keyword
  # (idx_first_word <- grep(pattern =keyword,PDF)) # Old way
  (idx_first_word <- startsWith(PDF, keyword))
  text_vec <- PDF[idx_first_word]
  
  if(length(text_vec)==0) text_one_line <- rep(NA,l_PDF$ncol_m_enfermedad + 1) else {
    text_one_line <- correccion_espacios_en_lineas(text_vec)
  }
  # if(is.list(text_one_line))  return(text_one_line[[1]])
  return(text_one_line)
}




#' Extract from the saved file the information of one PDF
#'
#' @param anho 
#' @param l_PDF 
#'
#' @return
#' @export
#'
#' @examples
one_PDF_fast <- function(anho,l_PDF) {
  text_to_eval <- paste0(c("PDF = l_PDF$PDF_raw_",anho),collapse = "")
  eval(parse(text=text_to_eval))
  return(PDF)
}

#' Correcting columns of the matrix m_enfermedad
#'
#' @param m_enfermedad 
#'
#' @return
#' @export
#'
#' @examples
correcting_columns_m_enfermedad <- function(m_enfermedad) {
  # Correct Total: error = some cases had comma to separate the numbers
  n_col_corrige <- 2
  columna <- m_enfermedad[,n_col_corrige]
  columna <- as.numeric(gsub('\\,', '', columna))
  m_enfermedad[,n_col_corrige] <- columna
  # Correct Tasa: error = some cases had comma in the place of period
  n_col_corrige <- 3
  columna <- m_enfermedad[,n_col_corrige]
  columna <- gsub("\\,", ".", str_trim(columna))
  m_enfermedad[,n_col_corrige] <- columna
  
  # Transforming into numeric
  m_enfermedad <- structure(vapply(m_enfermedad, as.numeric, numeric(1)), dim=dim(m_enfermedad))
  colnames_m_enfermedad <- c("Año","Total","Tasa","<1","1-4","5-9","10 - 14","15 - 19","20 - 24","25 - 44","45 - 49","50 - 59","60 - 64","65 y +","Ign.")
  colnames(m_enfermedad) <- colnames_m_enfermedad
  
  
  return(m_enfermedad)
}



#' Generation of the matrix m_enfermedad
#'
#' @param keyword 
#' @param anhos 
#'
#' @return
#' @export
#'
#' @examples
GEN_m_enfermedad <- function(keyword,anhos,l_PDF) {
  
  colnames_m_enfermedad <- c("Año","Total","Tasa","<1","1-4","5-9","10 - 14","15 - 19","20 - 24","25 - 44","45 - 49","50 - 59","60 - 64","65 y +","Ign.")
  length(colnames_m_enfermedad)
  
  m_enfermedad <- matrix(NA,length(anhos),length(colnames_m_enfermedad))
  m_enfermedad[,1] <- anhos
  colnames(m_enfermedad) <- colnames_m_enfermedad
  
  # Extra variable for testing
  print_line_year = T
  print_line_year = F
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
    # m_enfermedad[i,1] <- anho
    m_enfermedad[i,-1] <- text_one_line[-1:-2]
  }
  
  m_enfermedad <- correcting_columns_m_enfermedad(m_enfermedad)
  
  if(keyword=="Poliomielitis aguda           ") {
    # 2005
    m_enfermedad[3,-1] <- 0
    # 2006
    m_enfermedad[4,-1] <- 0
    m_enfermedad[4,c(2,4)] <- 1
    # 2007
    m_enfermedad[5,-1] <- 0
    # 2007
    m_enfermedad[6,-1] <- 0
  }
  
  
  return(m_enfermedad)
}


#' Correction of keywords
#'
#' Obs a current problem is the elimination of spaces
#'
#' @param keyword 
#'
#' @return
#' @export
#'
#' #' @examples
#' keyword <- "Microcefalia**                "
#' correcting_keyword(keyword) # returns "Microcefalia"
correcting_keyword <- function(keyword) {
  # We only consider the text since some extra characters give problems
  # keyword <- sub("^([[:alpha:]]*).*", "\\1", keyword) # keeping only letters errasing spaces
  keyword <- gsub("[^[:alnum:][:space:]]","",keyword)
  
  return(keyword)
}


#' Given a matrix of data of one disease and three years this gives the
#' barplot w.r.t. the age interval 
#'
#' @param three_years 
#' @param m_enfermedad 
#'
#' @return
#' @export
#'
#' @examples
#' load(file="Variables_Vaccine.RData")
#' 
#' # set.seed(42)
#' (three_years <- sort(sample(anhos,3,replace = F)))
#' (keyword <- sub_keywords[3])
#' 
#' m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
#' m_enfermedad
#' plot_3years_barplot(three_years,m_enfermedad)
plot_3years_barplot <- function(three_years,m_enfermedad,vec_partition_bars=1:11) {
  
  par(mfrow=c(3,1))
  for(i in 1:3) {
    year <- three_years[i]
    datos_anho <- m_enfermedad[m_enfermedad[,1]==year,4:(ncol(m_enfermedad)-1)]
    
    my_barplot_age_blocks(datos_anho,year,vec_partition_bars) 
    # barplot(datos_anho, main=year)
  }
  par(mfrow=c(1,1))
}



#' Imprime las primeras letras de cada renglon de un pdf
#'
#' @param anho 
#' @param l_PDF 
#' @param cuantas_letras 
#'
#' @return
#' @export
#'
#' @examples
#' cat("\014") # borra consola 
#' rm(list=ls())  # Borra todo
#' source("Fn_epidemiologic.R")
#' load(file="Variables_Vaccine.RData")
#' imprime_primeras_letras_cada_renglon(2010,l_PDF,25)
imprime_primeras_letras_cada_renglon <- function(anho,l_PDF,cuantas_letras =20) {
  PDF <- one_PDF_fast(anho,l_PDF)
  for(i in 1:length(PDF)) cat(i," ",substr(PDF[i],1,cuantas_letras),"\n")
}


#' Extracting the information between two line of keywords
#'
#' @param anho
#' @param l_PDF
#' @param keyword_1 first keyword
#' @param keyword_2 second keyword
#'
#' @return
#' @export
#'
#' @examples
#' cat("\014") # borra consola 
#' rm(list=ls())  # Borra todo
#' source("Fn_epidemiologic.R")
#' load(file="Variables_Vaccine.RData")
#' anho <- 2009
#' 
#' keyword_1 <- "Influenza debida a virus de la influenza"
#' keyword_2 <- "identificado"
#' extract_between_lines(anho,keyword_1,keyword_2,l_PDF)
extract_between_lines <- function(anho,keyword_1,keyword_2,l_PDF) {
  PDF <- one_PDF_fast(anho,l_PDF)
  # Extraction of the line with respect to the keyword
  (idx_keyword_1 <- which(startsWith(PDF, keyword_1)))
  (idx_keyword_2 <- which(startsWith(PDF, keyword_2)))
  
  # In caso of not finding the keyword
  if(length(idx_keyword_1)==0) {
    cat("Warning - keyword_1=",keyword_1,"not find for year ",anho," function extract_between_lines\n")
    return(rep(NA,l_PDF$ncol_m_enfermedad + 1) )
  }
  if(length(idx_keyword_2)==0) {
    cat("Warning - keyword_2=",keyword_2,"not find for year ",anho," function extract_between_lines\n")
    return(rep(NA,l_PDF$ncol_m_enfermedad + 1) )
  }
  
  # The keywords need to be separeted by only one line
  if(idx_keyword_1!=idx_keyword_2-2) {
    for(i in 1:10) print("Error - keyword_1 is not separeted by one line from keyword_2")
    return(-1)
  }
  # We get the information of the line between
  text_vec <- PDF[idx_keyword_1+1]
  # Some corrections
  if(length(text_vec)==0) text_one_line <- rep(NA,l_PDF$ncol_m_enfermedad + 1) else {
    text_one_line <- correccion_espacios_en_lineas(text_vec)
  }
  # We paste the two words at the beginig of the vector
  text_one_line <- c(paste0(c(keyword_1,keyword_2),collapse = " "),text_one_line)
  
  return(text_one_line)
}


#' Generation of INFLUENZA tables
#'
#' @param i_Influenza value in 1-4 that represents: "Influenza                  ",
#' "Influenza A(H1N1), 2009 identificado              ",
#' "Influenza debida a virus de la influenza identificado",
#' "Influenza debida a virus no identificado          "
#' @param anhos 
#' @param l_PDF 
#'
#' @return
#' @export
#'
#' @examples
GEN_m_enfermedad_INFLUENZA <- function(i_Influenza,anhos,l_PDF) {
  
  keywords_options_Influenza <- c("Influenza                  ",
                                  "Influenza A(H1N1), 2009 identificado              ",
                                  "Influenza debida a virus de la influenza identificado",
                                  "Influenza debida a virus no identificado          ")
  print(keywords_options_Influenza[i_Influenza])
  
  if(i_Influenza %in% c(1,2,4)) keyword <-  keywords_options_Influenza[i_Influenza] else if(i_Influenza ==3 ){
    keyword_1 <- "Influenza debida a virus de la influenza"
    keyword_2 <- "identificado"
  }
  colnames_m_enfermedad <- c("Año","Total","Tasa","<1","1-4","5-9","10 - 14","15 - 19","20 - 24","25 - 44","45 - 49","50 - 59","60 - 64","65 y +","Ign.")
  
  # m_enfermedad <- matrix(NA,length(anhos),length(colnames_m_enfermedad))
  m_enfermedad <- matrix(0,length(anhos),length(colnames_m_enfermedad))
  
  m_enfermedad[,1] <- anhos
  colnames(m_enfermedad) <- colnames_m_enfermedad
  
  # Extra variable for testing
  print_line_year = T
  print_line_year = F
  i <- 1
  
  for(i in 1:nrow(m_enfermedad)) {
    i
    (anho <- anhos[i])
    if(i_Influenza %in% c(1,2,4)) text_one_line <- extract_one_line(anho,keyword,l_PDF) else if(i_Influenza == 3){
      text_one_line <- extract_between_lines(anho,keyword_1,keyword_2,l_PDF)
    }
    
    if(print_line_year) {
      cat("Year", anho,"\n")
      cat(text_one_line,"\n")
      cat(length(text_one_line),"\n") # 16 = l_PDF$ncol_m_enfermedad + 1 
    }
    m_enfermedad[i,-1] <- text_one_line[-1:-2]
  }
  
  m_enfermedad <- correcting_columns_m_enfermedad(m_enfermedad)
  return(m_enfermedad)
}


#' Adding matrix of Influenza data
#'
#' @param i_Influenza_vec a vector with the columns to sum and generate the value
#' of the influenza data with values in 1:4
#' @param anhos 
#' @param l_PDF 
#'
#' @return
#' @export
#'
#' @examples
#' cat("\014") # borra consola 
#' rm(list=ls())  # Borra todo
#' source("Fn_epidemiologic.R")
#' load(file="Variables_Vaccine.RData")
#' 
#' # anhos_elegidos <- 2005:2019
#' anhos <- 2005:2019
#' i_Influenza_vec <- 1:4
#' i_Influenza_vec <- 1
#' (m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))
#' i_Influenza_vec <- 2
#' (m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))
#' 
#' i_Influenza_vec <- c(1,2)
#' (m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))
#' 
ADD_GEN_m_enfermedad_INFLUENZA <- function(i_Influenza_vec,anhos,l_PDF) {
  if(length(i_Influenza_vec)==1) return(GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF))
  m_enfermedad <- GEN_m_enfermedad_INFLUENZA(i_Influenza_vec[1],anhos,l_PDF)
  
  m_enfermedad[is.na(m_enfermedad)] <- 0
  for(i_Influenza in i_Influenza_vec[-1]) {
    m_enfermedad_to_add <- GEN_m_enfermedad_INFLUENZA(i_Influenza,anhos,l_PDF)
    
    m_enfermedad_to_add[is.na(m_enfermedad_to_add)] <- 0
    
    m_enfermedad <- m_enfermedad + m_enfermedad_to_add
  }
  m_enfermedad[,1] <- anhos
  
  m_enfermedad <- correction_decimal_year_Ncases(m_enfermedad)
  
  return(m_enfermedad)
}


#' Special paste of Meningitis Tuberculosa
#' 
#' This disease has the problem of having to names: 
#' 2019-2017 Meningitis tuberculosa
#' 2016-2004, 2002 Tuberculosis meníngea
#' 2003 Tuberculosis men¡nge (ie. mal escrita)
#'
#' OBS: THIS FUNCTION IS RESTRICTED TO WORK ONLY WITH THE YEARS
#' anhos <- 2003:2019
#' @param anhos 
#' @param l_PDF 
#'
#' @return
#' @export
#'
#' @examples
ADD_GEN_m_enfermedad_Only_Meningitis <- function(anhos,l_PDF) {
  
  cat("entro a ADD_GEN_m_enfermedad_Only_Meningitis\n")
  anhos <- 2003:2019 
  # There are three different names
  keyword1 <- "Tuberculosis men¡nge         "
  (m_enfermedad_name_2003 <- GEN_m_enfermedad(keyword1,anhos,l_PDF))
  
  keyword2 <- "Tuberculosis meníngea         "
  (m_enfermedad_name_MT <- GEN_m_enfermedad(keyword2,anhos,l_PDF))
  
  keyword3 <- "Meningitis tuberculosa        "
  (m_enfermedad_name_TM <- GEN_m_enfermedad(keyword3,anhos,l_PDF))
  
  # We paste the results
  (m_enfermedad2 <- rbind(m_enfermedad_name_2003[1,],
                          m_enfermedad_name_MT[2:14,], 
                          m_enfermedad_name_TM[15:17,]))
  # Clean the table
  # m_enfermedad <- correction_decimal_year_Ncases(m_enfermedad)
  return(m_enfermedad2)
}

#' Errasing the extra zeros for year and number of cases in the table 
#'
#' @param m_enfermedad 
#'
#' @return
#' @export
#'
#' @examples
correction_decimal_year_Ncases <- function(m_enfermedad) {
  col_without_decimals <- 1:2
  m_enfermedad[col_without_decimals] <- floor(m_enfermedad[col_without_decimals])
  return(m_enfermedad)
}


#' Adding the extracction of influenza to the genaral tables
#'
#' @param keyword 
#' @param i_Influenza_vec a vector with the columns to sum and generate the value
#' of the influenza data with values in 1:4
#' @param anhos 
#' @param l_PDF 
#'
#' @return
#' @export
#'
#' @examples
#' cat("\014") # borra consola 
#' rm(list=ls())  # Borra todo
#' source("Fn_epidemiologic.R")
#' load(file="Variables_Vaccine.RData")
#' m_enfermedad <- -1
#' (three_years <- sort(sample(anhos,3,replace = F)))
#' (keyword <- sub_keywords[2])
#' i_Influenza_vec <- 1:4
#' i_Influenza_vec <- 1:2
#' GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
GEN_m_enfermedad_w_INFLUENZA <- function(keyword,i_Influenza_vec,anhos,l_PDF) {
  if(keyword == "Influenza                     ") {
    m_enfermedad <- ADD_GEN_m_enfermedad_INFLUENZA(i_Influenza_vec,anhos,l_PDF)
    m_enfermedad <- correction_decimal_year_Ncases(m_enfermedad)
  } else if(keyword == "Meningitis tuberculosa        ") {
    m_enfermedad <- ADD_GEN_m_enfermedad_Only_Meningitis(anhos,l_PDF) 
  } else {
    m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
    m_enfermedad <- correction_decimal_year_Ncases(m_enfermedad)
  }
  
  return(m_enfermedad)
}




#' Title
#'
#' @param datos_anho 
#' @param vec_partition_bars 
#'
#' @return
#' @export
#'
#' @examples
#' cat("\014") # borra consola 
#' rm(list=ls())  # Borra todo
#' source("Fn_epidemiologic.R")
#' load(file="Variables_Vaccine.RData")
#' 
#' # set.seed(42)
#' (three_years <- sort(sample(anhos,3,replace = F)))
#' 
#' three_years <- c(2006,2010,2019)
#' (keyword <- sub_keywords[2])
#' 
#' m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
#' m_enfermedad
#' i <- 1
#' year <- three_years[i]
#' (datos_anho <- m_enfermedad[m_enfermedad[,1]==year,4:(ncol(m_enfermedad)-1)])
#' 
#' 
#' vec_partition_bars <- 1:11
#' vec_partition_bars <- c(1:3, 7:8,10:11)
#' vec_partition_bars <- c(1:8,10:11)
#' 
#' length(datos_anho)  
my_barplot_age_blocks <- function(datos_anho,year,vec_partition_bars) {
  
  age_labels <- c("00-01",
                  "01-04",
                  "05-09",
                  "10-14",
                  "15-19",
                  "20-24",
                  "25-44",
                  "45-49",
                  "50-59",
                  "60-64",
                  "65-99")
  
  
  j <- 1
  new_datos_anho <- datos_anho[1]
  new_age_labels <- age_labels[1]
  for(j in 2:length(datos_anho)) {
    if(j %in% vec_partition_bars) {
      new_datos_anho <- c(new_datos_anho,datos_anho[j]) 
      new_age_labels <- c(new_age_labels,age_labels[j])
    } else {
      new_datos_anho[length(new_datos_anho)] <- new_datos_anho[length(new_datos_anho)] + datos_anho[j]
      new_age_labels[length(new_age_labels)] <- paste0(c(substr(new_age_labels[length(new_age_labels)],1,3),
                                                         substr(age_labels[j],4,5)),collapse = "")
    }
  }
  barplot(new_datos_anho, main=year,names.arg = new_age_labels,xlab = "",ylab="")
}


#' Extraction of the data matrix with percentages
#'
#' @param keyword 
#' @param anhos 
#' @param l_PDF 
#' @param i_Influenza_vec a vector with the columns to sum and generate the value
#' of the influenza data with values in 1:4
#'
#' @return
#' @export
#'
#' @examples
extrae_m_enfer_per <- function(keyword,i_Influenza_vec,anhos,l_PDF) {
  
  # m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
  m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
  
  col_to_percent = 4:14
  percent_mat = m_enfermedad[,col_to_percent]
  decimal_points = 1
  percent_mat = round(percent_mat/rowSums(percent_mat)*100,decimal_points)
  m_enfer_per = m_enfermedad
  m_enfer_per[,col_to_percent] = percent_mat
  
  
  m_enfer_per <- correction_decimal_year_Ncases(m_enfer_per)
  
  return(m_enfer_per)
}


#' Plot of the four figures: total number of cases and bar plots of percentages
#'
#' OBS: This function also saves the plots into the folder called 'Figures'
#'
#' @param idx_keyword value between 1 and 9 that represents the disease that will be plot
#'
#' @return
#' @export
#'
#' @examples
Plot_line_and_bars <- function(idx_keyword) {
  keyword <- sub_keywords[idx_keyword]
  
  # [1] "Difteria                      " "Influenza                     "
  # [3] "Meningitis tuberculosa        " "Parotiditis infecciosa        "
  # [5] "Rubéola                       " "Sarampión                     "
  # [7] "Tétanos                       " "Tos ferina                    "
  # [9] "Tuberculosis respiratoria     "
  # 10] "Poliomielitis aguda           "
  # 
  # 
  # 1Difteria Diphtheria 
  # 2Influenza Influenza 
  # 3Meningitis tuberculosa  es pulmonary tuberculosis
  # 4Paperas Mumps 
  # 5Rubeola Rubella
  # 6Sarampion Measles
  # 7Tetanus Tetanus
  # 8Tosferina Whooping cough 
  # 9 Pulmonary tuberculosis
  
  name_disease <- c("Diphtheria",  # 1
                    "Influenza", # 2
                    "Meningeal tuberculosis", # 3
                    "Mumps", # 4
                    "Rubella", # 5
                    "Measles", # 6
                    "Tetanus", # 7
                    "Whooping cough", # 8
                    "Pulmonary tuberculosis")[idx_keyword] # 9
  
  cat("Before ",keyword, ", now=",name_disease,"\n")
  
  # name_disease <- str_replace_all(string=keyword, pattern="  ", repl="")
  # mat_years_disease controls the years for the bar plots
  mat_years_disease <- matrix(c(2007,2012,2019, # 1 Difteria FALTA 
                                2009,2014,2019, # 2 Influenza FALTA
                                2008,2014,2019, # 3 Meningitis tuberculosa FALTA
                                2005,2017,2018, # 4 Parotiditis infecciosa antes 2005,2018,2019,
                                2005,2007,2010, # 5 Rubéola  
                                2005,2006,2010, # 6 Sarampión WITHOUT YEARS / 
                                2005,2010,2019, # 7 Tétanos 
                                2009,2012,2015, # 8 Tos ferina   
                                2005,2010,2019),# 9 Tuberculosis respiratoria
                              length(sub_keywords),3,byrow = T) 
  
  
  three_years <- mat_years_disease[idx_keyword,]
  # vec_partition_bars gives the division of the ages for
  # the bar plots
  vec_partition_bars = c(1,2,3,5,7)
  vec_partition_bars = 1:12
  # mat_how_to_plot is the dispossition of the plots in 
  # the figure
  mat_how_to_plot <- matrix(c(1,1,2,
                              1,1,3,
                              1,1,4),3,3,byrow = T)
  
  
  mat_how_to_plot <- matrix(c(1,1,2,2,
                              1,1,3,3,
                              1,1,4,4),3,4,byrow = T)
  
  mat_how_to_plot <- matrix(c(1,1,2,2,2,
                              1,1,3,3,3,
                              1,1,4,4,4),3,5,byrow = T)
  
  
  
  # we stablish the dispossition of the plots in the figure
  layout(mat_how_to_plot)
  i_Influenza_vec <- 1:2
  m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
  
  # print(m_enfermedad)
  # grid(m_enfermedad[,1],seq(0,10000,by = 1000))
  plot(m_enfermedad[,1],m_enfermedad[,2],type = "b",pch=18, main = name_disease,
       ylab = "Number of cases",xlab = "Year")
  
  for(i in 1:3) {
    year <- three_years[i]
    datos_anho <- m_enfermedad[m_enfermedad[,1]==year,4:(ncol(m_enfermedad)-1)]
    
    my_barplot_age_blocks(datos_anho,year,vec_partition_bars) 
    # barplot(datos_anho, main=year)
  }
  # plot_3years_barplot(three_years,m_enfermedad)
  
  file_pdf <- paste0(c("Figures/",name_disease,".pdf"),collapse = "")
  cat("The file ",file_pdf," is saved.\n")
  file_pdf
  dev.print(pdf,file_pdf,width=10, height=6)
}


#' Generation of all the figures 
#'
#' @return
#' @export
#'
#' @examples
Gen_ALL_Plot_line_and_bars <- function() {
  
  
  for(idx_keyword in 1:length(sub_keywords)){
    Plot_line_and_bars(idx_keyword) 
  }  
}
