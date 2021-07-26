#-------------------------------------------------------------------------#
#                                                                         #
#                           NOMBRE DE ARCHIVO                             #
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


library("varhandle")
library("stringi")
library("stringr")



#install.packages("")

# Carpeta del proyecto ----------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo



# Extract values csv ------------------------------------------------------

dat <- read.csv("Dat_sarampion.csv", stringsAsFactors = F, fileEncoding = "latin1") 

head(dat)

dat$Age


idx_with_meses <- grep(pattern ="meses",dat$Age) # encuentra una palabra en un texto

dat$Age[idx_with_meses] <- substr(dat$Age[idx_with_meses],1,2)

dat$Age <- as.numeric(dat$Age)
dat$Age[idx_with_meses] <- dat$Age[idx_with_meses]/12


hist(dat$Age)

vec_breaks_hist <- c(0,1,4,9,14,19,24,44,49,59,64,65,max(dat$Age))
hist(dat$Age,breaks = vec_breaks_hist)


# aontuh ------------------------------------------------------------------


"<1"=1,"1-4"=2,"5-9"=3,"10 - 14"=4,
"15 - 19"=5, "20 - 24"=6, "25 - 44"=7, 
"45 - 49"=8, "50 - 59"=9, "60 - 64"=10,
"65 y +"=11 )


# My data -----------------------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
pdf_name <- "Tabla_resumen_casos_confirmados_sarampion_2020.08.14.pdf"
PDF <- pdf_text(pdf_name) %>%
  readr::read_lines() #open the PDF inside your project folder
PDF

length(PDF)

cuantas_letras <- 150
for(i in 1:length(PDF)) cat(i," ",substr(PDF[i],1,cuantas_letras),"\n")


for(i in 1:90) cat(i," ",substr(PDF[i],1,cuantas_letras),"\n")

# aseotuh -----------------------------------------------------------------


keyword_1 <- "             NOTIFICANTE"
keyword_2 <- "             NOTIFICANTE"
n_col <- 15

# We get the information of the line between
text_vec <- PDF[idx_keyword_1+1]
# Some corrections
if(length(text_vec)==0) text_one_line <- rep(NA,l_PDF$ncol_m_enfermedad + 1) else {
  text_one_line <- correccion_espacios_en_lineas(text_vec)
}


# snotueha ----------------------------------------------------------------

i <- 179
i <- 5
i <- 9
(linea <- PDF[i])
length(linea)
s1 = unlist(strsplit(linea, split='  ', fixed=TRUE))
s1 <- s1[s1!=""]
s1
str_trim(s1)

col_mat_sarampion_largo <- c("No. CASO",
                             "MUNCIPIO NOTIFICANTE",
                             "ESTADO NOTIFICANTE",
                             "ESTADO DE RESIDENCIA",
                             "MUNICIPIO DE NOTIFICANTE",
                             "SEXO",
                             "EDAD",
                             "ANTECEDENTE VACUNAL",
                             "FECHA DE INICIO DEL EXANTEMA",
                             "DIAGNOSTICO SEROLOGICO",
                             "DIGNOSTICO MOLECULAR",
                             "GENOTIPO",
                             "LINAJE",
                             "CRITERIO DE CONFIRMACION",
                             "CLASIFICACIÓN FINAL DEL CASO" )

col_mat_sarampion <- c("No.",
                       "Estado Not",
                       "Municipio Not",
                       "Estado Res",
                       "Municipio Res",
                       "Sex",
                       "Age",
                       "Ant Vac",
                       "Date exantema",
                       "Diag Ser",
                       "Diag Mol",
                       "Genotipo",
                       "Linaje",
                       "Criterio",
                       "Clasificacion" )
num_total_casos_sarampion <- 196
mat_sarampion <- matrix(0,num_total_casos_sarampion,length(col_mat_sarampion))
colnames(mat_sarampion) <- col_mat_sarampion
mat_sarampion


# asohu -------------------------------------------------------------------


# saunoth -----------------------------------------------------------------

cat("\014") # borra consola 

vec_for <- 10
vec_for <- length(PDF)

for(i in 1:vec_for) {
  (linea <- PDF[i])
  if(linea=="") next
  s1 = unlist(strsplit(linea, split='  ', fixed=TRUE))
  s1 <- s1[s1!=""]
  (s1 <- str_trim(s1))
  
  if(s1[1]=="NOTIFICANTE") {
    cat("i=",i,"\n",s1,"\n")
    cat("Guardaremos este valor \n")
  }
  if(substr(s1[1],1,5)=="CASOS") {
    cat("i=",i,"\n",s1,"\n")
    cat("Guardaremos este valor \n")
  }
  
  
}
# saunoth -----------------------------------------------------------------

cat("\014") # borra consola 

vec_for <- 80
vec_for <- length(PDF)


i_mat_sarampion <- 1
i <- 1
i <- 9
i <- 12
vec_lineas <- NULL
for(i in 1:vec_for) {
  (linea <- PDF[i])
  # cat("i=",i,"\n",linea,"\n")
  
  # linea <- stri_trans_general(linea,"Latin-ASCII")
  linea <- rm_accent(linea) 
  
  if(linea=="") next
  
  
  s1 = unlist(strsplit(linea, split='  ', fixed=TRUE))
  s1 <- s1[s1!=" " & s1!=""]
  s1 <- s1[s1!=" "]
  (s1 <- str_trim(s1))
  
  # cat("i=",i,"\n",substr(s1,1,20),"\n length(s1)=",length(s1),"\n")
  
  
  
  if(s1[1]==as.character(i_mat_sarampion)) {
    # if(!grepl("^[A-Za-z]+$", s1[1], perl = T) ) {
    cat("i=",i,"\n","length(s1)=",length(s1),"\n")
    # cat(" -------------------Guardaremos este valor \n")
    cat(substr(s1[1:4],1,20),"\n")
    vec_lineas[i_mat_sarampion] <- linea
    mat_sarampion[i_mat_sarampion,1:length(s1)] <- s1
    i_mat_sarampion <- i_mat_sarampion+1
  }
}
mat_sarampion


# otueh -------------------------------------------------------------------

con<-file('Dat_sarampion.csv',encoding="UTF-8")
write.csv(file=con)

# sthao -------------------------------------------------------------------


for(i in 1:vec_for) {
  (linea <- PDF[i])
  cat(linea,"\n")
}
# SAT ---------------------------------------------------------------------


for(i in 1:90) cat(i," ",substr(PDF[i],1,cuantas_letras),"\n")

# SAT ---------------------------------------------------------------------

cuantas_letras <- 40
for(i in 7:63) cat(i," ",substr(PDF[i],1,cuantas_letras),"\n")


# satouh ------------------------------------------------------------------


i <- 1
(linea <- PDF[i])
# SAUTH -------------------------------------------------------------------


cuantas_letras <- 250


for(i in 1:10) cat(i," ",substr(PDF[i],1,cuantas_letras),"\n")

# osuntha -----------------------------------------------------------------


all_stat_lines[[i]] <- c(s1[1],gsub(' ', '', s1[-1]))





# aseotuh -----------------------------------------------------------------



# aseotuh -----------------------------------------------------------------



# aseotuh -----------------------------------------------------------------

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

# aseotuh -----------------------------------------------------------------


