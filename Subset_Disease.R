#-------------------------------------------------------------------------#
#                                                                         #
#                           Subset_Disease                                #
#                                                                         #
#-------------------------------------------------------------------------#

# V1 08 dic 2020

#-------------------------------------------------------------------------#
# OBSERVACIONES:

# 1. Only work with 1-Difteria, 2-Influenza, 3-Paratiditis, 4-Rubeola, 
#   5-Sarampion, 6-Tetanos, 7-Tos ferina, 8-Tuberculosis resp
#   in total 8.

# 2. sub_keywords contiene estas enfermedades

# 3. Influenza es compleja ya que la dividen en:
#       Influenza A(H1N1), 2009 identificado 
#       Influenza debida a virus de la influenza identificado 
#       Influenza debida a virus no identificado
#   por lo cual la dejaré para después

#
#-------------------------------------------------------------------------#


# ansteouh ----------------------------------------------------------------

l_PDF$Variable_explanation <- c(l_PDF$Variable_explanation,"sub_keywords: tiene a las enfermedades Difteria, Influenza, Paratiditis, Rubeola, Sarampion, Tetanos, Tos ferina, Tuberculosis resp")
# Saving needed variables -------------------------------------------------
keywords_options_Influenza 
# save(m_enfermedad,anhos,keyword,keywords,sub_keywords,l_PDF,keywords_options_Influenza,file="Variables_Vaccine.RData")
load(file="Variables_Vaccine.RData")


# Searching the 8 diseases ------------------------------------------------
names(l_PDF)

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# keywords = sort(keywords)


subset_disease <- c(28,75,104,110,111,120,124,130)
(sub_keywords <- keywords[subset_disease])


# Selecting 3 years for bar plot rates ------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[2])

m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad


plot_3years_barplot(three_years,m_enfermedad)





# Example bar plot --------------------------------------------------------

counts <- table(mtcars$gear)
barplot(datos_anho, main="Car Distribution",
        xlab="Number of Gears")



# Testing the diff shiny vr direct run in Influenza 2012 and 2013 ---------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo

source("Fn_epidemiologic.R")

load(file="Variables_Vaccine.RData")


(keyword <- sub_keywords[3])
m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad


# Legend the matplot  -----------------------------------------------------

cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")
(keyword <- sub_keywords[2])
i_Influenza_vec <- 1:2

m_enfermedad <- -1

m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
# We eliminate the values of columns c(-1:-3,-15) = Year,Tatal,Rate, Ign.
m_enfermedad_only_GROUPS <- m_enfermedad[,c(-1:-3,-15)]
years <- m_enfermedad[,1]
matplot(years,m_enfermedad_only_GROUPS,type = "l",ylab = "Todos los grupos de edad",
        xlab = "Año",main=paste0(c("Comportamiento de todos los grupos de edad por año de ",keyword),collapse = ""))

nn <- ncol(m_enfermedad_only_GROUPS)
legend("top", colnames(m_enfermedad_only_GROUPS),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
m_enfermedad_only_GROUPS


# Correction of 3 plot bars -----------------------------------------------

#' load(file="Variables_Vaccine.RData")
#' 
#' # set.seed(42)
#' (three_years <- sort(sample(anhos,3,replace = F)))
#' (keyword <- sub_keywords[3])
#' 
#' m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
#' m_enfermedad
#' plot_3years_barplot(three_years,m_enfermedad)
plot_3years_barplot <- function(three_years,m_enfermedad) {
  
  par(mfrow=c(3,1))
  for(i in 1:3) {
    year <- three_years[i]
    datos_anho <- m_enfermedad[m_enfermedad[,1]==year,4:(ncol(m_enfermedad)-1)]
    barplot(datos_anho, main=year)
  }
  par(mfrow=c(1,1))
}

# anstoeuh ----------------------------------------------------------------
cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")
(keyword <- sub_keywords[2])
i_Influenza_vec <- 1:4
m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
# We eliminate the values of columns c(-1:-3,-15) = Year,Tatal,Rate, Ign.
m_enfermedad_only_GROUPS <- m_enfermedad[,c(-1:-3,-15)]

years <- m_enfermedad[,1]
rownames(m_enfermedad_only_GROUPS) <- years


layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot(years,m_enfermedad_only_GROUPS,type = "l",ylab = "Número de casos",
        xlab = "Año",main=paste0(c("Comportamiento de todos los grupos de edad por año de ",keyword),collapse = ""))
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
# legend("center", colnames(daily.pnl),col=seq_len(nn),cex=0.8,fill=seq_len(nn))

nn <- ncol(m_enfermedad_only_GROUPS)
legend("top", colnames(m_enfermedad_only_GROUPS),col=seq_len(nn),cex=0.8,fill=seq_len(nn))

m_enfermedad_only_GROUPS


# asouh -------------------------------------------------------------------

# dummy data
set.seed(45)
df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                 variable=rep(paste0("category", 1:9), each=5))
# plot
ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable))

# aseoth ------------------------------------------------------------------
matplot(x = years, y = m_enfermedad_only_GROUPS, type = "l", main="Ridge regression", xlab="λ", ylab="Coefficient-value", log = "x")

nr = ncol(m_enfermedad_only_GROUPS)
legend("topright", colnames(m_enfermedad_only_GROUPS), col=seq_len(nr), cex=0.8, lty=seq_len(nr), lwd=2)

# saotuh ------------------------------------------------------------------




cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

anhos <- 2003:2019
keyword <- "Asma"
keyword <- "Hepatitis vírica A"
keyword <- ""
keyword <- ""
keyword <- ""
keyword <- "Tétanos"  # WORKS
keyword <- "Cólera"   # WORKS

keyword <- "Fiebre tifoidea"      # WORKS
keyword <- keywords[sample(1:length(keywords),size = 1)]
keyword <- "Difteria"

keyword <- l_PDF$keyword_Patty[8] # 8 Influenzaau

# -------------------------------------------------------------------------

