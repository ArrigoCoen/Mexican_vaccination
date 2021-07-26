

# astuhae -----------------------------------------------------------------


install.packages("pdftools")
library("pdftools")
# setwd("/home/arrigo/Desktop/Vaccine 4 plots")


# atheu -------------------------------------------------------------------
dev.off()
# ths ---------------------------------------------------------------------

# Line plot ---------------------------------------------------------------


cat("\014") # borra consola 
rm(list=ls())  # Borra todo
source("Fn_epidemiologic.R")
load(file="Variables_Vaccine.RData")

library("stringr")


idx_keyword <- 1


# Plot_line_and_bars(idx_keyword)


Gen_ALL_Plot_line_and_bars()
# Save all plots ----------------------------------------------------------


# -------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
df <- ToothGrowth %>%
  group_by(dose) %>%
  summarise(len.mean = mean(len))

ggplot(data = df, aes(x = dose, y = len.mean, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point()

# asote -------------------------------------------------------------------


source("Fn_epidemiologic.R")
# set.seed(42)
(three_years <- sort(sample(anhos,3,replace = F)))
(keyword <- sub_keywords[1])



# vec_partition_bars=c(1,2,4,6)
m_enfermedad <- GEN_m_enfermedad(keyword,anhos,l_PDF)
m_enfermedad
plot_3years_barplot(three_years,m_enfermedad)


# tsua --------------------------------------------------------------------

age_labels <- c("00-01", # 1
                "01-04", # 2
                "05-09", # 3
                "10-14", # 4
                "15-19", # 5
                "20-24", # 6
                "25-44", # 7
                "45-49", # 8
                "50-59", # 9
                "60-64", # 10
                "65-99") # 11
vec_partition_bars=c(1,2,3,5,7)
my_barplot_age_blocks(datos_anho,year,vec_partition_bars) 


# asteouh -----------------------------------------------------------------

i <- 1
par(mfrow=c(3,1))
for(i in 1:3) {
  year <- three_years[i]
  datos_anho <- m_enfermedad[m_enfermedad[,1]==year,4:(ncol(m_enfermedad)-1)]
  
  my_barplot_age_blocks(datos_anho,year,vec_partition_bars) 
  # barplot(datos_anho, main=year)
}
par(mfrow=c(1,1))
# astoe -------------------------------------------------------------------

set.seed(123)
x <- arima.sim(model = list(ar = 0.8), n = 50)

plot(arima.sim(model = list(ar = 0.8), n = 50))

# saoteuhh ----------------------------------------------------------------


# One figure in row 1 and two figures in row 2
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)



# st ----------------------------------------------------------------------


# One figure in row 1 and two figures in row 2
attach(mtcars)

mat_how_to_plot <- matrix(c(1,1,2,
                            1,1,3,
                            1,1,4),3,3,byrow = T)

layout(mat_how_to_plot)

# hist(wt)

plot(arima.sim(model = list(ar = 0.8), n = 50))
hist(mpg,ylab="")
hist(disp,ylab="")
hist(disp,ylab="")


# atheu -------------------------------------------------------------------
dev.off()
# ths ---------------------------------------------------------------------



# One figure in row 1 and two figures in row 2
# row 1 is 1/3 the height of row 2
# column 2 is 1/4 the width of the column 1
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
       widths=c(3,1), heights=c(1,2))
hist(wt)
hist(mpg)
hist(disp)







# asoetu ------------------------------------------------------------------






