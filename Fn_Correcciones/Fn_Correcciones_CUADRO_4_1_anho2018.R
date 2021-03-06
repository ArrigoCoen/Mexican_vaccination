#--------------------------------------------------------
#-- fun_corrige_CUADRO_4_1_anho2018 --
#--------------------------------------------------------
#-- This function was generated by Fn_epidemiologic
#-- Created on: Wed Oct 21 15:12:32 2020
#-- Author = Arrigo Coen 



# Current expected number of columns is: 13

# Number of weeks consider from 1 to 52

# Obs: The commented values are the original, and they are as a reference.

fun_corrige_CUADRO_4_1_anho2018 <- function(text_table,semana,anho) {
	if(semana==11 && anho==2018) {
		i <- 15;
		#México    25    95    157    355    4    10    12    61 10 254    41 675    47 560    130 147
		text_table[i] <- 'México    25    95    157    355    4    10    12    61   10254    41675    47560    130147'
	}
	if(semana==12 && anho==2018) {
		i <- 15;
		#México    35    106    181    392    1    11    12    64 10 248    46 542    53 095    143 866
		text_table[i] <- 'México    35    106    181    392    1    11    12    64   10248    46542    53095    143866'
	}
	if(semana==44 && anho==2018) {
		i <- 15;
		#México    18    539    803    1 508    1    62    86    132 10 553    221 003    255 697    541 173
		text_table[i] <- 'México    18    539    803    1 508    1    62    86    132   10553    221003    255697    541173'
	}
	if(semana==46 && anho==2018) {
		i <- 15;
		#México    20    552    839    1 567    1    66    87    135 10 110    230 184    265 940    559 662
		text_table[i] <- 'México    20    552    839    1567    1    66    87    135    10110    230184    265940    559662'
	}
	if(semana==48 && anho==2018) {
		i <- 7;
		#Chiapas     214    7 280    12 332    20 825    7   273    332    465    2 071    92 590    111 255 204 589
		text_table[i] <- 'Chiapas     214    7280    12332    20825    7   273    332    465    2071    92590    111255    204589'
	}
	return(text_table)
}
