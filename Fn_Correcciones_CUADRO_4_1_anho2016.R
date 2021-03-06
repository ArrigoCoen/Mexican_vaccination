#--------------------------------------------------------
#-- fun_corrige_CUADRO_4_1_anho2016 --
#--------------------------------------------------------
#-- This function was generated by Fn_epidemiologic
#-- Created on: Wed Oct 21 12:06:01 2020
#-- Author = Arrigo Coen 



# Current expected number of columns is: 13


fun_corrige_CUADRO_CUADRO_4_1_anho2016 <- function(text_table,semana,anho) {
	if(semana==1 && anho==2016) {
		i <- 2;
		#Original:
		#Baja California    6    633    1 410 1 900    1    16    27    60  1 201    64 340    76 657    160 445
		x <- 'Baja California    6    633    1410   1900    1    16    27    60  1201    64340    76657    160445'
		text_table[i] <- x
		i <- 5;
		#Original:
		#Coahuila    12    970    2 201 5 561    8    19    28    44  2 003    66 769    79 241    141 451
		x <- 'Coahuila    12    970    2 201 5 561    8    19    28    44  2 003    66 769    79 241    141 451'
		text_table[i] <- x
		i <- 7;
		#Original:
		#Chiapas    215    7 191    13 130 20 205    10    312    425    888  1 876    80 468    99 084    164 805
		x <- 'Chiapas    215    7 191    13 130 20 205    10    312    425    888  1 876    80 468    99 084    164 805'
		text_table[i] <- x
		i <- 8;
		#Original:
		#Chihuahua    34    695    1 846 1 924    1    29    34    129  1 479    70 717    94 668    184 265
		x <- 'Chihuahua    34    695    1 846 1 924    1    29    34    129  1 479    70 717    94 668    184 265'
		text_table[i] <- x
	}
	if(semana==14 && anho==2016) {
		i <- 15;
		#Original:
		#México    57    153    253    415    1    15    39    23 11 939    56 102    65 001    142 638
		x <- 'México    57    153    253    415    1    15    39    23 11 939    56 102    65 001    142 638'
		text_table[i] <- x
	}
	if(semana==15 && anho==2016) {
		i <- 15;
		#Original:
		#México    34    163    278    454    1    15    40    25 14 964    63 023    73 064    154 541
		x <- 'México    34    163    278    454    1    15    40    25 14 964    63 023    73 064    154 541'
		text_table[i] <- x
	}
	if(semana==18 && anho==2016) {
		i <- 15;
		#Original:
		#México    36    211    354    570    2    25    54    32 14 918    85 074    99 329    192 849
		x <- 'México    36    211    354    570    2    25    54    32 14 918    85 074    99 329    192 849'
		text_table[i] <- x
	}
	if(semana==27 && anho==2016) {
		i <- 7;
		#Original:
		#Chiapas    479    4 092    7 579 10 412    20    117    180    434   4 194    40 203    50 421    99 663
		x <- 'Chiapas    479    4 092    7 579 10 412    20    117    180    434   4 194    40 203    50 421    99 663'
		text_table[i] <- x
		i <- 8;
		#Original:
		#Chihuahua    64    446    1 086 1 203    1    7    5    18   2 928    32 983    43 140    75 925
		x <- 'Chihuahua    64    446    1 086 1 203    1    7    5    18   2 928    32 983    43 140    75 925'
		text_table[i] <- x
	}
	if(semana==28 && anho==2016) {
		i <- 7;
		#Original:
		#Chiapas    350    4 227    7 797 10 834    14    119    192    448   3 305    41 652    52 291    104 031
		x <- 'Chiapas    350    4 227    7 797 10 834    14    119    192    448   3 305    41 652    52 291    104 031'
		text_table[i] <- x
		i <- 8;
		#Original:
		#Chihuahua    41    455    1 118 1 254    -    7    5    30   3 029    34 283    44 869    79 184
		x <- 'Chihuahua    41    455    1 118 1 254    -    7    5    30   3 029    34 283    44 869    79 184'
		text_table[i] <- x
	}
	if(semana==29 && anho==2016) {
		i <- 2;
		#Original:
		#Baja California    34    314    713 1 010    1    6    10    20  2 092    28 513    33 267    76 129
		x <- 'Baja California    34    314    713 1 010    1    6    10    20  2 092    28 513    33 267    76 129'
		text_table[i] <- x
		i <- 7;
		#Original:
		#Chiapas    471    4,462    8,214 11 223    12    125    198    458  3 866    43 969    54 903    107 969
		x <- 'Chiapas    471    4,462    8,214 11 223    12    125    198    458  3 866    43 969    54 903    107 969'
		text_table[i] <- x
		i <- 8;
		#Original:
		#Chihuahua    50    469    1,155 1 319    1    7    6    30  3 134    35 668    46 638    82 570
		x <- 'Chihuahua    50    469    1,155 1 319    1    7    6    30  3 134    35 668    46 638    82 570'
		text_table[i] <- x
		i <- 15;
		#Original:
		#México    40    380    652    1 105    -    40    69    99 11 249    156 410    186 015    340 374
		x <- 'México    40    380    652    1 105    -    40    69    99 11 249    156 410    186 015    340 374'
		text_table[i] <- x
	}
	if(semana==33 && anho==2016) {
		i <- 15;
		#Original:
		#México     32    424    735    1 254    -    42    69    102 10 091    174 162    207 224    382 900
		x <- 'México     32    424    735    1 254    -    42    69    102 10 091    174 162    207 224    382 900'
		text_table[i] <- x
	}
	if(semana==36 && anho==2016) {
		i <- 15;
		#Original:
		#México     38    473    805    1 359    3    45    76    117 10 370    187 894    223 320    415 869
		x <- 'México     38    473    805    1 359    3    45    76    117 10 370    187 894    223 320    415 869'
		text_table[i] <- x
	}
	if(semana==37 && anho==2016) {
		i <- 15;
		#Original:
		#México     53    497    834    1 408    2    46    77    119 10 005    192 608    228 646    428 037
		x <- 'México     53    497    834    1 408    2    46    77    119 10 005    192 608    228 646    428 037'
		text_table[i] <- x
	}
	if(semana==39 && anho==2016) {
		i <- 15;
		#Original:
		#México     28    509    865    1 474    2    48    81    126 10 446    201 690    239 085    450 799
		x <- 'México     28    509    865    1 474    2    48    81    126 10 446    201 690    239 085    450 799'
		text_table[i] <- x
	}
	if(semana==40 && anho==2016) {
		i <- 15;
		#Original:
		#México     26    524    876    1 525    2    49    82    127 10 026    206 443    244 463    462 494
		x <- 'México     26    524    876    1 525    2    49    82    127 10 026    206 443    244 463    462 494'
		text_table[i] <- x
	}
	if(semana==41 && anho==2016) {
		i <- 7;
		#Original:
		#Chiapas     329    6,112 11,241    15 884    3    173    257    610  2 718    60 219    74 787    146 543
		x <- 'Chiapas     329    6,112 11,241    15 884    3    173    257    610  2 718    60 219    74 787    146 543'
		text_table[i] <- x
		i <- 15;
		#Original:
		#México     28    534    894    1 565    2    50    83    127 10 447    211 400    250 054    474 169
		x <- 'México     28    534    894    1 565    2    50    83    127 10 447    211 400    250 054    474 169'
		text_table[i] <- x
	}
	if(semana==42 && anho==2016) {
		i <- 7;
		#Original:
		#Chiapas     366    6,256 11,464    16 345    1    173    258    629  3 075    61 651    76 439    149 905
		x <- 'Chiapas     366    6,256 11,464    16 345    1    173    258    629  3 075    61 651    76 439    149 905'
		text_table[i] <- x
		i <- 15;
		#Original:
		#México     28    546    910    1 637    9    54    88    129 10 412    216 307    255 723    486 437
		x <- 'México     28    546    910    1 637    9    54    88    129 10 412    216 307    255 723    486 437'
		text_table[i] <- x
	}
	if(semana==50 && anho==2016) {
		i <- 7;
		#Original:
		#Chiapas     304    7,203 13,065    19 618    3    200    300    707  2 113    70 565    86 967    173 537
		x <- 'Chiapas     304    7,203 13,065    19 618    3    200    300    707  2 113    70 565    86 967    173 537'
		text_table[i] <- x
	}
	if(semana==51 && anho==2016) {
		i <- 7;
		#Original:
		#Chiapas     223    7,307 13,198    19 918    3   201    302    719  1 836    71 410    88 014    175 894
		x <- 'Chiapas     223    7,307 13,198    19 918    3   201    302    719  1 836    71 410    88 014    175 894'
		text_table[i] <- x
	}
	return(text_table)
}
