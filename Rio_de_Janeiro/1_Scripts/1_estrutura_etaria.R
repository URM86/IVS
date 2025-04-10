#------------------------------------------------------------------------------------------------------#
# Autor: Ulises Rodrigo Magdalena                                                                      #                                                              
#------------------------------------------------------------------------------------------------------#

setwd(choose.dir()) # diret�rio de trabalho

dir() #conferindo a pasta

#------------------------------------------------------------------------------------------------------#

install.packages("stringr") # Pacote utilizado para a manipula��o de "strings", ou seja, caracteres e palavras.
install.packages("readr")
install.packages("sp")
install.packages("rgdal")

#------------------------------------------------------------------------------------------------------#

require("sp")
require("stringr")
require("readr")
require("rgdal")

#------------------------------------------------------------------------------------------------------#

options(scipen=999) # fun��o para desabilitar n�mera��o cient�fica

EE  <- read.csv(file = "2_Tabelas_IBGE/Pessoa13_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
SC.RJ <- readOGR("3_Shapefile_setor_RJ/rj_setores_censitarios.shp", encoding = "UTF-8") # chamar os setores do RJ

#------------------------------------------------------------------------------------------------------#

# selecionar colunas espec�ficas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"

EF1 <- as.data.frame(sapply(subset(EE, select = c(Cod_setor, V022, V035:V046)), as.numeric))
EF2 <- as.data.frame(sapply(subset(EE, select = c(Cod_setor, V094:V134)), as.numeric))

#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
EF1 <- EF1[!(rowSums(is.na(EF1))),]
EF2 <- EF2[!(rowSums(is.na(EF2))),]

#------------------------------------------------------------------------------------------------------#

EF1$Cod_mun <- str_sub(EF1$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
EF2$Cod_mun <- str_sub(EF2$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Cria��o de uma coluna com a soma das linhas de cada 

EF1$sum.EF1 <- apply(EF1[, 2:14], 1, sum) 
EF2$sum.EF2 <- apply(EF2[, 2:42], 1, sum)

# Remo��o das vari�veis que n�o ser�o mais utilizadas. 

EF1 <- EF1[,-c(2:14)]
EF2 <- EF2[,-c(2:42)]

#------------------------------------------------------------------------------------------------------#

# limpar a tabela do shapefile com os setores cencit�rio para fazer o marge
SC.RJ@data<-SC.RJ@data[,-c(1, 4, 6:8, 10, 12, 13)]

# renomear a tabela para fazer o marge 

names(SC.RJ@data) <- c("Cod_setor", "tipo", "bairro", "distrito", "municipio") 

#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = EF1, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = EF2, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(6,8)]
View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diret�rio para salvar a tabela
dir.create("5_KML") # criar diret�rio para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diret�rio para salvar o arquivo em Kml

# Salvar os dados processados. 
write_excel_csv(SC.RJ@data, path = "4_Table/1_EF.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/1_EF.kml", "EF", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/1_EF.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#