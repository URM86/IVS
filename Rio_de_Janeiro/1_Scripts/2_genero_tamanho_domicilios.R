#------------------------------------------------------------------------------------------------------#
# Autor: Ulises Rodrigo Magdalena                                                                      #                                                              
#------------------------------------------------------------------------------------------------------#

setwd(choose.dir()) # diretório de trabalho

dir() #conferindo a pasta

#------------------------------------------------------------------------------------------------------#

install.packages("stringr") # Pacote utilizado para a manipulação de "strings", ou seja, caracteres e palavras.
install.packages("readr")
install.packages("sp")
install.packages("rgdal")

#------------------------------------------------------------------------------------------------------#

require("sp")
require("stringr")
require("readr")
require("rgdal")

#------------------------------------------------------------------------------------------------------#

options(scipen=999) # função para desabilitar númeração científica

GTD.PIR.PRIA  <- read.csv(file = "2_Tabelas_IBGE/Responsavel02_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
GTD.PMR  <- read.csv(file = "2_Tabelas_IBGE/Responsavel01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
GTD.PD4M  <- read.csv(file = "2_Tabelas_IBGE/Domicilio01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela

SC.RJ <- readOGR("6_Shapefile/1_EF.shp", encoding = "UTF-8") # chamar os setores do SP

#------------------------------------------------------------------------------------------------------#

# selecionar colunas específicas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"
EF3 <- as.data.frame(sapply(subset(GTD.PIR.PRIA, select = c(Cod_setor, V052:V092)), as.numeric))
EF4 <- as.data.frame(sapply(subset(GTD.PMR, select = c(Cod_setor, V001)), as.numeric))
EF5 <- as.data.frame(sapply(subset(GTD.PIR.PRIA, select = c(Cod_setor, V006:V057)), as.numeric))
EF6 <- as.data.frame(sapply(subset(GTD.PD4M, select = c(Cod_setor, V053:V059)), as.numeric))

#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
EF3 <- EF3[!(rowSums(is.na(EF3))),]
EF4 <- EF4[!(rowSums(is.na(EF4))),]
EF5 <- EF5[!(rowSums(is.na(EF5))),]
EF6 <- EF6[!(rowSums(is.na(EF6))),]


#------------------------------------------------------------------------------------------------------#

EF3$Cod_mun <- str_sub(EF3$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
EF4$Cod_mun <- str_sub(EF4$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
EF5$Cod_mun <- str_sub(EF5$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
EF6$Cod_mun <- str_sub(EF6$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Criação de uma coluna com a soma das linhas de cada 
EF3$sum.EF3 <- apply(EF3[, 2:42], 1, sum) 
EF5$sum.EF5 <- apply(EF5[, 2:53], 1, sum) 
EF6$sum.EF6 <- apply(EF6[, 2:8], 1, sum)

names(EF4) <- c("Cod_setor", "sum.EF4", "Cod_mun")

# Criação das colunas utilizadas para o Join

EF3 <- EF3[,-c(2:42)]
EF5 <- EF5[,-c(2:53)]
EF6 <- EF6[,-c(2:8)]

#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = EF3, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = EF4, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = EF5, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = EF6, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(8,11,12,14)]

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml

# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/2_EF_GTD.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/2_EF_GTD.kml", "EF", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/2_EF_GTD.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
