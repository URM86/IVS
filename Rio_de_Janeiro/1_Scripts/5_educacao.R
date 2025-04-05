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

EA.0  <- read.csv(file = "2_Tabelas_IBGE/Pessoa01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
EA.1  <- read.csv(file = "2_Tabelas_IBGE/Responsavel01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela

SC.RJ <- readOGR("6_Shapefile/4_RPD.shp", encoding = "UTF-8") # chamar os setores do SP

#------------------------------------------------------------------------------------------------------#

# selecionar colunas específicas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"
RE5 <- as.data.frame(sapply(subset(EA.0, select = c(Cod_setor, V002:V015)), as.numeric))
RE6 <- as.data.frame(sapply(subset(EA.0, select = c(Cod_setor, V011:V062)), as.numeric))
RE7 <- as.data.frame(sapply(subset(EA.1, select = c(Cod_setor, V093)), as.numeric))

#------------------------------------------------------------------------------------------------------#

#remover os  valores nulos "NA" 
RE5 <- RE5[!(rowSums(is.na(RE5))),]
RE6 <- RE6[!(rowSums(is.na(RE6))),]
RE7 <- RE7[!(rowSums(is.na(RE7))),]

#------------------------------------------------------------------------------------------------------#

RE5$Cod_mun <- str_sub(RE5$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
RE6$Cod_mun <- str_sub(RE6$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
RE7$Cod_mun <- str_sub(RE7$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Criação de uma coluna com a soma das linhas de cada 
RE5$sum.RE5 <- apply(RE5[, 2:15], 1, sum) 
RE6$sum.RE6 <- apply(RE6[, 2:53], 1, sum)

names(RE7) <- c("Cod_setor", "sum.RE7", "Cod_mun")

# Criação das colunas utilizadas para o Join

RE5 <- RE5[,-c(2:15)]
RE6 <- RE6[,-c(2:53)] 

#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = RE5, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = RE6, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = RE7, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(16,18,21)]

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml

# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/5_EA.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/5_EA.kml", "EA", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/5_EA.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
