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

RPD  <- read.csv(file = "2_Tabelas_IBGE/DomicilioRenda_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
SC.RJ <- readOGR("6_Shapefile/3_RPR.shp", encoding = "UTF-8") # chamar os setores do RJ

#------------------------------------------------------------------------------------------------------#

# selecionar colunas específicas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"
RE3 <- as.data.frame(sapply(subset(RPD, select = c(Cod_setor, V014)), as.numeric))
RE4 <- as.data.frame(sapply(subset(RPD, select = c(Cod_setor, V005:V006)), as.numeric))

#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
RE3 <- RE3[!(rowSums(is.na(RE3))),]
RE4 <- RE4[!(rowSums(is.na(RE4))),]

#------------------------------------------------------------------------------------------------------#

RE3$Cod_mun <- str_sub(RE3$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
RE4$Cod_mun <- str_sub(RE4$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Criação de uma coluna com a soma das linhas de cada 
RE4$sum.RE4 <- apply(RE4[, 2:3], 1, sum) 

names(RE3) <- c("Cod_setor", "sum.RE3", "Cod_mun")

# Criação das colunas utilizadas para o Join

RE4 <- RE4[,-c(2:3)]

#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = RE3, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = RE4, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)


#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(15,16)]

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml

# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/4_RPD.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/4_RPD.kml", "RPD", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/4_RPD.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
