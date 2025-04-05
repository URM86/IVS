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

RPR  <- read.csv(file = "2_Tabelas_IBGE/ResponsavelRenda_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
SC.RJ <- readOGR("6_Shapefile/2_EF_GTD.shp", encoding = "UTF-8") # chamar os setores do SP

#------------------------------------------------------------------------------------------------------#

# selecionar colunas específicas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"
RE1 <- as.data.frame(sapply(subset(RPR, select = c(Cod_setor, V001:V002)), as.numeric))
RE2 <- as.data.frame(sapply(subset(RPR, select = c(Cod_setor, V054)), as.numeric))

#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
RE1 <- RE1[!(rowSums(is.na(RE1))),]
RE2 <- RE2[!(rowSums(is.na(RE2))),]

#------------------------------------------------------------------------------------------------------#

RE1$Cod_mun <- str_sub(RE1$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
RE2$Cod_mun <- str_sub(RE2$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Criação de uma coluna com a soma das linhas de cada 
RE1$sum.RE1 <- apply(RE1[, 2:3], 1, sum) 

names(RE2) <- c("Cod_setor", "sum.RE2", "Cod_mun")

# Criação das colunas utilizadas para o Join

RE1 <- RE1[,-c(2:3)]

#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = RE1, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = RE2, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(12,15)]

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml

# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/3_RPR.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/3_RPR.kml", "RPR", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/3_RPR.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
