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

IED.0  <- read.csv(file = "2_Tabelas_IBGE/Domicilio01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
IED.1  <- read.csv(file = "2_Tabelas_IBGE/Entorno03_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela

SC.RJ <- readOGR("6_Shapefile/6_CHID.shp", encoding = "UTF-8") # chamar os setores do SP

#------------------------------------------------------------------------------------------------------#

# selecionar colunas específicas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"
HE6 <- as.data.frame(sapply(subset(IED.1, select = c(Cod_setor, V429,V431,V433)), as.numeric))
HE7 <- as.data.frame(sapply(subset(IED.1, select = c(Cod_setor, V435,V437,V439)), as.numeric))
HE8 <- as.data.frame(sapply(subset(IED.1, select = c(Cod_setor, V453,V455,V457)), as.numeric))
HE9 <- as.data.frame(sapply(subset(IED.1, select = c(Cod_setor, V465,V467,V469)), as.numeric))
HE10 <- as.data.frame(sapply(subset(IED.1, select = c(Cod_setor, V459,V461,V463)), as.numeric))
HE11 <- as.data.frame(sapply(subset(IED.0, select = c(Cod_setor, V035)), as.numeric))

#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
HE6 <- HE6[!(rowSums(is.na(HE6))),]
HE7 <- HE7[!(rowSums(is.na(HE7))),]
HE8 <- HE8[!(rowSums(is.na(HE8))),]
HE9 <- HE9[!(rowSums(is.na(HE9))),]
HE10 <- HE10[!(rowSums(is.na(HE10))),]
HE11 <- HE11[!(rowSums(is.na(HE11))),]

#------------------------------------------------------------------------------------------------------#

HE6$Cod_mun <- str_sub(HE6$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE7$Cod_mun <- str_sub(HE7$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE8$Cod_mun <- str_sub(HE8$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE9$Cod_mun <- str_sub(HE9$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE10$Cod_mun <- str_sub(HE10$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE11$Cod_mun <- str_sub(HE11$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Criação de uma coluna com a soma das linhas de cada 
HE6$sum_HE6 <- apply(HE6[, 2:4], 1, sum) 
HE7$sum_HE7 <- apply(HE7[, 2:4], 1, sum)
HE8$sum_HE8 <- apply(HE8[, 2:4], 1, sum) 
HE9$sum_HE9 <- apply(HE9[, 2:4], 1, sum)
HE10$sum_HE10 <- apply(HE10[, 2:4], 1, sum) 

names(HE11) <- c("Cod_setor", "sum_HE11", "Cod_mun")


# Criação das colunas utilizadas para o Join

HE6 <- HE6[,-c(2:4)]
HE7 <- HE7[,-c(2:4)] 
HE8 <- HE8[,-c(2:4)]
HE9 <- HE9[,-c(2:4)] 
HE10 <- HE10[,-c(2:4)]


#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = HE6, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE7, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE8, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE9, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE10, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE11, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(24,26,28,30,32,35)]

#------------------------------------------------------------------------------------------------------#

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml


# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/7_IED.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/7_IED.kml", "IED", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/7_IED.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
