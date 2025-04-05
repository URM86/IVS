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

CHID.0  <- read.csv(file = "2_Tabelas_IBGE/Domicilio01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
CHID.1  <- read.csv(file = "2_Tabelas_IBGE/Domicilio02_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
CHID.2  <- read.csv(file = "2_Tabelas_IBGE/Entorno04_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela


SC.RJ <- readOGR("6_Shapefile/5_EA.shp", encoding = "UTF-8") # chamar os setores do SP

#------------------------------------------------------------------------------------------------------#

# selecionar colunas específicas do dataframe inicial, transformar os "X" em "NAs" para a soma de colnas e depois remove os "NA's"
HE1 <- as.data.frame(sapply(subset(CHID.1, select = c(Cod_setor, V012)), as.numeric))
HE2 <- as.data.frame(sapply(subset(CHID.1, select = c(Cod_setor, V017)), as.numeric))
HE3 <- as.data.frame(sapply(subset(CHID.1, select = c(Cod_setor, V043)), as.numeric))
HE4 <- as.data.frame(sapply(subset(CHID.0, select = c(Cod_setor, V008:V011)), as.numeric))
HE5 <- as.data.frame(sapply(subset(CHID.2, select = c(Cod_setor, V627:V628)), as.numeric))


#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
HE1 <- HE1[!(rowSums(is.na(HE1))),]
HE2 <- HE2[!(rowSums(is.na(HE2))),]
HE3 <- HE3[!(rowSums(is.na(HE3))),]
HE4 <- HE4[!(rowSums(is.na(HE4))),]
HE5 <- HE5[!(rowSums(is.na(HE5))),]


#------------------------------------------------------------------------------------------------------#

HE1$Cod_mun <- str_sub(HE1$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE2$Cod_mun <- str_sub(HE2$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE3$Cod_mun <- str_sub(HE3$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE4$Cod_mun <- str_sub(HE4$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres
HE5$Cod_mun <- str_sub(HE5$Cod_setor, end = 7) # seleciona apenas os sete primeiros caracteres

#------------------------------------------------------------------------------------------------------#

# Criação de uma coluna com a soma das linhas de cada 

HE4$sum.HE4 <- apply(HE4[, 2:5], 1, sum) 
HE5$sum.HE5 <- apply(HE5[, 2:3], 1, sum)


names(HE1) <- c("Cod_setor", "sum.HE1", "Cod_mun")
names(HE2) <- c("Cod_setor", "sum.HE2", "Cod_mun")
names(HE3) <- c("Cod_setor", "sum.HE3", "Cod_mun")

# Criação das colunas utilizadas para o Join

HE4 <- HE4[,-c(2:5)]
HE5 <- HE5[,-c(2:3)] 

#------------------------------------------------------------------------------------------------------#

SC.RJ <- merge(x = SC.RJ, y = HE1, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE2, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE3, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE4, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1
SC.RJ <- merge(x = SC.RJ, y = HE5, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) # marge com dados EF1

View(SC.RJ@data)


#------------------------------------------------------------------------------------------------------#
# limpar a tabela para exportar.

SC.RJ@data<-SC.RJ@data[,-c(20,22,24,25,27)]

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml

# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/6_CHID.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/6_CHID.kml", "CHID", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/6_CHID.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
