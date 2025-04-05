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
install.packages("dplyr")
install.packages("raster")

#------------------------------------------------------------------------------------------------------#
require("raster")
require("sp")
require("stringr")
require("readr")
require("rgdal")
require("dplyr")

#------------------------------------------------------------------------------------------------------#

options(scipen=999) # função para desabilitar númeração científica

SC.RJ <- readOGR("7_Shapefile_Muriaé/suscetibilidade_geral.shp", encoding = "UTF-8") # chamar os setores do BA
View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

B <- SC.RJ@data

# removendo colunas que não serão utilizadas 
B<-B[,-c(3, 4)]

#------------------------------------------------------------------------------------------------------#

names(B) <- c("Cod_setor",	"tipo",	"municipio", "area.km²","prop_mata",
              "prop_HAND", "prop_imper", "CLAS_VUL" ,"prop_cultivo","prop_pastagem")

#------------------------------------------------------------------------------------------------------#

# Definir os valores nulos "NA" como zero. 
B[is.na(B)] <- 0

#------------------------------------------------------------------------------------------------------#
# Setores com os valores Rurais 
rural <- subset(B, tipo == "RURAL")

# Setores com os valores Urbanos 
urbano <- subset(B, tipo == "URBANO")

#------------------------------------------------------------------------------------------------------#
# Os municípios que serão utilizados.
q.rural <- rural %>% # data frame utilizado 
  dplyr::group_by(municipio) %>% # agrupar por munícipio
  dplyr::mutate(q_prop_mata = .bincode(as.numeric(prop_mata),
                                       breaks = quantile(as.numeric(prop_mata), probs =seq(from = 0, to = 1, by = .25)),
                                       include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_mata = dplyr::recode(q_prop_mata, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop_cultivo = .bincode(as.numeric(prop_cultivo),
                                          breaks = quantile(as.numeric(prop_cultivo), probs =seq(from = 0, to = 1, by = .25)),
                                          include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_cultivo = dplyr::recode(q_prop_cultivo, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop_pastagem = .bincode(as.numeric(prop_pastagem),
                                           breaks = quantile(as.numeric(prop_pastagem), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                           include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_pastagem = dplyr::recode(q_prop_pastagem, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop_HAND = .bincode(as.numeric(prop_HAND),
                                       breaks = quantile(as.numeric(prop_HAND), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                       include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_HAND = dplyr::recode(q_prop_HAND, "1" = 1, "2" = 2, "3" = 2, "4" = 3))

                
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

q.urbano <- urbano %>% # data frame utilizado 
  dplyr::group_by(municipio) %>% # agrupar por munícipio
  dplyr::mutate(q_prop_mata = .bincode(as.numeric(prop_mata),
                                       breaks = quantile(as.numeric(prop_mata), probs =seq(from = 0, to = 1, by = .25)),
                                       include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_mata = dplyr::recode(q_prop_mata, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop_imper = .bincode(as.numeric(prop_imper),
                                        breaks = quantile(as.numeric(prop_imper), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                        include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_imper = dplyr::recode(q_prop_imper, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop_HAND = .bincode(as.numeric(prop_HAND),
                                       breaks = quantile(as.numeric(prop_HAND), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                       include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop_HAND = dplyr::recode(q_prop_HAND, "1" = 1, "2" = 2, "3" = 2, "4" = 3))

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

# Calcular usos da terra associados
q.rural$Usos.Assoc <- apply(q.rural[, 11:13], 1, sum) 

# Calcular Suscetibilidade Rural  
q.rural$Susc <- apply(q.rural[, 14:15], 1, sum)

#------------------------------------------------------------------------------------------------------#
# Reclassificar Suscetibilidade Rural 

q.rural <- q.rural %>% 
  dplyr::mutate(Susc.n = case_when(
    Susc <= 6 ~1,
    Susc > 6 & Susc <= 9 ~2,
    Susc >= 10 ~ 3))

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

# Manipulação dos valores Urbanos para calcular a Sibilidade Urbana

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#


# Calcular ussos associados 
q.urbano$Usos.Assoc <- apply(q.urbano[, c(11:12)], 1, sum) 

# Calcular Sibilidade Urbana
q.urbano$Susc <- apply(q.urbano[, 13:14], 1, sum)

#------------------------------------------------------------------------------------------------------#
# Reclassificar Sibilidade Urbana
q.urbano <- q.urbano %>% 
  dplyr::mutate(Susc.n = case_when(
    Susc <= 5 ~1,
    Susc > 5 & Susc <= 6 ~2,
    Susc > 6 ~ 3))

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

#Juntar as tabelas para realizr o marge no shapefile. 
Susc <- rbind(q.urbano, q.rural)

# realizar a join da tabela
names(SC.RJ@data ) <- c("Cod_setor",	"tipo", "bairro", "distrito",	"municipio",
                        "area.km²", "prop_HAND", "prop_mata", "prop_imper","CLAS_VUL",
                        "prop_cultivo", "prop_pastagem")
View(SC.RJ@data)
SC.RJ@data <- SC.RJ@data[,-c(6:9, 11:12)] 

SC.RJ <- merge(x = SC.RJ, y = Susc, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE)

SC.RJ@data <- SC.RJ@data[,-c(6:8)] 
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

View(SC.RJ@data)
rm(B, q.rural, q.urbano, rural, urbano,S, Susc)

#------------------------------------------------------------------------------------------------------#

# Calcular a Vulnerabilidade Soeambiental
SC.RJ@data$VulSoeAm <- apply(SC.RJ@data[, c(10,18)], 1, sum) 

# Reclassificar Vulnerabilidade Soeambiental
SC.RJ@data <- SC.RJ@data %>% 
  dplyr::mutate(VulSoeAm.n = case_when(
    VulSoeAm <= 3 ~1,
    VulSoeAm > 3 & VulSoeAm <= 4 ~2,
    VulSoeAm > 4 ~ 3))


#------------------------------------------------------------------------------------------------------#
dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml


# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/9_Muriae.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/9_VulSoeAm.kml", "VulSoeAm", "SC.SP", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/9_Muriae.shp", "SC.SP", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
