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

# Adição de variáveis para fazer os cálculos de proporção
pessoa01  <- read.csv(file = "2_Tabelas_IBGE/Pessoa01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
domicilio02  <- read.csv(file = "2_Tabelas_IBGE/Domicilio02_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
responsavel02  <- read.csv(file = "2_Tabelas_IBGE/Responsavel02_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela
domicilio01  <- read.csv(file = "2_Tabelas_IBGE/Domicilio01_RJ.csv", header = T, dec = ".", sep = ";", stringsAsFactors = FALSE) # chamar a tabela

SC.RJ <- readOGR("6_Shapefile/7_IED.shp", encoding = "UTF-8") # chamar os setores do SP

#------------------------------------------------------------------------------------------------------#

# Selecionando a coluna que será utilizada no calcúlo de proporção.
pessoa01<-pessoa01[,-c(2, 4:88)]
domicilio02<-domicilio02[,-c(2, 4:134)] 
responsavel02<-responsavel02[,-c(2, 4:218)]
domicilio01<-domicilio01[,-c(2, 4:244)]
#------------------------------------------------------------------------------------------------------#

# renomeado as colunas para realizar a junção com a tabela no shapefile.
names(pessoa01) <- c("Cod_setor","pessoa01.V001")
names(domicilio02) <- c("Cod_setor","domicilio02.V001")
names(responsavel02) <- c("Cod_setor","responsavel02.V001")
names(domicilio01) <- c("Cod_setor","domicilio01.V001")
#------------------------------------------------------------------------------------------------------#

# remover os  valores nulos "NA" 
pessoa01 <- pessoa01[!(rowSums(is.na(pessoa01))),]
domicilio02 <- domicilio02[!(rowSums(is.na(domicilio02))),]
responsavel02 <- responsavel02[!(rowSums(is.na(responsavel02))),]
domicilio01 <- domicilio01[!(rowSums(is.na(domicilio01))),]

#------------------------------------------------------------------------------------------------------#
# Junção da tabelas com os dados presentes no shapefile.
SC.RJ <- merge(x = SC.RJ, y = pessoa01, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) 
SC.RJ <- merge(x = SC.RJ, y = domicilio02, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) 
SC.RJ <- merge(x = SC.RJ, y = responsavel02, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE) 
SC.RJ <- merge(x = SC.RJ, y = domicilio01, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE)

View(SC.RJ@data)

#------------------------------------------------------------------------------------------------------#

rm(domicilio01, domicilio02, pessoa01, responsavel02)

#------------------------------------------------------------------------------------------------------#

prop <- SC.RJ@data

#remover colunas 
prop <- prop[,-c(3, 4)]

# remoção dos valores "NA"
prop <- prop[!(rowSums(is.na(prop))),]

#------------------------------------------------------------------------------------------------------#
# Calcular a proporção Estrutura Familiar (EF)

prop$prop.EF1 <- prop$sum_EF1 / prop$domicilio02.V001 
prop$prop.EF2 <- prop$sum_EF2 / prop$domicilio02.V001
prop$prop.EF3 <- prop$sum_EF3 / prop$responsavel02.V001
prop$prop.EF4 <- prop$sum_EF4 / prop$responsavel02.V001
prop$prop.EF5 <- prop$sum_EF5 / prop$responsavel02.V001
prop$prop.EF6 <- prop$sum_EF6 / prop$domicilio01.V001

# Calcular a proporção Dimensão: Renda e Educação (RE)

prop$prop.RE1 <- prop$sum_RE1 / prop$responsavel02.V001
prop$prop.RE2 <- prop$sum_RE2 / prop$responsavel02.V001
prop$prop.RE3 <- prop$sum_RE3 / prop$domicilio01.V001
prop$prop.RE4 <- prop$sum_RE4 / prop$domicilio01.V001
prop$prop.RE5 <- prop$sum_RE5 / prop$pessoa01.V001
prop$prop.RE6 <- prop$sum_RE6 / prop$pessoa01.V001
prop$prop.RE7 <- prop$sum_RE7 / prop$pessoa01.V001

# Calcular a proporção Dimensão: Habitação e Entorno (HE)

prop$prop.HE1 <- prop$sum_HE1 / prop$domicilio02.V001
prop$prop.HE2 <- prop$sum_HE2 / prop$domicilio02.V001
prop$prop.HE3 <- prop$sum_HE3 / prop$domicilio02.V001
prop$prop.HE4 <- prop$sum_HE4 / prop$domicilio02.V001
prop$prop.HE5 <- prop$sum_HE5 / prop$domicilio02.V001
prop$prop.HE6 <- prop$sum_HE6 / prop$domicilio02.V001
prop$prop.HE7 <- prop$sum_HE7 / prop$domicilio02.V001
prop$prop.HE8 <- prop$sum_HE8 / prop$domicilio02.V001
prop$prop.HE9 <- prop$sum_HE9 / prop$domicilio02.V001
prop$prop.HE10 <- prop$sum_HE10 / prop$domicilio02.V001
prop$prop.HE11 <- prop$sum_HE11 / prop$domicilio02.V001

#------------------------------------------------------------------------------------------------------#
# Setores com os valores Rurais 
rural <- subset(prop, tipo == "RURAL")

# Setores com os valores Urbanos 
urbano <- subset(prop, tipo == "URBANO")

#------------------------------------------------------------------------------------------------------#
# Os municípios que serão utilizados.
q.rural <- rural %>% # data frame utilizado 
  dplyr::group_by(municipio) %>% # agrupar por munícipio
  dplyr::mutate(q_prop.EF1 = .bincode(as.numeric(prop.EF1),
                                        breaks = quantile(as.numeric(prop.EF1), probs =seq(from = 0, to = 1, by = .25)),
                                        include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF1 = dplyr::recode(q_prop.EF1, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF2 = .bincode(as.numeric(prop.EF2),
                                        breaks = quantile(as.numeric(prop.EF2), probs =seq(from = 0, to = 1, by = .25)),
                                        include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF2 = dplyr::recode(q_prop.EF2, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF3 = .bincode(as.numeric(prop.EF3),
                                      breaks = quantile(as.numeric(prop.EF3), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF3 = dplyr::recode(q_prop.EF3, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF4 = .bincode(as.numeric(prop.EF4),
                                      breaks = quantile(as.numeric(prop.EF4), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF4 = dplyr::recode(q_prop.EF4, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF5 = .bincode(as.numeric(prop.EF5),
                                      breaks = quantile(as.numeric(prop.EF5), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF5 = dplyr::recode(q_prop.EF5, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.EF6 = .bincode(as.numeric(prop.EF6),
                                      breaks = quantile(as.numeric(prop.EF6), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF6 = dplyr::recode(q_prop.EF6, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE1 = .bincode(as.numeric(prop.RE1),
                                      breaks = quantile(as.numeric(prop.RE1), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE1 = dplyr::recode(q_prop.RE1, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE2 = .bincode(as.numeric(prop.RE2),
                                      breaks = quantile(as.numeric(prop.RE2), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE2 = dplyr::recode(q_prop.RE2, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE3 = .bincode(as.numeric(prop.RE3),
                                      breaks = quantile(as.numeric(prop.RE3), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE3 = dplyr::recode(q_prop.RE3, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE4 = .bincode(as.numeric(prop.RE4),
                                      breaks = quantile(as.numeric(prop.RE4), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE4 = dplyr::recode(q_prop.RE4, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE5 = .bincode(as.numeric(prop.RE5),
                                      breaks = quantile(as.numeric(prop.RE5), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE5 = dplyr::recode(q_prop.RE5, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.RE6 = .bincode(as.numeric(prop.RE6),
                                      breaks = quantile(as.numeric(prop.RE6), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE6 = dplyr::recode(q_prop.RE6, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.RE7 = .bincode(as.numeric(prop.RE7),
                                      breaks = quantile(as.numeric(prop.RE7), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE7 = dplyr::recode(q_prop.RE7, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE1 = .bincode(as.numeric(prop.HE1),
                                      breaks = quantile(as.numeric(prop.HE1), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE1 = dplyr::recode(q_prop.HE1, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE2 = .bincode(as.numeric(prop.HE2),
                                      breaks = quantile(as.numeric(prop.HE2), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE2 = dplyr::recode(q_prop.HE2, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE3 = .bincode(as.numeric(prop.HE3),
                                      breaks = quantile(as.numeric(prop.HE3), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE3 = dplyr::recode(q_prop.HE3, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE4 = .bincode(as.numeric(prop.HE4),
                                      breaks = quantile(as.numeric(prop.HE4), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE4 = dplyr::recode(q_prop.HE4, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.HE5 = .bincode(as.numeric(prop.HE5),
                                      breaks = quantile(as.numeric(prop.HE5), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE5 = dplyr::recode(q_prop.HE5, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.HE9 = .bincode(as.numeric(prop.HE9),
                                      breaks = quantile(as.numeric(prop.HE9), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE9 = dplyr::recode(q_prop.HE9, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE11 = .bincode(as.numeric(prop.HE11),
                                      breaks = quantile(as.numeric(prop.HE11), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE11 = dplyr::recode(q_prop.HE11, "1" = 3, "2" = 2, "3" = 2, "4" = 1))
              
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

q.urbano <- urbano %>% # data frame utilizado 
  dplyr::group_by(municipio) %>% # agrupar por munícipio
  dplyr::mutate(q_prop.EF1 = .bincode(as.numeric(prop.EF1),
                                      breaks = quantile(as.numeric(prop.EF1), probs =seq(from = 0, to = 1, by = .25)),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF1 = dplyr::recode(q_prop.EF1, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF2 = .bincode(as.numeric(prop.EF2),
                                      breaks = quantile(as.numeric(prop.EF2), probs =seq(from = 0, to = 1, by = .25)),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF2 = dplyr::recode(q_prop.EF2, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF3 = .bincode(as.numeric(prop.EF3),
                                      breaks = quantile(as.numeric(prop.EF3), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF3 = dplyr::recode(q_prop.EF3, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF4 = .bincode(as.numeric(prop.EF4),
                                      breaks = quantile(as.numeric(prop.EF4), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF4 = dplyr::recode(q_prop.EF4, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.EF5 = .bincode(as.numeric(prop.EF5),
                                      breaks = quantile(as.numeric(prop.EF5), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF5 = dplyr::recode(q_prop.EF5, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.EF6 = .bincode(as.numeric(prop.EF6),
                                      breaks = quantile(as.numeric(prop.EF6), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.EF6 = dplyr::recode(q_prop.EF6, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE1 = .bincode(as.numeric(prop.RE1),
                                      breaks = quantile(as.numeric(prop.RE1), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE1 = dplyr::recode(q_prop.RE1, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE2 = .bincode(as.numeric(prop.RE2),
                                      breaks = quantile(as.numeric(prop.RE2), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE2 = dplyr::recode(q_prop.RE2, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE3 = .bincode(as.numeric(prop.RE3),
                                      breaks = quantile(as.numeric(prop.RE3), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE3 = dplyr::recode(q_prop.RE3, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE4 = .bincode(as.numeric(prop.RE4),
                                      breaks = quantile(as.numeric(prop.RE4), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE4 = dplyr::recode(q_prop.RE4, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.RE5 = .bincode(as.numeric(prop.RE5),
                                      breaks = quantile(as.numeric(prop.RE5), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE5 = dplyr::recode(q_prop.RE5, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.RE6 = .bincode(as.numeric(prop.RE6),
                                      breaks = quantile(as.numeric(prop.RE6), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE6 = dplyr::recode(q_prop.RE6, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.RE7 = .bincode(as.numeric(prop.RE7),
                                      breaks = quantile(as.numeric(prop.RE7), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.RE7 = dplyr::recode(q_prop.RE7, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE1 = .bincode(as.numeric(prop.HE1),
                                      breaks = quantile(as.numeric(prop.HE1), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE1 = dplyr::recode(q_prop.HE1, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE2 = .bincode(as.numeric(prop.HE2),
                                      breaks = quantile(as.numeric(prop.HE2), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE2 = dplyr::recode(q_prop.HE2, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE3 = .bincode(as.numeric(prop.HE3),
                                      breaks = quantile(as.numeric(prop.HE3), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE3 = dplyr::recode(q_prop.HE3, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE4 = .bincode(as.numeric(prop.HE4),
                                      breaks = quantile(as.numeric(prop.HE4), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE4 = dplyr::recode(q_prop.HE4, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.HE5 = .bincode(as.numeric(prop.HE5),
                                      breaks = quantile(as.numeric(prop.HE5), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE5 = dplyr::recode(q_prop.HE5, "1" = 1, "2" = 2, "3" = 2, "4" = 3),
                q_prop.HE6 = .bincode(as.numeric(prop.HE6),
                                      breaks = quantile(as.numeric(prop.HE6), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE6 = dplyr::recode(q_prop.HE6, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE7 = .bincode(as.numeric(prop.HE7),
                                      breaks = quantile(as.numeric(prop.HE7), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE7 = dplyr::recode(q_prop.HE7, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE8 = .bincode(as.numeric(prop.HE8),
                                      breaks = quantile(as.numeric(prop.HE8), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE8 = dplyr::recode(q_prop.HE8, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE9 = .bincode(as.numeric(prop.HE9),
                                      breaks = quantile(as.numeric(prop.HE9), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                      include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE9 = dplyr::recode(q_prop.HE9, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE10 = .bincode(as.numeric(prop.HE10),
                                       breaks = quantile(as.numeric(prop.HE10), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                       include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE10 = dplyr::recode(q_prop.HE10, "1" = 3, "2" = 2, "3" = 2, "4" = 1),
                q_prop.HE11 = .bincode(as.numeric(prop.HE11),
                                       breaks = quantile(as.numeric(prop.HE11), probs =seq(from = 0, to = 1, by = .25), na.rm = T),
                                       include.lowest = TRUE), #  criar e extrair o valor por quantil
                q_prop.HE11 = dplyr::recode(q_prop.HE11, "1" = 3, "2" = 2, "3" = 2, "4" = 1))

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
# remover colunas que não serão utilizadas 
q.rural <- q.rural[,-c(4:27)]
q.urbano <- q.urbano[,-c(4:27)]
#------------------------------------------------------------------------------------------------------#

# Calcular o EE
q.rural$EE <- apply(q.rural[, 32:33], 1, sum) 

# Calcular o GTD
q.rural$GTD <- apply(q.rural[, 34:37], 1, sum) 

# Calcular Renda 
q.rural$RENDA <- apply(q.rural[, 38:41], 1, sum)

# Calcular Educação
q.rural$EDUC <- apply(q.rural[, 42:44], 1, sum)

# Calcular Estrutura da Familia
q.rural$EEF <- apply(q.rural[, 52:53], 1, sum)

# Calcular Renda e Educação
q.rural$RENeEDUC <- apply(q.rural[, 54:55], 1, sum)

#------------------------------------------------------------------------------------------------------#
# Reclassificar Estrutura familair
q.rural <- q.rural %>% 
    dplyr::mutate(EEF.n = case_when(
    EEF <= 10 ~1,
    EEF > 10 & EEF <= 13 ~2,
    EEF > 13 ~ 3))
#------------------------------------------------------------------------------------------------------#
# Reclassificar Renda e Educação 
q.rural <- q.rural %>% 
  dplyr::mutate(RENeEDUC.n = case_when(
    RENeEDUC <= 12 ~1,
    RENeEDUC > 12 & RENeEDUC <= 15 ~2,
    RENeEDUC > 15 ~ 3))

#------------------------------------------------------------------------------------------------------#
# Calcular Pessoa
q.rural$PESSOA <- apply(q.rural[, 58:59], 1, sum)

# Reclassificar Pessoa 
q.rural <- q.rural %>% 
  dplyr::mutate(PESSOA.n = case_when(
    PESSOA <= 3 ~1,
    PESSOA > 3 & PESSOA <= 4 ~2,
    PESSOA > 4 ~ 3))

#------------------------------------------------------------------------------------------------------#

# calcular Infraestrutura Urbana Rural
q.rural$CHID <- apply(q.rural[, 45:49], 1, sum)

# calcular Infraestrutura Urbana Rural
q.rural$IED <- apply(q.rural[, c(50,51)], 1, sum)

# calcular Infraestrutura Urbana Rural
q.rural$IED.s <- apply(q.rural[, 62:63], 1, sum)

# Reclassificar IED.RURAL
q.rural <- q.rural %>% 
  dplyr::mutate(IED.s.n = case_when(
    IED.s <= 12 ~1,
    IED.s > 12 & IED.s <= 16 ~2,
    IED.s > 16 ~ 3))

#------------------------------------------------------------------------------------------------------#
# Vulnerabilidade Rural 
q.rural$VUL.S <- apply(q.rural[, c(61,65)], 1, sum)

#------------------------------------------------------------------------------------------------------#
# Reclassificar Vulnerabilidade Rural 

q.rural <- q.rural %>% 
  dplyr::mutate(CLAS_VUL_S = case_when(
    VUL.S <= 3 ~1,
    VUL.S > 3 & VUL.S <= 4 ~2,
    VUL.S > 4 ~ 3))

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

# Manipulação dos valores Urbanos para calcular a Vunerabilidade Urbana

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#


# Calcular o EE
q.urbano$EE <- apply(q.urbano[, 32:33], 1, sum) 

# Calcular o GTD
q.urbano$GTD <- apply(q.urbano[, 34:37], 1, sum) 

# Calcular Renda 
q.urbano$RENDA <- apply(q.urbano[, 38:41], 1, sum)

# Calcular Educação
q.urbano$EDUC <- apply(q.urbano[, 42:44], 1, sum)

# Calcular Estrutura da Familia
q.urbano$EEF <- apply(q.urbano[, 56:57], 1, sum)

# Calcular Renda e Educação
q.urbano$RENeEDUC <- apply(q.urbano[, 58:59], 1, sum)

#------------------------------------------------------------------------------------------------------#
# Reclassificar Estrutura familair
q.urbano <- q.urbano %>% 
  dplyr::mutate(EEF.n = case_when(
    EEF <= 10 ~1,
    EEF > 10 & EEF <= 13 ~2,
    EEF > 13 ~ 3))
#------------------------------------------------------------------------------------------------------#
# Reclassificar Renda e Educação 
q.urbano <- q.urbano %>% 
  dplyr::mutate(RENeEDUC.n = case_when(
    RENeEDUC <= 12 ~1,
    RENeEDUC > 12 & RENeEDUC <= 15 ~2,
    RENeEDUC > 15 ~ 3))

#------------------------------------------------------------------------------------------------------#
# Calcular Pessoa
q.urbano$PESSOA <- apply(q.urbano[, 62:63], 1, sum)

# Reclassificar Pessoa 
q.urbano <- q.urbano %>% 
  dplyr::mutate(PESSOA.n = case_when(
    PESSOA <= 3 ~1,
    PESSOA > 3 & PESSOA <= 4 ~2,
    PESSOA > 4 ~ 3))
#------------------------------------------------------------------------------------------------------#

# calcular Infraestrutura Urbana Rural
q.urbano$CHID <- apply(q.urbano[, 45:49], 1, sum)

# calcular Infraestrutura Urbana Rural
q.urbano$IED <- apply(q.urbano[, 50:55], 1, sum)

# calcular Infraestrutura Urbana Rural
q.urbano$IED.s <- apply(q.urbano[, 66:67], 1, sum)

# Reclassificar IED.RURAL
q.urbano <- q.urbano %>% 
  dplyr::mutate(IED.s.n = case_when(
    IED.s  <= 19 ~1,
    IED.s  > 19 & IED.s  <= 24 ~2,
    IED.s  > 24 ~ 3))

#------------------------------------------------------------------------------------------------------#
# Vulnerabilidade Rural 
q.urbano$VUL.S <- apply(q.urbano[, c(65,69)], 1, sum)

#------------------------------------------------------------------------------------------------------#
# Reclassificar Vulnerabilidade Rural 

q.urbano <- q.urbano %>% 
  dplyr::mutate(CLAS_VUL_S = case_when(
    VUL.S <= 3 ~1,
    VUL.S > 3 & VUL.S <= 4 ~2,
    VUL.S > 4 ~ 3))

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

#Juntar as tabelas para realizr o marge no shapefile. 
VUL <- rbind(q.urbano, q.rural)

VUL <- VUL[,-c(4:7)]

# realizar a join da tabela
SC.RJ <- merge(x = SC.RJ, y = VUL, by = "Cod_setor", by.x = "Cod_setor", by.y = "Cod_setor", all.x = TRUE)

SC.RJ@data <- SC.RJ@data[,-c(34,35)] 

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

View(SC.RJ@data)
summary(SC.RJ@data$CLAS_VUL_S)

#------------------------------------------------------------------------------------------------------#
dir.create("4_Table") # criar diretório para salvar a tabela
dir.create("5_KML") # criar diretório para salvar o arquivo em Kml
dir.create("6_Shapefile") # criar diretório para salvar o arquivo em Kml


# Salvar os dados processados. 
write_delim (SC.RJ@data, path = "4_Table/8_VUL.txt", delim = ";", col_names = T)
writeOGR(SC.RJ, "5_KML/8_VUL.kml", "IED", "SC.RJ", driver="KML")
writeOGR(SC.RJ, "6_Shapefile/8_VUL.shp", "SC.RJ", driver="ESRI Shapefile", overwrite_layer=TRUE)

#------------------------------------------------------------------------------------------------------#
# Fim
#------------------------------------------------------------------------------------------------------#
