####  Estudo de Caso - Limpeza e Pré-Processamento dos Dados  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/2.Big-Data-Real-Time-Analytics-com-Python-e-Spark/2.Manipulacao_de_Dados_em_Python_com_Numpy")
getwd()

  
##  Replicando o Código do Curso Feito em Python  ##



## Importando Pacotes
library(readxl)         # carregar arquivos
library(dplyr)


#### Carregando dados
dados <- data.frame(read.csv2("datasets/dataset1.csv"))

str(dados)
dim(dados)


## Verificando Dados Ausentes
colSums(is.na(dados))
head(dados)
head(dados$issue_d)
head(dados$int_rate)



#  -> Diferentemente do Python e NumPy , o R carrega todos os dados como chr e assim não temos valores considerados ausentes e sim como chr ""



#### Repetindo o Processo do Estudo de Caso para diferenciar colunas do tipo string das do tipo numérica


## Verificando valores ausentes (aqui serão considerados dados como chr "" e caracteres especiais)

# Colunas com valores vazios (chr "")
sapply(dados, function(x) sum(x == ""))

# Colunas com caracteres especiais
sapply(dados, function(x) sum(grepl("[^\\x20-\\x7E]", x)))


## Início Tratamento Valores Ausentes

# Função ajustada para limpar colunas com critérios específicos (iremos adicionar valores NA para chr"" e coluna int_rate)
limpar_colunas <- function(coluna, nome_coluna) {
  coluna[coluna == ""] <- NA                                         # Substituir strings vazias por NA para todas as colunas
  if (nome_coluna == "int_rate" || nome_coluna == "installment") {   # Para a coluna int_rate, substituir strings com caracteres especiais por NA
    coluna[!grepl("[^\\x20-\\x7E]", coluna)] <- NA
  }
  return(coluna)
}

dados_limpos <- as.data.frame(mapply(limpar_colunas, dados, names(dados), SIMPLIFY = FALSE))
head(dados_limpos)
str(dados_limpos)


## Convertendo Colunas Para Tipo String e Numérico

colunas_para_converter <- c("id", "loan_amnt", "funded_amnt", "int_rate", "installment", "total_pymnt") # Lista das colunas para converter para numérico

# Loop pelas colunas especificadas e converter para numérico
for (coluna in colunas_para_converter) {
  dados_limpos[[coluna]] <- as.numeric(dados_limpos[[coluna]])
}

str(dados_limpos)



