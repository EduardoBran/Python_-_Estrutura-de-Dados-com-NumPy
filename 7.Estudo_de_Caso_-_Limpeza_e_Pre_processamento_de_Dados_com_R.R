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
#colSums(is.na(dados))
head(dados)
head(dados$issue_d)
head(dados$int_rate)


#  -> Diferentemente do Python e NumPy , o R carrega todos os dados como chr e assim não temos valores considerados ausentes e sim como chr ""


## Verificando valores ausentes (aqui serão considerados dados como chr "" e caracteres especiais)

sapply(dados, function(x) sum(x == ""))                     # Colunas com valores vazios (chr "")
sapply(dados, function(x) sum(grepl("[^\\x20-\\x7E]", x)))  # Colunas com caracteres especiais


## Início Tratamento Valores Ausentes e Caracter Especial (transformando valores ausentes e caracter especial em NA)

# Função ajustada para limpar colunas com critérios específicos (iremos adicionar valores NA para chr"" e colunas int_rate e installment)
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
rm(limpar_colunas)


## Transformando Colunas Para Numérico (e assim diferenciar dasquelas que serão do tipo factor)

# Lista das colunas para converter para numérico
colunas_para_converter <- c("id", "loan_amnt", "funded_amnt", "int_rate", "installment", "total_pymnt") 

# Loop pelas colunas especificadas e converter para numérico
for (coluna in colunas_para_converter) {
  dados_limpos[[coluna]] <- as.numeric(dados_limpos[[coluna]])
}
str(dados_limpos)
rm(colunas_para_converter, coluna)



#### Tratando Colunas do tipo String ('issue_d' 'loan_status' 'term' 'grade' 'sub_grade' 'verification_status' 'url' 'addr_state')


### Pré-Processamento da Variável issue_date com Label Encoding (valor ausente tratado - todo valor ausente considerado como 0)

# Modificando nome
# which(names(dados_limpos) == "issue_d")
names(dados_limpos)[2] <- "issue_date"

# Verificando valores únicos
unique(dados_limpos$issue_date)

# Remover "-15" de issue_date
dados_limpos$issue_date <- gsub("-15", "", dados_limpos$issue_date)

# Definindo o vetor associativo para mapear os nomes dos meses para números
meses_para_numeros <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6, Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)

# Loop usando sapply para substituir os nomes dos meses pelos números correspondentes
dados_limpos$issue_date <- sapply(dados_limpos$issue_date, function(x) {
  if (!is.na(x) && x %in% names(meses_para_numeros)) {
    meses_para_numeros[x]
  } else {
    0  # Atribuir 0 se não for um mês válido ou se for NA originalmente
  }
})

# Converter a coluna issue_date para factor
dados_limpos$issue_date <- as.factor(dados_limpos$issue_date)

# Verificar as mudanças
table(dados_limpos$issue_date, useNA = "ifany")
rm(meses_para_numeros)


## Pré-Processamento da Variável loan_status com Binarização (valor ausente tratado - todo valor ausente considerado como 0)

# Verificando valores únicos
unique(dados_limpos$loan_status)

# Modificar loan_status para 0 e 1 conforme as condições
dados_limpos$loan_status <-
  ifelse(dados_limpos$loan_status %in% c(NA, "Charged Off", "Default", "Late (16-30 days)"), 
      0, 
  ifelse(is.na(dados_limpos$loan_status), 
      NA, 
      1)
)


# Converter a coluna loan_status para factor
dados_limpos$loan_status <- as.factor(dados_limpos$loan_status)

# Verificar as mudanças
table(dados_limpos$loan_status, useNA = "ifany")



### Pré-Processamento da Variável term com Limpeza de String (valores ausentes tratados - substituindo valores NA pelo maior valor)

# Modificando nome
names(dados_limpos)[6] <- "term_months"

# Verificando valores únicos
unique(dados_limpos$term_months)

# Remover " months" de e "" no início de term_months
dados_limpos$term_months <- trimws(gsub(" months", "", dados_limpos$term_months))

# Substituir NA pelo maior valor ("60") em term_months
dados_limpos$term_months[is.na(dados_limpos$term_months)] <- "60"

# Converter a coluna term_months para factor
dados_limpos$term_months <- as.factor(dados_limpos$term_months)

# Verificar as mudanças
table(dados_limpos$term_months, useNA = "ifany")



### Pré-Processamento das Variáveis grade e sub_grade trasnformando em tipo factor (variaveis ausentes tratadas)

# Verificando valores únicos
unique(dados_limpos$grade)
unique(dados_limpos$sub_grade)

# - Analisando a variável foi constatado que elas possuem uma relação, com sub_grade como uma espécia de subcategoria de grade
#   Não podemos e não vamos manter duas variáveis com mesmas informações. Vamos manter apenas sub_grade pois temos mais informações.


# Loop para ajustar a variável sub_grade
# -> O loop identifica casos em que a variável sub_grade está vazia ('') e a variável grade correspondente tem um valor.
#    Nesses casos, atribui-se à sub_grade o valor da grade com um '5' adicionado ao final.

unique_grades <- na.omit(unique(dados_limpos$grade)) # gravando objeto com valores únicos da variável grade sem NA

for(grade in unique_grades) {
  i <- which(is.na(dados_limpos$sub_grade) & dados_limpos$grade == grade) # Índices onde sub_grade está vazia e grade corresponde ao valor atual do loop
  
  dados_limpos$sub_grade[i] <- paste0(grade, "5") # Atribuir a sub_grade o valor da grade com um '5' adicionado ao final para esses índices
}
rm(grade, unique_grades, i)

# Verificar se a substituição foi realizada corretamente
table(dados_limpos$sub_grade, useNA = "ifany")


# Substituir valores NA por 'H1' em sub_grade
dados_limpos$sub_grade[is.na(dados_limpos$sub_grade)] <- 'H1'

# Removendo Variável Grade
dados_limpos$grade <- NULL

# Verificar se a substituição foi realizada corretamente
table(dados_limpos$sub_grade, useNA = "ifany")
str(dados_limpos)

# Garantir que 'sub_grade' seja um fator e os níveis estejam na ordem correta (em python precisou criar um dicionário)
dados_limpos$sub_grade <- factor(dados_limpos$sub_grade, levels = c("A1", "A2", "A3", "A4", "A5", 
                                                                    "B1", "B2", "B3", "B4", "B5", 
                                                                    "C1", "C2", "C3", "C4", "C5", 
                                                                    "D1", "D2", "D3", "D4", "D5", 
                                                                    "E1", "E2", "E3", "E4", "E5", 
                                                                    "F1", "F2", "F3", "F4", "F5", 
                                                                    "G1", "G2", "G3", "G4", "G5", "H1"))

# Verificando Tipo dos Dados
str(dados_limpos)
summary(dados_limpos$sub_grade)



## Pré-Processamento da Variável verification_status com Binarização (valor ausente tratado - todo valor ausente considerado como 0)

# Verificando valores únicos antes da modificação
unique(dados_limpos$verification_status)

# Binarizar a variável verification_status corretamente
dados_limpos$verification_status <- 
  ifelse(dados_limpos$verification_status == "Not Verified" | is.na(dados_limpos$verification_status), 0, 1)

# Converter a coluna loan_status para factor
dados_limpos$verification_status <- as.factor(dados_limpos$verification_status)

# Verificar as mudanças aplicadas
table(dados_limpos$verification_status, useNA = "ifany")



### Pré-Processamento da Variável url com Extração de ID

# - Constado que o valor final da informação da variável URL é possivelmente o mesmo valor da variável ID (faremos a verificação)

head(dados_limpos$url, 3)
head(dados_limpos$id, 3)

# Extrair o ID da URL
dados_limpos$url <- gsub("https://www.lendingclub.com/browse/loanDetail.action\\?loan_id=", "", dados_limpos$url)

# Converter a coluna url para numérico para facilitar a comparação
dados_limpos$url <- as.numeric(dados_limpos$url)
head(dados_limpos$url, 3)

# Comparar os IDs extraídos da URL com a coluna id
identical(dados_limpos$url, dados_limpos$id)

# Removendo a Variável url pois tem as mesmas informações da variável id
dados_limpos$url <- NULL

# Tipo de Dados
str(dados_limpos)



### Pré-Processamento da Variável address com Categorização (dados ausentes ficaram com valor 0)

# Ajustar nome da coluna
# which(names(dados_limpos) == "addr_state")
names(dados_limpos)[11] <- "state_address"

# Substituir valores ausentes por "NA"
dados_limpos$state_address[is.na(dados_limpos$state_address)] <- "NA"

# Contar e visualizar a ordenação dos estados
sort(table(dados_limpos$state_address), decreasing = TRUE)


# Definir vetores por região
states_west <- c('WA', 'OR', 'CA', 'NV', 'ID', 'MT', 'WY', 'UT', 'CO', 'AZ', 'NM', 'HI', 'AK')
states_south <- c('TX', 'OK', 'AR', 'LA', 'MS', 'AL', 'TN', 'KY', 'FL', 'GA', 'SC', 'NC', 'VA', 'WV', 'MD', 'DE', 'DC')
states_midwest <- c('ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO', 'WI', 'IL', 'IN', 'MI', 'OH')
states_east <- c('PA', 'NY', 'NJ', 'CT', 'MA', 'VT', 'NH', 'ME', 'RI')

# Categorizar estados por região
dados_limpos$state_address <- ifelse(dados_limpos$state_address %in% states_west, 1,
                                    ifelse(dados_limpos$state_address %in% states_south, 2,
                                           ifelse(dados_limpos$state_address %in% states_midwest, 3,
                                                  ifelse(dados_limpos$state_address %in% states_east, 4, 0))))
rm(states_west, states_south, states_midwest, states_east)

# Converter a coluna state_address para factor
dados_limpos$state_address <- as.factor(dados_limpos$state_address)

# Verificar as mudanças
table(dados_limpos$state_address, useNA = "ifany")

# Tipo de dados
str(dados_limpos)
summary(dados_limpos)

## Verificando Dados Ausentes
colSums(is.na(dados_limpos))

# variáveis com valores ausentes ->  loan_amnt, funded_amnt, int_rate, installment e total_pymnt




#### Tratando Colunas do tipo Numérica     ("sapply(dados_limpos, is.numeric)" -> colunas numéricas)

## Este trecho do código seria somente para ficar igual ao projeto original do Python (não necessário em Python)

# Definindo um valor coringa
# valor_coringa <- max(sapply(dados_limpos[, sapply(dados_limpos, is.numeric)], max, na.rm = TRUE)) + 1

## Antes de iniciar o tratamento das colunas vamos substituir qualquer valor NA ou ausente (chr "") por um valor arbitrário chamado valor_coringa
# dados_limpos[, sapply(dados_limpos, is.numeric)] <- lapply(dados_limpos[, sapply(dados_limpos, is.numeric)], function(coluna) {
#   # Substitui NA por valor_coringa na coluna
#   coluna[is.na(coluna)] <- valor_coringa
#   return(coluna)
# })


## Criando um dataframe de estatísticas (se usasse o valor_coringa como em python precisaria ajustar a funcao estatisticas)

# Calculando estatísticas
estatisticas <- sapply(dados_limpos[, sapply(dados_limpos, is.numeric)], function(coluna) {
  c(min = min(coluna, na.rm = TRUE), 
    mean = mean(coluna, na.rm = TRUE), 
    max = max(coluna, na.rm = TRUE))
})

# Convertendo para dataframe
df_estatisticas <- as.data.frame(t(estatisticas))
df_estatisticas
rm(estatisticas)
summary(dados_limpos)



## Tratando coluna numérica funded_amtn (substituindo todos os valores NA pelo valor mínimo)

# Substituindo valores NA por valor mínimo na coluna funded_amnt
dados_limpos$funded_amnt[is.na(dados_limpos$funded_amnt)] <- df_estatisticas["funded_amnt", "min"]

# Verificando Dados Ausentes e Summary
colSums(is.na(dados_limpos))
summary(dados_limpos$funded_amnt)



## Tratando colunas numéricas loan_amnt, int_rate, installment e total_pymnt  (substituindo todos os valores NA pelo valor médio)

# Substituindo valores NA pelo valor médio das colunas especificadas
for(coluna in c("loan_amnt", "int_rate", "installment", "total_pymnt")) {
  # Extraímos o valor médio como um número
  valor_medio <- as.numeric(df_estatisticas[coluna, "mean"])
  
  # Usamos o valor médio para substituir NA's
  dados_limpos[[coluna]][is.na(dados_limpos[[coluna]])] <- valor_medio
}
rm(valor_medio, coluna)

# Verificando Dados Ausentes e Summary
colSums(is.na(dados_limpos))
summary(dados_limpos)



#### Carregando o segundo dataset

# Este dataset contém a taxa de câmbio de dólar para euro. Cada coluna representa uma taxa de cambio de abertura (Open) a fechamento (Close)
# Vamos usar a coluna Close

# Iremos pegar estes dados, casar com os dados que estão em dólar, fazer os ajustes e preparar novas colunas.

## Carregando dados
dados_cot <- read.csv2("datasets/dataset2.csv", sep = ",", dec = ".")
dados_cot
head(dados_limpos)

## Adicionando Coluna Mes
dados_cot$Mes <- 1:12
dados_cot$Mes <- as.factor(dados_cot$Mes)


## Verificando Dados Ausentes e Sumário
colSums(is.na(dados_cot))
str(dados_cot)
dim(dados_cot)


## Combinando os dois datasets

# Adicionando Uma Nova Coluna Taxa_Cambio a dados_limpos

dados_limpos$taxa_cambio <- NA     # Inicializa a coluna com NA

# Agora, percorra cada linha de dados_limpos$issue_date para preencher taxa_cambio

for (i in 1:nrow(dados_limpos)) {
  # Encontre o índice em dados_cot que corresponde ao Mes em dados_limpos$issue_date
  mes_correspondente <- as.numeric(levels(dados_cot$Mes)[dados_cot$Mes]) == (dados_limpos$issue_date[i])
  
  # Se existe um mês correspondente, atualize taxa_cambio com o valor Close de dados_cot
  if (any(mes_correspondente)) {
    dados_limpos$taxa_cambio[i] <- dados_cot$Close[mes_correspondente]
  } else {
    # Caso contrário, deixe taxa_cambio como NA (já está definido por padrão)
    next # Opcional, apenas para clareza. NA já é o valor padrão.
  }
}
rm(i, mes_correspondente, taxas_cambio)

# Substitui os valores NA em dados_limpos$taxa_cambio pelo valor médio calculado
dados_limpos$taxa_cambio[is.na(dados_limpos$taxa_cambio)] <- mean(dados_cot$Close, na.rm = TRUE)

## Verificando Dados Ausentes e Sumário
colSums(is.na(dados_limpos))
summary(dados_limpos$issue_date)
summary(dados_limpos$taxa_cambio)


# Adicionando 4 novas colunas chamado 'loan_amnt_EUR', 'funded_amnt_EUR', 'installment_EUR', 'total_pymnt_EUR'
dados_limpos$loan_amnt_EUR <- dados_limpos$loan_amnt / dados_limpos$taxa_cambio
dados_limpos$funded_amnt_EUR <- dados_limpos$funded_amnt / dados_limpos$taxa_cambio
dados_limpos$installment_EUR <- dados_limpos$installment / dados_limpos$taxa_cambio
dados_limpos$total_pymnt_EUR <- dados_limpos$total_pymnt / dados_limpos$taxa_cambio

# Modificando o nome das colunas
names(dados_limpos)[names(dados_limpos) == "loan_amnt"] <- "loan_amnt_USD"
names(dados_limpos)[names(dados_limpos) == "funded_amnt"] <- "funded_amnt_USD"
names(dados_limpos)[names(dados_limpos) == "installment"] <- "installment_USD"
names(dados_limpos)[names(dados_limpos) == "total_pymnt"] <- "total_pymnt_USD"

# Removendo a coluna taxa_cambio
dados_limpos$taxa_cambio <- NULL

# Reordenando o dataframe
dados_limpos <- dados_limpos[, c('id', 'loan_amnt_USD', 'loan_amnt_EUR', 'funded_amnt_USD', 'funded_amnt_EUR', 
                                 'int_rate', 'installment_USD', 'installment_EUR', 'total_pymnt_USD', 
                                 'total_pymnt_EUR', 'issue_date', 'loan_status', 'term_months', 
                                 'sub_grade', 'verification_status', 'state_address')]

# Pré-Processamento da Variável int_rate (convertendo valor por 100)
dados_limpos$int_rate <- dados_limpos$int_rate / 100


View(dados_limpos)



## Salvando dataset
# write.csv(dados_limpos, "datasets/dados_limpos.csv", row.names = FALSE)




