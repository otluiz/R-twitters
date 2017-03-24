#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

rm(df) ## remove antigos objetos
################################################################################
############################             #######################################
###########################     BR 101      ####################################
############################             #######################################
################################################################################
## calcula quantidade de mortos o atributo Gravidade----------------------------
df <- read.csv("./data/LimpoCheiroso.csv") ## carrega o data frame

### ajustando a variável Dia da semana, tem que criar uma nova coluna
for(i in 1:nrow(df)){
  if(df[i, "DiaDaSemana"] == "Segunda-feira") { df[i, "DiaSemana"] = 1 }
  if(df[i, "DiaDaSemana"] == "Terça-feira") { df[i, "DiaSemana"] = 2 }
  if(df[i, "DiaDaSemana"] == "Quarta-feira") { df[i, "DiaSemana"] = 3 }
  if(df[i, "DiaDaSemana"] == "Quinta-feira") { df[i, "DiaSemana"] = 4 }
  if(df[i, "DiaDaSemana"] == "Sexta-feira") { df[i, "DiaSemana"] = 5 }
  if(df[i, "DiaDaSemana"] == "Sábado") { df[i, "DiaSemana"] = 6 }
  if(df[i, "DiaDaSemana"] == "Domingo") { df[i, "DiaSemana"] = 7 }
}
#### limpando o dada frame
df$Binario <- NULL
df$Gravidade <- NULL
df$DiaDaSemana <- NULL
df$Municipio <- NULL
df$TotFerVivos <- NULL
df$TipoAuto <- NULL

###-----------------------------------------------------------------------------
df.Br101 <- subset(df, BR=='101')
df.Br232 <- subset(df, BR=='232')

#write.csv(df.Br101,"./data/BR101/RNNLiteral2.csv", row.names = FALSE)
#write.csv(df.Br232,"./data/BR232/RNNLiteral2.csv", row.names = FALSE)

################################################################################
###### Calculo das Probabilidades para cada variável
################################################################################
########################## Calculo Restrição visibilidade ---: 1/10
qtdLinhas <- nrow(df.Br101) # = 21.861
table(df.Br101$RestrVisibili)

CrtazesFaixas <- 8/qtdLinhas                 ###########  Corr. Linear: 0.05271526
Configuracaodoterreno = 477/qtdLinhas        ###########      
Inexistente = 20499/qtdLinhas                ###########              : 0.9376973
Ofuscamento = 98/qtdLinhas                   ###########   
Outros = 635/qtdLinhas                       ###########            
Placas = 18/qtdLinhas                        ###########              
PoeiraFumaçaNeblina = 53/qtdLinhas           ###########                
Vegetacao = 25/qtdLinhas                     ###########                
VeiculoEstacionado = 48/qtdLinhas            ###########              

########################## Calculo Condição Pista ################################
levels(df.Br101$CondPista)    
table(df.Br101$CondPista)

ComBuraco = 1060/qtdLinhas                     ###########  Corr. Linear: 0.05271526
ComGelo = 4/qtdLinhas                           ##########        "      
ComMaterialGranulado = 38/qtdLinhas             ##########        "       
EmObra = 392/qtdLinhas                         ###########        " 
Enlameada = 37/qtdLinhas                  ################        "      -
Escorregadia  = 233/qtdLinhas               ##############        "      
Molhada = 3193/qtdLinhas                       ###########        "      
Oleosa = 27/qtdLinhas                   ##################        "      
Outra = 60/qtdLinhas                              ########        "       
Seca = 16817/qtdLinhas                        ############        "      : 0.7692695 

#################################################################################
########################### regressão linear  ###################################
#################################################################################

fit <- lm(df.Br101$KMArredondado ~ df.Br101$CondPista + df.Br101$TipoAcident + df.Br101$TracadoVia)
fit


### função para crirar uma matriz de n rows e m columns
criarMatriz <- function(lin,col,alt) {
  rs = array(0, dim=c(lin, col, alt));
  return(rs)
}

#nrow(df.Br101)
#### Criar uma matriz já inicializada com todas as entradas em zero
criarMatrizFinal <- function(lin,col,alt){
  r <- criarMatriz(lin,col,alt)
  for(i in 1:nrow(df.Br101)){
    r[df.Br101[i,8]+1,df.Br101[i,9]+1,df.Br101[i,10]] <- r[df.Br101[i,8]+1,df.Br101[i,9]+1,df.Br101[i,10]] + df.Br101[i,7]
  }
  return(r)
}

l = 24  ## hora 0 - 23
c = max(df.Br101$KMArredondado)+1 ## quilômetro máximo da rodovia
a = 7    ## correspondente aos dias da semana
rs = criarMatrizFinal(l,c,a)


### Converte para data frame & Alterar o no me da linha e coluna

dfs <- as.data.frame(rs[,,1])
colnames(dfs) = c(0:(c -1))
row.names(dfs) = c(0:(l -1))
write.csv(dfs,"./data/BR101/MatrizMortos3D1.csv", row.names = FALSE)

################################################################################
### Salvar cada dia da semana em um arquivo diferente, ex.: MatrizMortos3d1.csv, MatrizMortos3D2.csv...
### Cada linha e coluna estão numeradas a partir do Km(0) e Hora(0)

salvaMatriz <- function(rs) {
  for(i in 1:7){
    dfs <- as.data.frame(rs[,,i])
    colnames(dfs) = c(0:(c -1))
    row.names(dfs) = c(0:(l -1))
    nomePart = paste("./data/BR101/MatrizMortos3D",i, sep = "", collapse = NULL) 
    tipoExte = ".csv"
    nomeArq = paste(nomePart, tipoExte, sep = "") 
    write.csv(dfs,nomeArq, row.names = TRUE)
  }
}



