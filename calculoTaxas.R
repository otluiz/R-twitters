setwd("~/workspace/R/src")

# dados$x <- NULL ## remove a coluna x

dl <- read.csv("../data/prfLimpoDelegacias.csv") ## carrega o data frame
qtdLinhas <- nrow(dl)

levels(dl$RestrVisibili) 
table(dl$RestrVisibili)

## calcula taxa frequência Restrição visibilidade ---
for(i in 1:qtdLinhas){
  if (dl[i,3] == "Cartazes/faixas")        { dl[i,"tx_RestVisibi"] = 19/qtdLinhas }
  if (dl[i,3] == "Configuração do terreno"){ dl[i,"tx_RestVisibi"] = 1307/qtdLinhas }
  if (dl[i,3] == "Inexistente")            { dl[i,"tx_RestVisibi"] = 46454/qtdLinhas }
  if (dl[i,3] == "Ofuscamento")            { dl[i,"tx_RestVisibi"] = 531/qtdLinhas }
  if (dl[i,3] == "Outros")                 { dl[i,"tx_RestVisibi"] = 1564/qtdLinhas }
  if (dl[i,3] == "Placas")                 { dl[i,"tx_RestVisibi"] = 47/qtdLinhas }
  if (dl[i,3] == "Poeira/fumaça/neblina")  { dl[i,"tx_RestVisibi"] = 225/qtdLinhas }
  if (dl[i,3] == "Vegetação")              { dl[i,"tx_RestVisibi"] = 60/qtdLinhas }
  if (dl[i,3] == "Veículo estacionado")    { dl[i,"tx_RestVisibi"] = 148/qtdLinhas }
}

## calcula taxa frequência Condição Pista ---
levels(dl$CondPista)    
table(dl$CondPista)

for(i in 1:qtdLinhas){
  if (dl[i,2] == "Com buraco" )           { dl[i,"tx_CondPista"] = 1282/qtdLinhas }
  if (dl[i,2] == "Com Gelo")              { dl[i,"tx_CondPista"] = 6/qtdLinhas }
  if (dl[i,2] == "Com material granulado"){ dl[i,"tx_CondPista"] = 184/qtdLinhas }
  if (dl[i,2] == "Em obra")               { dl[i,"tx_CondPista"] = 886/qtdLinhas }
  if (dl[i,2] == "Enlameada")             { dl[i,"tx_CondPista"] = 50/qtdLinhas }
  if (dl[i,2] == "Escorregadia")          { dl[i,"tx_CondPista"] = 468/qtdLinhas }
  if (dl[i,2] == "Molhada")               { dl[i,"tx_CondPista"] = 6808/qtdLinhas }
  if (dl[i,2] == "Oleosa")                { dl[i,"tx_CondPista"] = 48/qtdLinhas }
  if (dl[i,2] == "Outra")                 { dl[i,"tx_CondPista"] = 226/qtdLinhas }
  if (dl[i,2] == "Seca")                  { dl[i,"tx_CondPista"] = 40397/qtdLinhas }
}

levels(dl$TipoAcident)   
table(dl$TipoAcident)
for(i in 1:qtdLinhas){
  if (dl[i,4] == "Atropelamento de animal") { dl[i,"tx_TipoAcident"] = 1796/qtdLinhas }
  if (dl[i,4] == "Atropelamento de pessoa") { dl[i,"tx_TipoAcident"] = 1705/qtdLinhas }
  if (dl[i,4] == "Capotamento")             { dl[i,"tx_TipoAcident"] = 2819/qtdLinhas }
  if (dl[i,4] == "Colisão com bicicleta")   { dl[i,"tx_TipoAcident"] = 587/qtdLinhas }
  if (dl[i,4] == "Colisão com objeto fixo") { dl[i,"tx_TipoAcident"] = 1695/qtdLinhas }
  if (dl[i,4] == "Colisão com objeto móvel"){ dl[i,"tx_TipoAcident"] = 285/qtdLinhas }
  if (dl[i,4] == "Colisão frontal")         { dl[i,"tx_TipoAcident"] = 2179/qtdLinhas }
  if (dl[i,4] == "Colisão lateral")         { dl[i,"tx_TipoAcident"] = 10972/qtdLinhas }
  if (dl[i,4] == "Colisão Transversal")     { dl[i,"tx_TipoAcident"] = 4576/qtdLinhas }
  if (dl[i,4] == "Colisão traseira")        { dl[i,"tx_TipoAcident"] = 15161/qtdLinhas }
  if (dl[i,4] == "Danos Eventuais")         { dl[i,"tx_TipoAcident"] = 178/qtdLinhas }
  if (dl[i,4] == "Derramamento de Carga")   { dl[i,"tx_TipoAcident"] = 92/qtdLinhas }
  if (dl[i,4] == "Incêndio")                { dl[i,"tx_TipoAcident"] = 163/qtdLinhas }
  if (dl[i,4] == "QuedaMoto/bicla")         { dl[i,"tx_TipoAcident"] = 1615/qtdLinhas }
  if (dl[i,4] == "Saída de Pista")          { dl[i,"tx_TipoAcident"] = 5243/qtdLinhas }
  if (dl[i,4] == "Tombamento")              { dl[i,"tx_TipoAcident"] = 1289/qtdLinhas }
}

levels(dl$DiaDaSemana)   
table(dl$DiaDaSemana)
for(i in 1:qtdLinhas){
  if (dl[i,9] == "Domingo")       { dl[i,"tx_DiaSemana"] = 6649/qtdLinhas }
  if (dl[i,9] == "Segunda-feira") { dl[i,"tx_DiaSemana"] = 7821/qtdLinhas }
  if (dl[i,9] == "Terça-feira")   { dl[i,"tx_DiaSemana"] = 6904/qtdLinhas }
  if (dl[i,9] == "Quarta-feira")  { dl[i,"tx_DiaSemana"] = 6856/qtdLinhas }
  if (dl[i,9] == "Quinta-feira")  { dl[i,"tx_DiaSemana"] = 7115/qtdLinhas }
  if (dl[i,9] == "Sexta-feira")   { dl[i,"tx_DiaSemana"] = 7954/qtdLinhas }
  if (dl[i,9] == "Sábado")        { dl[i,"tx_DiaSemana"] = 7056/qtdLinhas }
}

levels(dl$CausaAcident)  
table(dl$CausaAcident)
for(i in 1:qtdLinhas){
  if (dl[i,5] == "Animais na Pista" )           { dl[i,"tx_CausaAcident"] = 2407/qtdLinhas }
  if (dl[i,5] == "Defeito mecânico")            { dl[i,"tx_CausaAcident"] = 1977/qtdLinhas }
  if (dl[i,5] == "Defeito na via")              { dl[i,"tx_CausaAcident"] = 1431/qtdLinhas }
  if (dl[i,5] == "Desobediência à sinalização") { dl[i,"tx_CausaAcident"] = 1931/qtdLinhas }
  if (dl[i,5] == "Dormindo")                    { dl[i,"tx_CausaAcident"] = 1124/qtdLinhas }
  if (dl[i,5] == "Falta de atenção")            { dl[i,"tx_CausaAcident"] = 18858/qtdLinhas }
  if (dl[i,5] == "Ingestão de álcool")          { dl[i,"tx_CausaAcident"] = 2259/qtdLinhas }
  if (dl[i,5] == "Não guardar distância segura"){ dl[i,"tx_CausaAcident"] = 4460/qtdLinhas }
  if (dl[i,5] == "Outras")                      { dl[i,"tx_CausaAcident"] = 12820/qtdLinhas }
  if (dl[i,5] == "Ultrapassagem indevida")      { dl[i,"tx_CausaAcident"] = 1227/qtdLinhas }
  if (dl[i,5] == "Velocidade incompatível")     { dl[i,"tx_CausaAcident"] = 1861/qtdLinhas }
}


levels(dl$TracadoVia)   
table(dl$TracadoVia)
for(i in 1:qtdLinhas){
  if (dl[i,6] == "Cruzamento"){ dl[i,"tx_TracadoVia"] = 1839/qtdLinhas }
  if (dl[i,6] == "Curva")     { dl[i,"tx_TracadoVia"] = 7631/qtdLinhas }
  if (dl[i,6] == "Reta")      { dl[i,"tx_TracadoVia"] = 40885/qtdLinhas }
}


write.csv(dl,"../data/prfCalculoTaxas.csv", row.names = FALSE) #### Script => rodandoALgoritmos.R
