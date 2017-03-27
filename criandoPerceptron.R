setwd("~/workspace/R/")

## nosso perceptron

# pontos representam cancer em uma amostra de laboratório
Ax1 = rnorm(20, 0, 1)
Ax2 = rnorm(20, 5, 1)
A = cbind(Ax1, Ax2)
plot(A, col='red')

# normal
Bx1 = rnorm(30, -3, 1)
Bx2 = rnorm(30, 0, 1)
B = cbind(Bx1, Bx2)

plot(rbind(A, B), type = "n")
points(A, col="red")
points(B, col="blue")
abline(b, a)
abline(1.8, -1, lwd=2)

### salvando os dados para ficarem fixos
write.table(A, "./data/classe-A.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(B, "./data/classe-B.txt", sep = "\t", row.names = FALSE, quote = FALSE)

## x é vetor
perceptron = function(x1, x2, w0, w1, w2){
  valor = w0 + w1*x1 + w2*x2
  if(valor > 0){return(1)} else {return(0)}
}

a = -1; b=1.8
## a = -1; b=1.8 ## parâmetros sairam de abline(1.8, -1, lwd=2)
## perceptron(1.3264277, 2.729110, -b, a, 1)

### calculando os erros, A é a classe 1 ou vermelho ou acima da reta
for(i in 1:dim(A)[1]){
  resultado = perceptron(A[i,1], A[i,2], -b, -a, 1)
  print(resultado)
}

### calculando os erros, B é a classe 0 ou azul abaixo da reta
for(i in 1:dim(B)[1]){
  resultado = perceptron(B[i,1], B[i,2], -b, -a, 1)
  print(resultado)
}

### tudo certo daqui pra cima e testado

## parametrizar "genérico" em termos de w's para mais tarde fazer "for"
w0 = -b
w1 = -a
w2 = +1

### checando se sabemos contar erros (é o mesmo que erro médio quadratico)
erro = 0
for(i in 1:dim(B)[1]){
  predicao = perceptron(B[i,1], B[i,2], w0, w1, w2)
  erro = erro + (predicao != 0) ## se alguém não 0 "fala que errou"
}
for(i in 1:dim(A)[1]){
  predicao = perceptron(A[i,1], A[i,2], w0, w1, w2)
  erro = erro + (predicao != 1) ## se alguém não 1 "contabiliza que errou"
}


## não errou nenhum, agora adicionando um erro 
teste = c(0,0) ## colocar o valor (0,0) em uma classe errada, se colocar isso na classe do A
A = rbind(A, teste) # coloquei um ponto (0,0) na classe A que é toda acima da reta
tail(A)

### funcionou agora, acusou que tem 1 erro, agora voltar o A novamente
A = A[1:20,] ### faz o A voltar como estava abandonando o último elemento colocado

## tudo em uma função agora
erro = function(A, B, w0, w1, w2){
  erro = 0
  for(i in 1:dim(B)[1]){
    predicao = perceptron(B[i,1], B[i,2], w0, w1, w2)
    erro = erro + (predicao != 0) ## se alguém não 0 "fala que errou"
  }
  for(i in 1:dim(A)[1]){
    predicao = perceptron(A[i,1], A[i,2], w0, w1, w2)
    erro = erro + (predicao != 1) ## se alguém não 1 "contabiliza que errou"
  }
  return(erro)
}

erro(A, B, -1.2, 1 ,1)

## abline(-1.6, -1) uma reta abaixo que produz erros
## se entrar com esses parametros para o perceptron detectar 
## ele vai ter que dizer que há dois erros
b = 1.6; a= 1
erro(A, B, -b, -a ,1)

### agora faz uma reta horizontal dando erro = 1
plot(rbind(A, B), type = "n")
points(A, col="red")
points(B, col="blue")
abline(b, a)

## faz outra reta separadora
b = 2; a = 0
erro(A, B, -b, -a ,1)

## Calculo do erro de outra forma
erro = function(A, B, w0, w1, w2){
  s = 0 ## vai guardar a soma dos erros
  n = dim(B)[1]  ## tamanho do B
  valorCorreto = 1 ## Código da Classe B
  for(i in 1:n){
    predicao = perceptron(B[i,1], B[i,2], w0, w1, w2)
    s = s + (valorCorreto - predicao)^2 ## se alguém não 0 "fala que errou"
  }
  n = dim(A)[1]  ## tamanho do A
  valorCorreto = 0 ## Código da Classe A
  for(i in 1:n){
    predicao = perceptron(A[i,1], A[i,2], w0, w1, w2)
    s = s + (valorCorreto - predicao)^2 ## se alguém não 1 "contabiliza que errou"
  }
  return(s)
}

### uma vez que temos uma função que mede os erros que funciona podemos partir para o aprendizado
plot(rbind(A, B), type = "n")
points(A, col="red")
points(B, col="blue")

erros = NULL;
for(w0 in  seq(0.1, -3) ){
  for(w1 in  seq(0.2, 2) ){
    for(w2 in  seq(0.2, 3) ){ 
      E = erro(A, B, w0, w1, w2)
      erros = c(erros, E)
      abline(-w0/w2, -w1/w2, col="grey") # a = -1; b=1.8 
    }                                    ## vou variar em trono dessa reta para procurar
  }
}


### Esta função tem erros
aprendizadoForcaBruta(A, B){
  erros = NULL; erroMin = 100 #; w0Min = 100; w1Min = 100; w2Min = 100
  for(w0 in  seq(0.1, -3) ){
    for(w1 in  seq(0.2, 2) ){
      for(w2 in  seq(0.2, 3) ){
        E = erro(A, B, w0, w1, w2)
        if (E < erroMin){
          w0Min = w0
          w1Min = w1
          w2Min = w2
          erroMin = E
        }
        erros = c(erros, E)
        if (erroMin == 0){ return(c(w0Min, w1Min, w2Min)) }
        # a = -1; b=1.8 ## vou variar em trono dessa reta para não procurar feito louco em todo lugar {:^)}
        #abline(-w0/w2, -w1/w2, col="grey")
      }
    }
  }
  return(c(w0Min,w1Min, w2Min))
}




summary(erros)

