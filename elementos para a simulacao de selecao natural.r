#================================================
#base da simulacao de selecao natural

#dicas para a simulação de seleção natural
#s eh a intensidade de selecao
#u eh o valor otimo (100% de sobrevivencia)

#funcao que calcula a probabilidade de sobrevivencia
sobrevivencia = function(x, S, u)
{
  return(exp(-S*(x-u)^2))
}

#com essa funcao, podemos calcular a sobrevivencia
#de acordo com o valor da caracteristica
sobrevivencia(x=5, S=1, u=5)
sobrevivencia(x=4.5, S=1, u=5)
sobrevivencia(x=3, S=1, u=5)

#e a partir de um valor de sobrevivencia, podemos
#sortear se um individuo nasce ou morre
rbinom(n = 1, size = 1, prob = 0.77)

#ou
rbinom(n = 1, size = 1, 
       prob = sobrevivencia(4.5, 1, 5))


#vamos fazer um teste da funcao de sobrevivencia
#e tentar desenhar um grafico
vetorx = seq(from=0, to=10, length.out = 500)
vetors = sobrevivencia(vetorx, S=1,u=5)
plot(vetors~vetorx)
#parece que faz sentido

#com isso, podemos fazer o sorteio de quem vive
#e quem morre
vive = rbinom(500, size = 1, prob = vetors)
plot(vive~vetorx)

#indexando soh os que tem valor de vive == 1 
vetorx[vive==1]

#sorteio da distribuicao normal com rnorm
rnorm(n = 500, mean = 0, sd = 2)

#================================================