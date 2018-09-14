#==========================================================
#Dicas de otimizacao

#(OBS: tambem existe um pacote chamado microbenchmark que eh feito
#so para medir tempo que as coisas demoram pra rodar)

#fazer o maximo de sorteios de uma vez!!!
#(e com isso economizar um for)
N = 1e6

tempo_inicial = Sys.time() #guardando o tempo inicial
resposta = rep(NA, N)
for(i in 1:N)
  resposta[i] = rnorm(1)
tempo_final = Sys.time() #guardando o tempo final
tempo_final - tempo_inicial #calculando a demora

tempo_inicial = Sys.time()
resposta = rnorm(N)
tempo_final = Sys.time()
tempo_final - tempo_inicial

#usar operacoes vetorizadas no lugar do for
vetor = rnorm(1e6)

tempo_inicial = Sys.time()
resposta = rep(NA, length(vetor))
for(i in 1:length(vetor))
  resposta[i] = exp(vetor[i])
tempo_final = Sys.time()
tempo_final - tempo_inicial

tempo_inicial = Sys.time()
resposta = exp(vetor)
tempo_final = Sys.time()
tempo_final - tempo_inicial

#alocar a memoria de uma vez (evitar mudar o tamanho dos objetos)

#compilar o codigo
#esse trecho precisa que o arquivo "presa COM predador.r"
#esteja no diretorio de trabalho

#carregando a funcao para a memoria a partir do arquivo
source("presa COM predador.r")

#carregando o pacote compiler
library(compiler)

#compilando a funcao
pred_preyC = cmpfun(pred_prey)

#rodando com a funcao sem compilar
tempo_inicial = Sys.time()
volterra1 = pred_prey()
tempo_final = Sys.time()
tempo_final - tempo_inicial

#rodando com a funcao compilada
tempo_inicial = Sys.time()
volterra2 = pred_preyC()
tempo_final = Sys.time()
tempo_final - tempo_inicial

#==========================================================