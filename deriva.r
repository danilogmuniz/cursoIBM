#================================================
#simulacao de deriva genetica
#rm(list = ls()) #limpa a area de trabalho

#funcao que roda uma simulacao simples de deriva
deriva = function(N=100, tempo=100)
{
  freqs = rep(NA, tempo)
  pop = rep(c(0,1), each=N/2)
  
  for(i in 1:tempo)
  {
    freqs[i] = mean(pop) #frequencia de valores 1
    pop = sample(pop, size = N, replace=TRUE)
  }
  return(freqs)
}

#testando a funcao
teste = deriva()
plot(teste, type="l")

#rodando varias geracoes para fazer o grafico
tempomax = 100 #tempo que vamos rodar
reps = 10 #numero de repeticoes

m = matrix(NA, nrow=tempomax, ncol = reps)

for(i in 1:reps)
{
  m[,i] = deriva(N = 100, tempo = 100)
}

matplot(m, type = "l", lty=1, lwd=2)
#================================================