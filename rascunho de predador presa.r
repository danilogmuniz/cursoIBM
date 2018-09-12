#================================================
#primeira versao do modelo, controle populacional 
#de presas
b = 1e-6; #parametro de reproducao
tempo = 100 #tempo maximo
#(pensar em como armazenar isso)

D = 500 #vamos armazenar como variavel atomica
registro = rep(NA, tempo)#vetor de tamanho populacional
#ao longo do tempo
registro[1] = D #guardando a populacao inicial

for(i in 2:tempo)
{
  #a cada passo de tempo, cada presa pode viver ou morrer
  D = rbinom(1, size = D, prob = 0.9)
  #entre as vivas, sorteamos um número de filhotes
  #produzidos
  lambda = exp(-b*D^2)
  D = D + sum(rpois(n = D, lambda = lambda))
  
  registro[i] = D
  #filhotes se tornam presas adultas na próxima rodada
}

#produzir um plot de tamanho populacional por tempo
plot(registro, type="l")
abline(h=K, col="red")
summary(registro)
#================================================