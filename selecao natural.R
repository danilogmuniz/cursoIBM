#================================================
rm(list = ls()) #limpando a memoria

#funcao de sobrevivencia
sobrevivencia = function(x, S, u)
{
  return(exp(-S*(x-u)^2))
}

#criar a populacao inicial
#da populacao precisamos saber
#media, desvio padrao, tamanho populacional (constante)

#parametros do modelo
N = 500
media_inicial = 3
desvio_inicial = 1
intensidade = 0.01
otimo = 4
erro_h = 0.05
geracoes = 1000

#vetores de registro
medias = rep(NA, geracoes)
desvios = rep(NA, geracoes)

#criando a populacao inicial
pop = rnorm(N, media_inicial, desvio_inicial)
#hist(pop)

for(i in 1:geracoes)
{
  medias[i] = mean(pop)
  desvios[i] = sd(pop)
  
  #calculando a probabilidade de sobrevivencia
  vetor_sobrevivencia = sobrevivencia(pop, S=intensidade, u=otimo)
  #hist(vetor_sobrevivencia)

  #sorteando quem vive e quem morre
  vivos = rbinom(n = N, size = 1, prob = vetor_sobrevivencia)
  #table(vivos)

  if(sum(vivos) == 0)
  {
    cat("MORREU TODO MUNDOO!!\n")
    break
  }  
  quem_ta_vivo = which(vivos == 1)
  #(1:N)[vivos == 1] #alternativa ao which

  #sortear uma mae para cada filhote da proxima geracao
  maes = sample(x = quem_ta_vivo, size = N, replace = TRUE)

  #criando a proxima geracao e salvando por cima da anterior
  pop = pop[maes] + rnorm(n = N, mean = 0, sd = erro_h)
  #morte por sobre-escrivinhamento

}


plot(medias)
plot(desvios)

gambiarra = cbind(medias-desvios,
                  medias,
                  medias+desvios)

matplot(gambiarra, type = "l",
        col = "black", lwd=c(2,4,2),
        lty=1, ylim=c(0,5))
abline(h=otimo, col="red")
#================================================