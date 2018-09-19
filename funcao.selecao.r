#================================================
#Script com a simulacao de selecao natural em
#formato de funcao. Esse arquivo eh carregado
#usando a funcao source() pelo script de exploracao
#de espaco de parametros
#------------------------------------------------
#funcao de sobrevivencia
sobrevivencia = function(x, S, u)
{
  return(exp(-S*(x-u)^2))
}
#------------------------------------------------
#funcao que roda o modelo de selecao natural
#estabilizadora
selecao = function(N = 500, #tamanho populacional
                   mean_ini = 3, #media inicial
                   sd_ini = 1, #desvio inicial
                   S = 0.1,#intensidade de selecao
                   mu =4, #otimo da selecao
                   h = 0.05, #"erro" da herdabilidade
                   g = 100) #numero de geracoes
{
  
  #criando vetores de registro
  medias = rep(NA, g)
  desvios = rep(NA, g)
  
  #criando a populacao inicial
  pop = rnorm(N, mean_ini, sd_ini)
  #hist(pop)
  
  for(i in 1:g)
  {
    medias[i] = mean(pop)
    desvios[i] = sd(pop)
    
    #calculando a probabilidade de sobrevivencia
    vetor_sobrevivencia = sobrevivencia(pop, S, mu)
    #hist(vetor_sobrevivencia)
    
    #sorteando quem vive e quem morre
    vivos = rbinom(n = N, size = 1, prob = vetor_sobrevivencia)
    #table(vivos)
    
    if(sum(vivos) == 0)
    {
      cat("morreram todos\n")
      break
    }  
    
    #descobre quais individuos estao vivos
    quem_ta_vivo = which(vivos == 1)
    
    #sorteia uma mae para cada filhote da proxima geracao
    maes = sample(x = quem_ta_vivo, size = N, replace = TRUE)
    
    #cria a proxima geracao e salva por cima da anterior
    #adicionando um "erro" que poderia ser gerado por mutacao
    pop = pop[maes] + rnorm(n = N, mean = 0, sd = h)
  }
  
  #retornando pro usuario um data.frame contendo as medias
  #e desvios por geracao
  return(data.frame(medias = medias, desvios = desvios))
}

#rodando alguns testes
#teste = selecao(N=50)
#plot(teste$medias, type="b")
#plot(teste$desvios, type="b")
#================================================