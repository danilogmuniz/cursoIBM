#====================================================================
#selecao disruptiva + acasalamento assortativo = especiacao?
############################################################
#--------------------------------------------------------------------
#funcao que calcula a sobrevivencia segundo uma funcao
#bimodal (usando uma leve gambiarra)
sobrevivenciaD = function(x, u1, u2, a)
{
  ans = exp(-a*(x-u1)^2) + exp(-a*(x-u2)^2)
  if(any(ans>1)) #se tiver algum valor maior que 1 
    ans = ans/max(ans) #divive tudo pelo valor maximo
  
  return(ans) #retorna pro usuario
}
#exemplo
#curve(sobrevivenciaD(x=x, u1=2, u2=4, a=4),
#      lwd=4, col="red", from=0, to=6) #it funfs!!

#--------------------------------------------------------------------
assort = function(zi, zj, b)
{
  exp(-b*(zi-zj)^2)
}
#assort(1, 5, 0.1)
#assort(1, 2, 0.1)
#--------------------------------------------------------------------
#alternative rbinom - eh soh um jeito esperto de usar um tapply
#na hora da escolha de parceiros
arbinom = function(probs)
{
  n = length(probs)
  rbinom(1:n, 1, probs)
}
#--------------------------------------------------------------------
#escolhe uma posicao onde exista um valor 1 dentro de um vetor de
#zeros e uns
pickone = function(x)
{
  ops = which(x==1)
  if(length(ops)==0)
    return(0)
  else
    return(sample(ops)[1])
}
#--------------------------------------------------------------------
#funcao que roda a simulacao
simdisrupt = function(N = 1000, #tamanho populacional
                      u1 = 2, u2 = 4, #medias da selecao disruptiva
                      media_ini = 3, sd_ini = 1, #parametros inicais da populacao
                      a = 5,  #intensidade da selecao natural
                      b=0.5,  #intensidade da escolha de parceiros
                      h=0.1,  #"erro" da herdabilidade
                      g=20,   #numero de geracoes pra rodar
                      registra = c(1,5,10, 15, 20)) #quais geracoes registrar
{

  pop = data.frame(z = rnorm(N, media_ini, sd_ini),
                 vivo = 1,
                 parceiro = 0)
  registro = NA

  if(!is.null(registra[1]))
    registro = data.frame(z = pop$z, gen = 1)

  for(i in 2:g)
  {
    if(i %in% registra)
      registro = rbind(registro, data.frame(z = pop$z, gen=i))
    
    #selecao disruptiva
    p_sobrevive = sobrevivenciaD(pop$z, u1, u2, a)
    pop$vivo = rbinom(N, 1, p_sobrevive)
    vivos = which(pop$vivo==1) #posicoes dos individuos vivos
    
    #escolha de parceiros
    
    #matriz de individuos vivos, vai ser multiplicada pela
    #matriz de probabilidade de copula para que a probabilidade
    #de copula com os mortos seja igual a zero
    #(multiplicacao no sentido de produto de Hadamard)
    matrizVivo = matrix(pop$vivo, nrow=N, ncol=N, byrow=TRUE)
    matrizVivo[1:6,1:6]
    
    #montando a matriz de probabilidade de escolha
    pij = outer(pop$z, pop$z, assort, b=b)
    diag(pij) = 0 #ninguem pode de "auto-escolher"
    
    #multiplando a matriz de escolha pela matriz dos vivos
    pij = pij * matrizVivo
    
    #mantendo so as linhas dos individuos vivos
    #(os mortos nem escolhem parceiros)
    pij = pij[vivos,]
    
    #decidindo com quais individuos cada um copularia
    possiveis = t(apply(pij, 1, arbinom))
    
    #escolhendo soh um dos possiveis usando a funcao
    #pickone
    escolhidos = apply(possiveis, 1, pickone)
    
    #guardando a informacao no data.frame
    pop$parceiro[vivos] = escolhidos
    
    #descobrindo quem esta vivo e conseguiu encontrar um parceiro sexual
    ferteis = which(pop$vivo ==1 & pop$parceiro !=0)
    
    #escolhendo uma mae para cada filho. Cada individuo
    #vivo vai ser mae de pelo menos um filho, e para povoar
    #o restante da populacao amostraremos aleatoriamente entre
    #os vivos
    maes = c(ferteis, 
             sample(ferteis, N-length(ferteis), replace=TRUE))
    
    #descobre o parceiro de cada mae
    pais = pop$parceiro[maes]
    
    #calculando o z dos filhos
    zfilhos = (pop$z[maes] + pop$z[pais])/2 + rnorm(N, 0, h)
    
    #substituindo a nova geracao na matriz
    pop = data.frame(z = zfilhos,
                     vivo = 1,
                     parceiro = 0)
  
  }
  return(registro)
}

#rodando um teste
teste = simdisrupt(N=2000)

#fazendo um plot bonitinho
library(lattice)
histogram(~z|factor(gen), data=teste)

#====================================================================
