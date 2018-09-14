#====================================================================
#Simulacao de predador-presa com passos de tempo discretos.
#Existe uma populacao maxima de predadores, mas nao de presas,
#e nao ha migracao/dispersao
#--------------------------------------------------------------------
#funcao que pega um vetor com um monte de uns e retorna um vetor
#com apenas um um. Se o vetor tiver valores diferentes de zero e um,
#vai dar problema
resta_um = function(x)
{
  if(sum(x)>1)
  {
    uns = which(x==1)
    x[uns] = 0
    x[sample(uns, 1)] = 1
    return(x)
  }
  else
    return(x)
}
#resta_um(c(0,1,1,0))
#--------------------------------------------------------------------
#########################################
#Funcao que roda o modelo predador-presa
#########################################

#parametros do modelo/argumentos da funcao
#b = 1e-6 #parametro de reproducao das vitimas
#a = 0.1 #sucesso de captura do predador
#l = 0.25 #parametro de reproducao dos predadores
#tempo = 100 #tempo maximo
#sV = 0.9 #sobrevivencia das vitimas

#DV = 500 #"densidade" de vitimas na mancha (valor inicial)
#DP = 10 #"densidade" de predadores na mancha (valor inicial)
#Pmax = 300 #numero maximo de predadores na mancha
#Cmax = 10 #numero maximo de capturas por predador por rodada

pred_prey = function(b = 1e-6, a=0.1, tempo=100, sV=0.9,
                     DV = 500, DP=10, Pmax=300, Cmax=10, l=0.25)
{
  #data.frame dos predadores
  predadores = data.frame(id = 1:Pmax, 
                          alive = rep(c(1,0), c(DP, Pmax-DP)),
                          capturas = rep(0, Pmax))
  #head(predadores)
  #matriz de tamanho populacional de vitimas e predadores 
  registro = cbind(V = rep(0, tempo), P = rep(0, tempo))
  
  #guardando a populacao inicial
  registro[1,] = c(DV, DP)
  #head(registro)
  
  #regras adicionais: predador sem comida morre
  #predador com comida deixa um numero de filhotes igual a
  # 1 + rpois(lambda = capturas*l)
  for(i in 2:tempo)
  {
    if(DV == 0)
      break
    
    #a cada passo de tempo, cada presa pode viver ou morrer
    DV = rbinom(1, size = DV, prob = sV)
    #entre as vivas, sorteamos um número de filhotes
    #produzidos
    lambda = exp(-b*DV^2)
    DV = DV + sum(rpois(n = DV, lambda = lambda))
    
    #predadores encontram as vitimas
    vitimas = 1:DV #vetor de vitimas
    DP = sum(predadores$alive)
    
    if(DP > 0 & DV > 0)
    {
      #criando uma matriz de encontros em que cada linha so tem uma quantidade
      #de uns igual a Cmax
      if(DV<=Cmax)
      {
        encontros = matrix(1, nrow=DP,
                           ncol = DV)
      }
      else
      {
        encontros = matrix(rep(c(1,0), c(Cmax, DV-Cmax)), 
                           nrow=DP, ncol=DV,
                           byrow=TRUE)
        #View(encontros)
        #"shufflando" cada linha da matriz pra espalhar os uns aleatoriamente
        encontros = t(apply(encontros, 1, sample))
      }
      
      #calculando numero total de encontros
      Nencontros = sum(encontros)
      
      capturas = encontros
      #View(capturas)
      capturas[capturas == 1] = rbinom(n = Nencontros, size=1, prob=a)
      
      #resolvendo a questao de que cada presa so pode ser consumida por um predador
      capturas = apply(capturas, 2, resta_um)
      #table(colSums(capturas))
      
      #colocando o numero de capturas no data.frame dos predadores
      predadores$capturas[predadores$alive == 1] = apply(capturas, 1, sum)
      
      #View(predadores)
      #matando os predadores que nao comeram nada
      predadores$alive[predadores$alive == 1 & predadores$capturas == 0] = 0
      DP = sum(predadores$alive) #atualizando a densidade de predadores
      
      #reproducao de quem comeu alguma coisa
      lambda_predadores = predadores$capturas[predadores$alive == 1]*l
      novos_predadores = sum(1+rpois(DP,
                                   lambda = lambda_predadores))
      if(DP+novos_predadores>=Pmax)
        predadores$alive = 1 #todos os "slots" de predadores estao ocupados
      else
      {
        #preenche so algumas vagas de predadores
        vagas = which(predadores$alive == 0)
        predadores$alive[vagas[1:novos_predadores]] = 1
      }
      
      #atualizando os valores de DP e DV
      DP = sum(predadores$alive)
      DV = DV - sum(capturas)
      
      predadores$capturas = 0 #resetando a barra de energia
    }
    
    registro[i,] = c(DV, DP)
    head(registro)
    #filhotes se tornam presas adultas na próxima rodada
  
  }
  return(registro)
}

###############
#exemplo
###############

#rodando pra ver se funciona
#lotka = pred_prey(Pmax = 350, tempo=40)

#dando uma olhada no objeto criado
#head(lotka)
#tail(lotka)

#grafico de tamanho populacional por tempo
#(presas em azul)
#matplot(lotka, type="l", 
#        col=c("blue", "red"), lty=1,lwd=3)




#================================================