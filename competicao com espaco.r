#================================================
#modelo de competicao com espaco explicito
#================================================


#funcao que calcula a probabilidade de morte
morte = function(a, v)
{
  1-exp(-a*v^2)
}
#curve(morte(a=1, v=x), from = 0, to=15, lwd=2)
#morte(1, 0); morte(0.1, 10)

#funcao que calcula as 3 probabilidades!
piz = function(R=1, B=2, a=0.1, v)
{
  M = morte(a, v)
  soma = R+B+M
  ans = c(Pr = R, Pm = M, Pb = B)
  return(ans/soma)
}

#Testando a funcao piz!
piz(R=1, B=2, a=0.1, v=0);
piz(R=1, B=2, a=0.1, v=3);
piz(R=1, B=2, a=0.1, v=10);

#funcao que calcula a matriz de distancias euclideanas
euclid = function(m1, m2=m1)
{
  sqrt(outer(m1[,1]-m2[,1], "-")^2+
       outer(m1[,2], m2[,2], "-")^2)
}

#funcao que calcula um vetor de distancias entre 
#uma coordenada e um conjunto de coordenadas
euclidv = function(v, m)
{
  sqrt((v[1]-m[,1])^2 + (v[2]-m[,2])^2)
}


#funcao que roda a simulacao toda
#no final das contas, eu coloquei o tempo continuo mesmo,
#com um sorteio a cada rodada de quanto tempo se passou.
#(apesar de eu nao ter colocado isso como obrigatorio no exercicio)

#com plota = TRUE, a simulacao praticamente faz uma animacao
simplanta = function(N = 500, #tamanho populacional inicial
                     a = 2,   #parametro de morte por competicao
                     D = 0.2, #raio de competicao
                     s = 0.2, #distancia media da dispersao
                     R = 1,   #vai determinar probabilidade de reproducao
                     B = 0,   #vai determinar a probabilidade de nao fazer nada
                     tmax = 10, #tempo maximo que vai rodar
                     lambda = 1, #fecundidade das plantas
                     plota=FALSE)
{

  #coordenadas iniciais da populacao
  m = cbind(runif(N, 0.25, 0.75),
            runif(N, 0.25, 0.75))
  
  #objetos de registro de atividades
  #listao = list() #essa lista guardava as coordenadas a cada passo de tempo
  #listao[[1]] = m
  registro = cbind(tempo = NA, N = NA)
  registro[1,] = c(0, N)
  tempo = 0
  
  if(plota) #se o usuario quiser graficos
  {
    x11() #abre uma janela grafica (senao trava o RStudio)
    par(las=1) #arruma o eixo y
    plot(m, pch=19, xlim=c(0,1), ylim=c(0,1), main="0.000") #faz o primeiro plot
  }
  
  #iniciando o contador de eventos
  i=2 #nessa versao isso nem esta sendo usado pra nada
  
  #enquanto o tempo for menor que o tmax que eh pra rodar
  while(tempo < tmax)
  {
    deltat = rexp(1, N) #sorteia quanto tempo se passou
    tempo = tempo+deltat #atualiza a varivavel de tempo
    
    if(N<1)
      break #se todo mundo morreu, sai do while
    else #esse else, na verdade, nao era necessario
    { 
      #se alguem ainda esta vivo
      
      #sorteia um individuo para "agir"
      ind = sample(1:N, 1) 

      #calcula o vetor de distancias
      vdist = euclidv(m[ind,], m[-ind,])
      
      #descobre o numero de vizinhos
      nviz = sum(vdist<=D)
      
      #calcula a "pizza" de probabilidades
      piz.i = piz(R, B, a, nviz)
      
      #sorteia a acao usando essas probabilidades
      acao = sample(1:3, size = 1, prob=piz.i)
      
      #realiza a acao
      if(acao == 1)#reproducao
      {
        nprole = rpois(1, lambda) #sorteia o numero de proles
        if(nprole > 0) #se for nascer alguem
          for(j in 1:nprole) #adiciona as proles uma de cada vez
          {
            angulo = runif(1, 0, 2*pi) #sorteia um algulo
            desl = rexp(1, 1/s) #sorteia uma distancia
            
            #calcula as coordenadas x e y
            nx = m[ind,1]+desl*cos(angulo) 
            ny = m[ind,2]+desl*sin(angulo)
            
            #correcoes pacman
            if(nx<0) nx = abs(nx)
            if(ny<0) ny = abs(ny)
            if(ny>1) ny = ny-1
            if(nx>1) nx = nx-1
            
            #adiciona o novo filhote na matriz
            m = rbind(m, c(nx,ny))
            
            #atualiza o tamanho popuacional
            N = N+1
            
            #depois que eu parei pra pensar, acho que da pra fazer multiplos
            #nascimentos sem o for, soh usando operacoes vetorizadas. Fica o
            #desafio!
            }
      }
      else if(acao == 2)#morte!!  
      {
        m = m[-ind,] #remove o individuo da matriz de coordenadas
        N=N-1 #atualiza o tamanho popuacional
      }
      
      #listao[[i]] = m #aqui seria atualizada a super lista de coordenadas
      
      #atualiza o registro de tamanho populacional ao longo do tempo
      registro = rbind(registro, c(tempo, N))
      
      if(plota)
        plot(m, pch=19, #xaxt="n", yaxt="n", 
             xlab="", ylab="", ylim=c(0,1), xlim=c(0,1), main=round(tempo,  4))
    }
    i=i+1 #atualizando o contador de numero de eventos
    #no momento, isso nao esta sendo usado pra nada
  }
  
  invisible(registro) #retorna o registro
}
#pacote animation! pelo fim da gambiarra!!

#testando
simplanta(plota=TRUE, N=200)
teste = simplanta(plota=TRUE, N=200)
head(teste)
tail(teste)
plot(teste)
#================================================