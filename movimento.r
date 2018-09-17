#===========================================
#exemplos com espaco continuo

#criando uma matriz de coordenadas
N = 100
coords = cbind(x = runif(N, 0, 1),
               y = runif(N, 0, 1))
head(coords)
plot(coords, cex=2, pch=19)

#funcao de distancia euclideanas
euclid = function(coords)
{
  return( sqrt(outer(coords[,1], coords[,1], "-")^2+
            outer(coords[,2], coords[,2], "-")^2) )
}

#teste com coordenadas bobinhas
nc = cbind(x = 1:10, y=1:10)
plot(nc, pch=19)
dist = euclid(nc)

N = 10
coords = cbind(x = runif(N, 0, 1),
               y = runif(N, 0, 1))
plot(coords, pch=19)
dist = euclid(coords)
round(dist, 2)
r = 0.5

interacoes = ifelse(dist<=0.5, yes=1, no=0) 

#com quem o individuo 1 pode interagir??
which(interacoes[1,]==1)

#movimento
N = 10
coords = cbind(x = runif(N, 0.5, 1.5),
               y = runif(N, 0.5, 1.5))
plot(coords, pch=19, ylim=c(0,2), xlim=c(0,2),
     cex=2)
#passo zero seria decidir quem vai se mover
#vamos assumir que todo mundo vai dar um passinho

#primeiro passo - sortear um angulo
angulo = runif(N, min = 0, max = 2*pi)

#segundo passo - sortear uma distancia
distancia = runif(N, 0, 0.25)

#terceiro passo - calcular as coordenadas
coords2 = cbind(x = coords[,1]+distancia*cos(angulo),
                y = coords[,2]+distancia*sin(angulo))
points(coords2, cex=2, pch=19, col="red")

arrows(x0=coords[,1], x1=coords2[,1],
       y0=coords[,2], y1=coords2[,2], lwd=3)

#como lidar com a condicao de contorno?
#condicao rigida
#ou morre no precipicio
#ou inverte o sinal do movimento que ia jogar o
#bicho pra fora

#condicao periodica (pacman)
#coordenadas negativas sao convertidas pra positivo
#coordenadas alem do maximo, sao subtraídas do maximo

#y_pacman = y - y_max

#===========================================