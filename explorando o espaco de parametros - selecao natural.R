#================================================
#SCRIPT DE EXPLORACAO DO ESPACO PARAMETRICO
#(com o modelo de selecao natural estabilizadora)

#para rodar esse script, vc precisa salvar o arquivo
#"funcao.selecao.r" no diretorio de trabalho do R
#para descobrir o diretorio de trabalho vc pode usar
#a funcao getwd(), e para mudar esse diretorio use
#setwd()

#carregando a funcao que roda a simulacao
source("funcao.selecao.r")
#================================================
#funcao que calcula uma medida de variacao
#em torno de um valor
medida = function(x, mu=4, pos=80:100)
{
  mean((x[pos] - mu)^2)
}
#================================================
#rodando uma exploracao de espaco de parametros "matricial"
#com 3 valores diferentes de N e 3 de S
vetorN = c(50, 500, 5000)
vetorS = c(0.125, 0.25, 0.5)

#criando o data.frame com todas as combinacoes de valores
#de parametros replicadas 10x
dados = expand.grid(replica = 1:10,
                     N = vetorN, 
                     S= vetorS)
head(dados)

#adicionando a coluna em que vamos guardar nossa medida
#de oscilacao
dados$medida = NA

#percorrendo o data.frame
for(i in 1:nrow(dados))
{
  #rodando uma simulacao para cada combinacao de valores
  #de parametro
  sim = selecao(N = dados$N[i],
                S = dados$S[i])
  #armazenando o valor da rodada atual no data.frame
  dados$medida[i] = medida(sim$medias)
}

#conferindo os dados
head(dados)
tail(dados)

#calculando medias em cada grupo
tapply(dados$medida,
       list(dados$N, dados$S),
       mean)

#produzindo um boxplot toscao inicial
boxplot(medida~S+N, data=dados)

#tentando melhorar esse boxplot

#decidindo as posicoes dos boxes
ats = rep(c(2,6,10), each=3)+rep(c(-1, 0, 1),3)

#plotando com corzinha e tudo
cores = c("aquamarine", "dodgerblue", "firebrick")

boxplot(log(medida)~S+N, data=dados,
        col=cores,
        range=0, xaxt="n",
        at = ats, xlab="Population size",
        ylab="Population variation (log)")
axis(side=1, at=c(2,6,10), labels = c(50,500,5000))

#colocando uma legenda
legend("topright", bty="n", pch=22,
       pt.bg=cores, cex=1.5, pt.cex=2,
       legend = vetorS,
       title = "Selection intensity")
#agora sim!!

#outra opcao (muito mais facil!!)
library(lattice)
bwplot(log(medida)~as.factor(S)|as.factor(N), data=dados)

#================================================
#rodando algumas simulacoes exemplo com as combinacoes
#anteriores de parametros, soh para poder desenhar as
#"trajetorias" de cada populacao ao longo do tempo

#criando o data.frame de valores de parametros
parms = expand.grid(replica = 1,
                   N = vetorN, S= vetorS)

#criando uma lista pra guardar tudo
listao = list()

#rodando as simulacoes
for(i in 1:nrow(parms))
{
  sim = selecao(N = parms$N[i],
                S = parms$S[i])
  
  #criando a matriz para o plot
  matriz = cbind(sim$medias-sim$desvios,
                 sim$medias,
                 sim$medias+sim$desvios)
  mi = medida(sim$medias)
  
  listao[[i]] = list(medida = mi,
                     m = matriz)
}

#fazendo os plots
par(mfrow = c(3,3))#dividindo o espaco de plotagem em 3 linhas por 3 colunas
for(i in 1:nrow(parms))
{
  titulo = paste("N =", parms$N[i],
                 "; S =", parms$S[i])
  
  matplot(listao[[i]]$m, type = "l",
          col = "black", lwd=c(2,4,2),
          lty=1, ylim=c(0,5),
          main = titulo)
  abline(h=4, col="red")
}
par(mfrow=c(1,1)) #devolvendo o espaco de plotagem ao estado original
#====================================================================

#um hipercubo nao latino
Nsim = 1000 #numero total de simulacoes

#amostrando valores de N e S de distribuicoes uniformes
dados2 = data.frame(N = round(runif(Nsim, 50, 1000)), 
                     S = runif(Nsim, 0.01, 0.5))
dados2$medida = NA
head(dados2)

for(i in 1:nrow(dados2))
{
  sim = selecao(N = dados2$N[i],
                S = dados2$S[i])
  dados2$medida[i] = medida(sim$medias)
}

head(dados2)
tail(dados2)

#calculando uma meio tosca maluca de interacao
dados2$int = dados2$N * dados2$S

#plorando a resposta em funcao dos parametros
par(mfrow=c(1,1))
plot(log(medida)~N, data=dados2)
plot(log(medida)~S, data=dados2)
plot(log(medida)~int, data=dados2)

#rodando uma regressao so pra ter uma ideia
#(a funcao scale() padroniza os valores)
analise = lm(log(medida)~scale(N)*scale(S), data=dados2)
coef(analise)
#tanto N quanto o S tem efeitos negativos marcantes,
#mas a interacao parece ter um efeito super fraco
#================================================