#================================================
#exemplos de estruturas de dados

x = 12
class(x)

palavra = "s"
class(palavra)

bool = FALSE
class(bool)

#criando vetores
vetor = c(1, 2, 3, 4, 56)
vetor
class(vetor)
length(vetor)
c(vetor,vetor)

?rep
repeticao = rep(vetor, 3)
rep(vetor, length.out = 12)
rep(vetor, each=2)

rep(c("ar", "condicionado"), 3)
palavras = c("ar", "condicionado")
rep(palavras, 3)
rep(palavras, each=2)

#criando sequencias
10:20
seq(from=1, to=10, by=1)
seq(1, 10, 1)
seq(from=1, to=10, length.out = 100)

#operando o vetor
vetor[4]
vetor[1:4] * 2
vetor * 2

vetor = vetor[1:4]
vetor
vetor[-1]

#criando matrizes
m = matrix(1:25, ncol=5, nrow=5, byrow=TRUE)
m

rbind(vetor, vetor)
cbind(vetor, vetor)

#acessando os conteudos da matriz
m[1,]
m[,1]

m[1:2,]

#plotando a matriz
matplot(m, type = "l")

#data frames
LETTERS
letters

123 -> x
x
dados <- data.frame(col1 = 1:10, col2 = 1:10, 
                    col3 = LETTERS[1:10], 
                    stringsAsFactors = FALSE)

dados
class(dados)
str(dados)

#acessando o data.frame
dados$col1
dados$col1[3]
dados[1,3] = "J"
dados

#lista
lista = list(m, dados)
lista

lista[[1]][2,2]
lista[[2]]$col3[3]

#================================================
