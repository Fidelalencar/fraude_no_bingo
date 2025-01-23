

##########
# BINGO

library(ggplot2)
n_apostadores <- 8000 # 2000 apostadores
apostas <- list()
for (i in 1:n_apostadores) { #gerando as cartelas
  cart <- sort(c(sample(1:15, 5, replace = F), # B
                 sample(16:30, 5, replace = F), # I
                 sample(31:45, 5, replace = F), # N
                 sample(46:60, 5, replace = F), # G
                 sample(61:75, 5, replace = F))) # O
  apostas[[i]] <- cart
}
apostas[[298]] # ver a cartela de numero ...

maior_acerto<-0
e<-1
sorteio <- c() # numeros sorteados
bingo <- c(1:75) # numeros dentro globo
acompanhamento <- list()  # os numeros que cada participante ja acertou
qnt_acertos <- c()
rod <- 1  # numero da rodada do sorteio
while (max(qnt_acertos) < 25) {
  s <- sample(bingo, 1, replace = F)
  bingo <- bingo[bingo != s]
  sorteio[rod] <- s
  sorteio <- sort(sorteio)
  
  for(e in 1:n_apostadores) {
    acompanhamento[[e]] <- intersect(sorteio, apostas[[e]])
    qnt_acertos[e] <- length(acompanhamento[[e]])
    maior_acerto <- max(qnt_acertos)
  }
  print(paste0("rodada: ", rod, " ## maior_qnt_acertos: ", max(qnt_acertos)))
  sorteio
  rod <- rod+1
}  
# sorteio
# qnt_acertos
# max(qnt_acertos)

qnt_acertosDF <- data.frame(x=qnt_acertos)
ggplot(data = qnt_acertosDF, aes(x = x)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(qnt_acertosDF$x),
                                  max(qnt_acertosDF$x), by = 1)) +
  labs(
    title = paste0("Histograma acertos na ", rod-1, "a rodada", " e ", n_apostadores, " cartelas."),
    x = "Acertos",
    y = "Frequência"
  ) +
  theme_minimal()

table(qnt_acertosDF$x)




# haverá um bingo. nele o globo de numero conterá 75 bolas e as cartelas conterão 25 numeros. os numeros das cartelas são organizados da seguinte maneira: 5 entre 1 e 15. 5 entre 16 e 30. 5 entre 31 e 45. 5 entre 46 e 60 e 5 entre 61 e 75. qual a chance de o bingo ser vencido no 49 sorteio ou antes?

# haverá um bingo. foram vendidas 5 mil cartelas. nesse o globo de bolas numeradas conterá 75 bolas e as cartelas conterão 25 numeros cada. os números das cartelas são organizados da seguinte maneira: 5 entre 1 e 15. 5 entre 16 e 30. 5 entre 31 e 45. 5 entre 46 e 60 e 5 entre 61 e 75. qual a chance de o bingo ser vencido no 54 sorteio ou antes?




















