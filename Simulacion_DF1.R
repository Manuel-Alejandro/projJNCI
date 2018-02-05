rm(list = ls())

setwd("C:/Users/Juan Diego Mejía/OneDrive/Otros")

library(ggplot2)


N = 100
m = 9
id = seq(1, N)
edad = c(abs(rnorm(m, 4.5, 4)), rnorm(N - m, 45, 10))
patologia = c(rep('F73', m), sample(c('G56.0', 'E10'), replace = T, N-m))
sala = sample(c(1, 2, 3), replace = T, N)
IMC = c(rnorm(m, 22), rnorm(N-m, 26))
tiempo = rexp(N, 7)


DF1 = data.frame(id, edad, patologia, sala, IMC, tiempo)

plot(IMC, edad)


#write.csv(DF1, file = "DF1.csv",row.names=FALSE, na="")


#################################################

PCLInicial = rep(0, N)
for( i in 1:N){
  if(patologia[i] == 'F73')
    PCL = runif(1, 0.40, 0.60)
  if(patologia[i] == 'G56.0')
    PCL = runif(1, 0.1, 0.2)
  if(patologia[i] == 'E10')
    PCL = runif(1, 0.3, 0.4)
  PCLInicial[i] = PCL
}

PCLFinal = rep(0, N)
for( i in 1:N){
  if(patologia[i] == 'E10')
    PCL = PCLInicial[i] + rnorm(1, 0.1, 0.005)
  else
    PCL = PCLInicial[i] + runif(1, 0, 0.02)
  PCLFinal[i] = PCL
}

qplot(PCLInicial, PCLFinal, colour = patologia)

modelo = lm(PCLFinal ~ PCLInicial + patologia + edad)
summary(modelo)

modeloReducido = lm(PCLFinal ~ PCLInicial + patologia)

anova(modeloReducido, modelo)

DF2 = data.frame(id, edad, patologia, PCLInicial, PCLFinal)
write.csv(DF2, file = "DF2.csv",row.names=FALSE, na="")
