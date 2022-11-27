#Simulación de las conexiones.
etiquetas<-c("Home","Admin","Staff","Student","Library",
             "Dept","Alumni","Books","F_inf")
Matriz_t<-matrix(NA,9,9)
set.seed(1234)
for (i in 1:9){
  Matriz_t[i,]<-rbinom(9,1,0.6)
}
Matriz_t
#Calculo de la matriz de transiciones
f<-NULL
for(i in 1:9){
  f[i]<-1/sum(Matriz_t[i,])
}
Matriz_t<-Matriz_t*f
Matriz_t

#Como no se obtuvo ninguna pagina con ninguna conexion se 
#crea arbitrariamente dos de ellas que no tengan ninguna.
Matriz_t[7,]<-1/9,
Matriz_t[8,]<-1/9,
Matriz_t
#Calculadno la matriz estocastica irreducible \bar{p_{ij}}
E<-matrix(1/9,9,9)
hp_ij<-0.85*Matriz_t+0.15*E
#Calculando la distrbucion estacionaria
library(markovchain)
mc3 = new("markovchain", transitionMatrix = hp_ij, 
          states = c("Home","Admin","Staff","Student","Library",
                     "Dept","Alumni","Books","F_inf"), 
          name = "Matriz de transición Caso PE")
round(steadyStates(mc3),3)
