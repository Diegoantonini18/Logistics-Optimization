setwd("C:/Users/Usuario/Desktop/TP OPE I")
library(lpSolve)
library(linprog)

datos=read.csv("datos.csv",T,",")
deposito_ciudad=read.csv("deposito-ciudad.csv",T,",")
fabrica_ciudad=read.csv("fabrica-ciudad.csv",T,",")
fabrica_deposito=read.csv("fabrica-deposito.csv",T,",")

terminacion_padron=41

datos_temp=datos[,-1]
mis_datos=datos_temp[terminacion_padron+1,]
rhs=c(mis_datos[,-(6:10)],rep(0,8))

fabrica=setNames(data.frame(matrix(ncol = 5, nrow = 1)), c("x1", "x2", "x3","x4","x5"))

deposito_ciudad_temp=data.frame(t(deposito_ciudad[,-1]))
colnames(deposito_ciudad_temp) = paste0('dep_ciud_', colnames(deposito_ciudad_temp))
deposito_ciudad_row=data.frame(lapply(deposito_ciudad_temp, function(x) t(data.frame(x))))
deposito_ciudad_row=deposito_ciudad_row*-0.1


fabrica_ciudad_temp=data.frame(t(fabrica_ciudad[,-1]))
colnames(fabrica_ciudad_temp) = paste0('fab_ciud_', colnames(fabrica_ciudad_temp))
fabrica_ciudad_row=data.frame(lapply(fabrica_ciudad_temp, function(x) t(data.frame(x))))
fabrica_ciudad_row=fabrica_ciudad_row*-0.2


fabrica_deposito_temp=data.frame(t(fabrica_deposito[,-1]))
colnames(fabrica_deposito_temp) = paste0('fab_dep_', colnames(fabrica_deposito_temp))
fabrica_deposito_row=data.frame(lapply(fabrica_deposito_temp, function(x) t(data.frame(x))))
fabrica_deposito_row=fabrica_deposito_row*-0.1


remove(deposito_ciudad_temp,fabrica_ciudad_temp,fabrica_deposito_temp)
merge_df=cbind(fabrica,fabrica_ciudad_row,deposito_ciudad_row,fabrica_deposito_row)
main=merge_df[-1,]

## Restricciones fabrica
for (i in 1:5){
  for (j in 1:5){
    ifelse(j==i,main[i,j]<-1,main[i,j]<-0)
  }
}

## Restricciones demanda
for (i in 6:15){
    for (k in seq(i,70+i,10)) {
      main[i,k]=1
    }
    
}

## Restricciones deposito
for (i in 16:18){
  for (k in seq(70+i,82+i,3)) {
    main[i,k]=1
  }
  
}

## Balances fabricas
aux1=0
aux2=0
for (i in 19:23){
  main[i,(6+aux1):(15+aux1)]=-1
  aux1=aux1+10
  main[i,(86+aux2):(88+aux2)]=-1
  aux2=aux2+3
  for (j in 1:5){
    ifelse(i-18==j,main[i,j]<-1,main[i,j]<-0)
  }
}

## Balances depositos
aux1=0
aux2=0
for (i in 24:26){
  main[i,(56+aux1):(65+aux1)]=1
  aux1=aux1+10
  main[i,c(86+aux2,89+aux2,92+aux2,95+aux2,98+aux2)]=-1
  aux2=aux2+1
}


## Relleno el resto con 0
main[is.na(main)] = 0

## Armo el funcional
z=merge_df
for (i in 1:5) {
  z[1,i]=350-mis_datos[1,5+i]
  
}

main_matrix=as.matrix.data.frame(main)
z=as.matrix.data.frame(z)
## Inecuaciones
inec = c(rep("<=",18),rep("<=",8))

lp4=lp("max",z,main_matrix,inec,rhs,
       all.int = T,
       compute.sens = TRUE)
lp4$sens.coef.from


solution=data.frame(colnames(z),lp4$solution)
lp4$sens.coef.to

solopt = solveLP(z, rhs, main_matrix, maximum = TRUE, inec)
solopt

# sum(mis_datos[,1:5]*mis_datos[,6:10])
sum(as.vector(solution[6:100,2])*z[6:100])
sum(solution[1:5,2]*350)
sum(as.vector(solution[1:100,2])*z[6:100])
