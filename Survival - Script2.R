library(survival)
mydata<-lung
names(lung)
head(lung)

## Step (1)
## Create the Survival Object
## we need status = 0 --> no event and status = 1 --> event happened
recodestatus<-function(x){
  if(x==1){rs=0} ## no event / censored
  if(x==2){rs=1} ## event happened
  return(rs)
}
for(i in 1:length(mydata$status)){
  mydata$recodedStatus[i]<-recodestatus(mydata$status[i])
}
mySurv<-Surv(time=mydata$time, event = mydata$recodedStatus)
class(mySurv)
head(mySurv)


####### B

## single survival curve: no comparisons
myfit<-survfit(mySurv~1) ## signle curve for all patients in 
## the dataset
myfit
median(mydata$time) # valor médio de sobrevivência

### Median survival is the time at which the survivorship 

plot(myfit) # eixo X é o tempo e eixo Y é a probabilidade do evento ocorrer
plot(myfit, conf.int = "none") # Plot sem o intervalo de confiança
abline(h=0.5) # corte em probabilidade 50%
abline(v=310) # mediana encontrada em "myfit"


## specify predictor variable in the formula
myfit<-survfit(mySurv~mydata$sex) # modelagem do modelo de sobrevivência considerando o grupos separados por sexo
myfit # estatística descritiva do modelo
plot(myfit) # Duas linhas, uma sendo para homem e uma sendo para mulher
table(mydata$sex) # Contagem de ocorrências para os sexos

# 1= Homem, 2= Mulher
plot(myfit, col=c("red","blue")) ## red = Male, Blue= female
plot(myfit, conf.int = "both", col=c("red","blue")) # com intervalos de confiança
plot(myfit, col=c("red","blue"))
plot(myfit, col=c("red","blue"), mark=3) ## mark.time=T marked at 
## each censoring time
legend("topright", c("male","female"), col=c("red","blue"), lty=1)
abline(h=0.5)
abline(v=270, col="red")
abline(v=426, col="blue")

# Vemos que a probabilidade da mulher passar para o próximo período sem que o evento ocorra é menor que a probabilidade do homem
# Em outras palavras, a probabilidade do evento ocorrer para a mulher é maior que para o homem
# Mas isso é apenas aleatório ou estatisticamente significante?
survdiff(mySurv~mydata$sex)
# Vemos que o p-valor é menor que 0.05, indicando que é estatisticamente relevante a informação.


# Plotando a função de sobrevivência de modo inverso para facilitar a interpretação:
plot(myfit, fun="event", col=c("red","blue"), mark=3)
# Em termos de comparação, vemos que a probabilidade de a mulher com "200 períodos" conseguir o evento é X vezes maior que a probabilidade do homem


coxph(mySurv~mydata$sex+mydata$age)
# Vemos aqui que as vari´veis Sexo e Idade são significativas para o modelo proposto


# Podemo então utilizar essa variáveis para criar a nossa interpretação do modelo em termos de comparação visto em:
# Basta utilizar os últimos modelos gerados no card encontrado em:
# EVERNOTE > MACHINE LEARNING > SURVIVAL ANALYSIS > 1 - ANÁLISE DE SOBREVIVÊNCIA | PRÁTICA E TEORIA


