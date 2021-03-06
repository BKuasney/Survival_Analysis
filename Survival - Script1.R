library("survival")

mydata <- read.csv("C:/Users/bkuasney/Desktop/Machine Learning A-Z/Survival Analysis/survival_unemployment.csv")
attach(mydata)
names(mydata)
# spell (quantos per�odos sem emprego)
# event (se encontrou ou continua sem emprego)


# Definindo vari�vel
time = spell
event = event
x = cbind(logwage, ui, age) # vari�veis independentes
group = ui # vari�vel categ�rica


# Estat�stica Descritiva
summary(time)
summary(event)
summary(x)
summary(group)


# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time, event)~1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
# Aqui fazemos a mesma an�lise por grupo, 2 grupos distintos (homem e mulher por exemplo)
kmsurvival2 <- survfit(Surv(time, event)~group)
summary(kmsurvival2)
plot(kmsurvival2, xlab="Time", ylab="Survival Probability")



# A an�lise de sobreviv�ncia at� os exemplos acima consideram an�lises de maneira geral, sem quaisquer compara��es.
# A partir das an�lises de baixo, temos as vari�veis categorizadas, ou seja, as an�lises passam a ser de compara��o, onde um valor sozinho apenas n�o faz diferen�a.
# Exemplo:
# Se temos grupo A = 0 e grup B = 1 e temos que o resultado da regress�o de an�lise de sobreviv�ncia abaixo �: e^B = 0,8, ent�o as taxas de falhas que ocorrem para o grupo B � 80% com que as falhas ocorrem para o Grupo A.
# Portanto, grupo A falha 1-0,8 vezes mais que o Grupo B.


# Exmeplo 2:
# Se Grupo B = 1,20, ent�o, na an�lise de regress�o teremos que o acr�scimo de 1 ano na idade aumenta a taxa de falha em 20%, pois 1,20+1 = 2,20 = 20% de acr�scimo.



# Exponential, Weibull, and log-logistic parametric model coefficients
exponential = survreg(Surv(time, event)~x, dist = "exponential")
summary(exponential)

weibull = survreg(Surv(time, event) ~ x, dist = "weibull")
summary(weibull)


loglogistic = survreg(Surv(time, event) ~ x, dist = "loglogistic")
summary(loglogistic)
