library("survival")

mydata <- read.csv("C:/Users/bkuasney/Desktop/Machine Learning A-Z/Survival Analysis/survival_unemployment.csv")
attach(mydata)
names(mydata)
# spell (quantos períodos sem emprego)
# event (se encontrou ou continua sem emprego)


# Definindo variável
time = spell
event = event
x = cbind(logwage, ui, age) # variáveis independentes
group = ui # variável categórica


# Estatística Descritiva
summary(time)
summary(event)
summary(x)
summary(group)


# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time, event)~1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
# Aqui fazemos a mesma análise por grupo, 2 grupos distintos (homem e mulher por exemplo)
kmsurvival2 <- survfit(Surv(time, event)~group)
summary(kmsurvival2)
plot(kmsurvival2, xlab="Time", ylab="Survival Probability")



# A análise de sobrevivência até os exemplos acima consideram análises de maneira geral, sem quaisquer comparações.
# A partir das análises de baixo, temos as variáveis categorizadas, ou seja, as análises passam a ser de comparação, onde um valor sozinho apenas não faz diferença.
# Exemplo:
# Se temos grupo A = 0 e grup B = 1 e temos que o resultado da regressão de análise de sobrevivência abaixo é: e^B = 0,8, então as taxas de falhas que ocorrem para o grupo B é 80% com que as falhas ocorrem para o Grupo A.
# Portanto, grupo A falha 1-0,8 vezes mais que o Grupo B.


# Exmeplo 2:
# Se Grupo B = 1,20, então, na análise de regressão teremos que o acréscimo de 1 ano na idade aumenta a taxa de falha em 20%, pois 1,20+1 = 2,20 = 20% de acréscimo.



# Exponential, Weibull, and log-logistic parametric model coefficients
exponential = survreg(Surv(time, event)~x, dist = "exponential")
summary(exponential)

weibull = survreg(Surv(time, event) ~ x, dist = "weibull")
summary(weibull)


loglogistic = survreg(Surv(time, event) ~ x, dist = "loglogistic")
summary(loglogistic)
