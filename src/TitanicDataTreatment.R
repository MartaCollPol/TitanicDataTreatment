## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('normtest')) install.packages('normtest'); library('normtest')
if (!require('car')) install.packages('car'); library('car')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')


## ---------------------------------------------------------------------------------------------------------------------------------------------
df <- read.csv('../data/train.csv',stringsAsFactors = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------
head(df)


## ---------------------------------------------------------------------------------------------------------------------------------------------
str(df)


## ---------------------------------------------------------------------------------------------------------------------------------------------
df$Survived <- factor(df$Survived)
df$Pclass <- factor(df$Pclass)
df$Sex <- factor(df$Sex)
df$SibSp <- factor(df$SibSp)
df$Parch <- factor(df$Parch)
df$Embarked <- factor(df$Embarked)


## ---------------------------------------------------------------------------------------------------------------------------------------------
irrelevant <- c("Cabin", "Ticket", "Name", "PassengerId")
titanic <- df[ , -which(names(df) %in% irrelevant)]


## ---------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(titanic))
colSums(titanic=="")


## ---------------------------------------------------------------------------------------------------------------------------------------------
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm=T)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Determinamos la categoría que se repite con mayor frecuencia
table(titanic$Embarked)


## ---------------------------------------------------------------------------------------------------------------------------------------------
titanic$Embarked[titanic$Embarked == ""] <- "S"


## ---------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(titanic))
colSums(titanic=="")


## ---------------------------------------------------------------------------------------------------------------------------------------------
summary(titanic)


## ---- out.width="50%"-------------------------------------------------------------------------------------------------------------------------
ggplot(titanic) +
  aes(x = "", y = Fare) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------------------------------------------------
head(titanic[titanic$Fare > 150, ])


## ---------------------------------------------------------------------------------------------------------------------------------------------
titanic <- subset(titanic, titanic$Fare < 500)


## ---------------------------------------------------------------------------------------------------------------------------------------------
write.csv(titanic, "../data/Titanic_clean.csv", row.names = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Agrupación por género
titanic.men   <- subset(titanic, Sex == "male")
titanic.women <- subset(titanic, Sex == "female")



## ---- out.width="50%"-------------------------------------------------------------------------------------------------------------------------

p1 <- ggplot(titanic, aes(sample = Age)) + stat_qq() + 
        stat_qq_line(colour= "red") +
        ylab("Sample Quantiles") + xlab("Theoretical Quantiles") +
        ggtitle("Normal Q-Q Plot para Age")

#[1]
p2 <- ggplot(titanic, aes(x = Age)) + 
        geom_histogram(aes(y = ..density..), bins = 30,
                       colour = 1, fill = "white") +
        geom_density(lwd = 1, colour = 4,
                     fill = 4, alpha = 0.25) +
        ggtitle("Distribución y densidad de Age")

grid.arrange(p1,p2,ncol = 2, nrow = 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------
#[2]
## Shapiro test
shapiro.test(titanic$Age)
## Jarque Bera test
jb.norm.test(titanic$Age)
## Forsini test
frosini.norm.test(titanic$Age)


## ---------------------------------------------------------------------------------------------------------------------------------------------
leveneTest(y = titanic$Age, group = titanic$Survived, center = "median")


## ---- out.width="50%"-------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(titanic, aes(sample = Fare)) + stat_qq() + 
        stat_qq_line(colour= "red") +
        ylab("Sample Quantiles") + xlab("Theoretical Quantiles") +
        ggtitle("Normal Q-Q Plot para Fare")

#[1]
p2 <- ggplot(titanic, aes(x = Fare)) + 
        geom_histogram(aes(y = ..density..), bins = 30,
                       colour = 1, fill = "white") +
        geom_density(lwd = 1, colour = 4,
                     fill = 4, alpha = 0.25) +
        ggtitle("Distribución y densidad de Fare")
grid.arrange(p1,p2,ncol = 2, nrow = 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------
#[2]
## Shapiro test
shapiro.test(titanic$Fare)
## Jarque Bera test
jb.norm.test(titanic$Fare)
## Forsini test
frosini.norm.test(titanic$Fare)


## ---------------------------------------------------------------------------------------------------------------------------------------------
leveneTest(y = titanic$Fare, group = titanic$Survived, center = "median")


## ---------------------------------------------------------------------------------------------------------------------------------------------
with(titanic, chisq.test(Survived, Sex))


## ---------------------------------------------------------------------------------------------------------------------------------------------
with(titanic, chisq.test(Survived, Pclass))


## ---------------------------------------------------------------------------------------------------------------------------------------------
with(titanic, chisq.test(Survived, SibSp))


## ---------------------------------------------------------------------------------------------------------------------------------------------
with(titanic, chisq.test(Survived, Parch))


## ---------------------------------------------------------------------------------------------------------------------------------------------
with(titanic, chisq.test(Survived, Embarked))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Histogramas
d.Sex <- ggplot(data = titanic, aes(x=Sex, fill=Survived))+geom_bar()+
              ggtitle("Sobrevivir en función del género") + 
              theme(plot.title = element_text(size = 10))

d.Pclass <- ggplot(data = titanic, aes(x=Pclass, fill=Survived))+geom_bar()+
              ggtitle("Sobrevivir en función de la classe socio económica") +
              theme(plot.title = element_text(size = 10)) 

d.SibSp <- ggplot(data = titanic, aes(x=SibSp, fill=Survived))+geom_bar()+
              ggtitle("Sobrevivir en función de tener a
                      bordo hermanos o esposos/as") +
              theme(plot.title = element_text(size = 10))

d.Parch <- ggplot(data = titanic, aes(x=Parch, fill=Survived))+geom_bar()+
              ggtitle("Sobrevivir en función de tener a bordo padres o hijos") +
              theme(plot.title = element_text(size = 10))

d.Embarked <- ggplot(data = titanic, aes(x=Embarked, fill=Survived))+geom_bar()+
                ggtitle("Sobrevivir en función del puerto de embarque") +
                theme(plot.title = element_text(size = 10))

grid.arrange(d.Sex, d.Pclass, d.SibSp, d.Embarked, d.Parch,
             ncol = 2, nrow = 3)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Se calculan los diferentes Odds-Ratios

# Tabla de frecuencias
a <- as.data.frame(table(titanic$Survived, titanic$Sex))

odds.female <- a[2,]$Freq  / a[1,]$Freq
odds.male <- a[4,]$Freq  / a[3,]$Freq

cat("The OR between the female and the male was", odds.female / odds.male, "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Se calculan los diferentes Odds-Ratios

# Tabla de frecuencias
a <- as.data.frame(table(titanic$Survived, titanic$Pclass))

odds.high.class <- a[2,]$Freq  / a[1,]$Freq
odds.medium.class <- a[4,]$Freq  / a[3,]$Freq
odds.low.class <- a[6,]$Freq  / a[5,]$Freq

cat("The OR between the high class and the low class was", odds.high.class / 
                                                           odds.low.class, "\n")
cat("The OR between the medium class and the low class was", odds.medium.class / 
                                                          odds.low.class, "\n")
cat("The OR between the high class and the medium class was", odds.high.class / 
                                                        odds.medium.class, "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Tabla de frecuencias
a <- as.data.frame(table(titanic$Survived, titanic$SibSp))

odds.zero.SibSp <- a[2,]$Freq  / a[1,]$Freq
odds.one.SibSp <- a[4,]$Freq  / a[3,]$Freq
odds.two.SibSp <- a[6,]$Freq  / a[5,]$Freq

cat("The OR between two SibSp and zero SibSp", odds.two.SibSp / 
                                               odds.zero.SibSp, "\n")
cat("The OR between one SibSp and two SibSp", odds.one.SibSp / 
                                              odds.two.SibSp, "\n")
cat("The OR between one SibSp and zero SibSp", odds.one.SibSp / 
                                               odds.zero.SibSp, "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Tabla de frecuencias
a <- as.data.frame(table(titanic$Survived, titanic$Parch))

odds.zero.Parch <- a[2,]$Freq  / a[1,]$Freq
odds.one.Parch <- a[4,]$Freq  / a[3,]$Freq
odds.two.Parch <- a[6,]$Freq  / a[5,]$Freq

cat("The OR between two Parch  and zero Parch ", odds.two.Parch  / 
      odds.zero.Parch , "\n")
cat("The OR between one Parch  and two Parch ", odds.one.Parch  / 
      odds.two.Parch , "\n")
cat("The OR between one Parch  and zero Parch ", odds.one.Parch  / 
      odds.zero.Parch , "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Tabla de frecuencias
a <- as.data.frame(table(titanic$Survived, titanic$Embarked))

odds.C <- a[2,]$Freq  / a[1,]$Freq
odds.Q <- a[4,]$Freq  / a[3,]$Freq
odds.S <- a[6,]$Freq  / a[5,]$Freq

cat("The OR between embarking in Q and embarking in S ", odds.Q  / 
      odds.S , "\n")
cat("The OR between embarking in C and embarking in Q ", odds.C  / 
      odds.Q , "\n")
cat("The OR between embarking in C and embarking in S ", odds.C  / 
      odds.S , "\n")


## ---- out.width="50%"-------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(titanic.women, aes(x = Age)) + 
        geom_histogram(aes(y = ..density..), bins = 30,
                       colour = 1, fill = "white") +
        geom_density(lwd = 1, colour = 4,
                     fill = 4, alpha = 0.25) +
        scale_y_continuous(limit = c(0,0.12)) +
        ggtitle("Distribución y densidad de la edad de las mujeres")

p2 <- ggplot(titanic.men, aes(x = Age)) + 
        geom_histogram(aes(y = ..density..), bins = 30,
                       colour = 1, fill = "white") +
        geom_density(lwd = 1, colour = 4,
                     fill = 4, alpha = 0.25) +
        scale_y_continuous(limit = c(0,0.12)) +
        ggtitle("Distribución y densidad de la edad de los hombres") 

grid.arrange(p1,p2,ncol = 2, nrow = 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(titanic.women$Age)


## ---------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(titanic.men$Age)


## ---------------------------------------------------------------------------------------------------------------------------------------------
var.test(titanic.women$Age, titanic.men$Age)


## ---------------------------------------------------------------------------------------------------------------------------------------------
t.test(titanic.women$Age, titanic.men$Age, alternative = "l", var.equal = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------

set.seed(23)

trainIndex=createDataPartition(titanic$Survived, p=0.75)$Resample1

titanic_train=titanic[trainIndex, ]
titanic_test= titanic[-trainIndex, ]

cat("The train set has", nrow(titanic_train), "rows\n\n")
cat("The train set has", nrow(titanic_test), "rows")


## ---------------------------------------------------------------------------------------------------------------------------------------------
for (var in colnames(titanic_train)){
  if (var != "Survived"){
    formula <- as.formula(sprintf("Survived ~ %s", var))
    model <- glm(formula, family = binomial(link=logit), data=titanic_train)
  
    titanic_test$pred <- predict(model, titanic_test, type = "response")
    titanic_test$pred_d <- ifelse(titanic_test$pred < 0.5, 0, 1)
    titanic_test$is_ok <- ifelse(titanic_test$Survived == titanic_test$pred_d, 
                               1, 0)
    accuracy <- sum(titanic_test$is_ok)/nrow(titanic_test)
    cat("The accuracy of the model with ", var, " is ", accuracy, "\n")
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
for (var in colnames(titanic_train)){
  if (var != "Sex" && var != "Survived"){
    formula <- as.formula(sprintf("Survived ~ Sex * %s", var))
    model <- glm(formula, family = binomial(link=logit), data=titanic_train)
  
    titanic_test$pred <- predict(model, titanic_test, type = "response")
    titanic_test$pred_d <- ifelse(titanic_test$pred < 0.5, 0, 1)
    titanic_test$is_ok <- ifelse(titanic_test$Survived == titanic_test$pred_d, 
                               1, 0)
    accuracy <- sum(titanic_test$is_ok)/nrow(titanic_test)
    cat("The accuracy of the model with Sex and ", var, " is ", accuracy, "\n")
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
for (var in colnames(titanic_train)){
  if (var != "Sex" && var != "Survived" && var != "SibSp" && var != "Parch"){
    formula <- as.formula(sprintf("Survived ~ Sex * SibSp + %s", var))
    model <- glm(formula, family = binomial(link=logit), data=titanic_train)
  
    titanic_test$pred <- predict(model, titanic_test, type = "response")
    titanic_test$pred_d <- ifelse(titanic_test$pred < 0.5, 0, 1)
    titanic_test$is_ok <- ifelse(titanic_test$Survived == titanic_test$pred_d, 
                               1, 0)
    accuracy <- sum(titanic_test$is_ok)/nrow(titanic_test)
    cat("The accuracy of the model with Sex, SibSp and ", var, " is ", accuracy,
        "\n")
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
for (var in colnames(titanic_train)){
  if (var != "Sex" && var != "Survived" && var != "SibSp" && var != "Parch"
      && var != "Pclass"){
    formula <- as.formula(sprintf("Survived ~ Sex * SibSp + Pclass * %s", var))
    model <- glm(formula, family = binomial(link=logit), data=titanic_train)
  
    titanic_test$pred <- predict(model, titanic_test, type = "response")
    titanic_test$pred_d <- ifelse(titanic_test$pred < 0.5, 0, 1)
    titanic_test$is_ok <- ifelse(titanic_test$Survived == titanic_test$pred_d, 
                               1, 0)
    accuracy <- sum(titanic_test$is_ok)/nrow(titanic_test)
    cat("The accuracy of the model with Sex, SibSp, Pclass and ", var, " is ",
        accuracy, "\n")
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
for (var in colnames(titanic_train)){
  if (var != "Sex" && var != "Survived" && var != "SibSp" && var != "Parch"
      && var != "Pclass" && var != "Age"){
    formula <- as.formula(sprintf("Survived ~ Sex * SibSp + Pclass * Age * %s",
                                  var))
    model <- glm(formula, family = binomial(link=logit), data=titanic_train)
  
    titanic_test$pred <- predict(model, titanic_test, type = "response")
    titanic_test$pred_d <- ifelse(titanic_test$pred < 0.5, 0, 1)
    titanic_test$is_ok <- ifelse(titanic_test$Survived == titanic_test$pred_d, 
                               1, 0)
    accuracy <- sum(titanic_test$is_ok)/nrow(titanic_test)
    cat("The accuracy of the model with Sex, SibSp, Pclass, Age and ", var,
        " is ", accuracy, "\n")
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
model.final <- glm(Survived ~ Sex * SibSp + Pclass * Age, 
                   family = binomial(link=logit), data=titanic_train)
titanic_test$pred <- predict(model.final, titanic_test, type = "response")
titanic_test$pred_d <- ifelse(titanic_test$pred < 0.5, 0, 1)
titanic_test$is_ok <- ifelse(titanic_test$Survived == titanic_test$pred_d, 1, 0)
accuracy.final <- sum(titanic_test$is_ok)/nrow(titanic_test)
cat("The accuracy of the final model is ", accuracy.final)


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------
Contribuciones <- c("Investigación previa", "Redacción de las respuestas ", 
                    "Desarrollo código ")
Firma <- c("Marta Coll Pol, Manuel De Blas Pino",
           "Marta Coll Pol, Manuel De Blas Pino", "Marta Coll Pol, Manuel De Blas Pino")

d <- data.frame(Contribuciones, Firma)

d

