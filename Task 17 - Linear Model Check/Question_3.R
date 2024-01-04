# Задание №3

# 1) Дано:
file <- "cold.dat"
x3   <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)
str(x3)  # вывод сведений о структуре объекта
data<-x3

data.st <- apply(data, MARGIN = 2, FUN = scale)
data.st<- as.data.frame(data)
#install.packages("car")
library(car)
model0 <- lm(cold ~  contacts + stress + demand + mood, data = data.st) # предикторы: контакт, стресс, оценку треб и эмоц фон
model1 <- lm(cold ~ contacts + stress, data = data.st)  # предикторы: контакт и стресс
anova(model1, model0)  # проверка гипотезы

vif(model0)  # вычисление VIF для всех предикторов в модели

step_result <- step(model0) # Выбор лучшей модели
b_stress <- coef(step_result)["stress"][[1]]
print(paste("b_stress =", b_stress))

model3 <- lm(cold ~ stress + mood, data = data.st)  # Выбранная модель = предикторы: требования и стресс
summary_model3 <- summary(model3) # R2 выбранной модели + предиктора стресс
B_stress <- sd(data$stress)/sd(data$cold)*b_stress  # Cтандартизованный вес предиктора стресс
print(paste("B_stress =", B_stress))

