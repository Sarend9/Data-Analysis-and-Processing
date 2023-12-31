# Вопрос 1: Три группы испытуемых изучали один и тот же предмет
# разными методами: индуктивное обучение (группа 1),
# дедуктивное обучение (группа 2) и их комбинация (группа 3).
# В каждую из групп вошли испытуемые двух разных
# типов: “экспериментаторы” (тип 1), любящие исследовать
# новые объекты, и “теоретики” (тип 2), предпочитающие
# анализировать уже имеющиеся сведения об объекте. 
# В конце обучения было проведено тестирование,
# измеряющее процент усвоенных знаний: test.dat.

# 1) Дано:
file <- "test.dat"
x1 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)
str(x1)  #  вывод сведений о структуре объекта

test <- x1

# Вывод переменных:
print("Значение переменной file:")
print(file)

print("Структура переменной x1:")
print(str(x1))

# 2) Проверьте гипотезу о том, что метод обучения по-разному
# влияет на усвоение знаний в зависимости от типа обучающегося.

# индуктивное обучение (группа 1), дедуктивное обучение (группа 2) и их комбинация (группа 3)
#"экспериментаторы" (тип 1) и "теоретики" (тип 2)

test[, 1] <- factor(test[, 1])
test[, 2] <- factor(test[, 2])
model5 <- aov(score ~ method * type, data = test) # зависимость результата от метода и типа + взаимодействие факторов
summary(model5) #df2 - rediduals df

# Вывод переменных:
print("Значение переменной test:")
print(head(test))

n2m_t <- 4536 / (562 + 27 + 4536 + 10817)

n2m_t <- 3259 / (640 + 106 + 3259 + 9001)
n2m_t
tukey_result <- TukeyHSD(model5)
print(tukey_result)
# Проверка на наличие результатов
if (length(tukey_result) > 0) {
  comparisons <- summary(tukey_result)
  if ("p adj" %in% colnames(comparisons)) {
    significant_comparisons <- comparisons[comparisons[,"p adj"] < 0.05, ]
    if (nrow(significant_comparisons) > 0) {
      cat("Статистически значимые различия:\n")
      for (i in 1:nrow(significant_comparisons)) {
        group1 <- rownames(significant_comparisons)[i]
        p_val <- significant_comparisons[i, "p adj"]
        cat(paste("#", group1, "(", p_val, ")", "\n"))
      }
    } else {
      cat("Нет статистически значимых различий.\n")
    }
  } else {
    cat("Нет статистически значимых различий.\n")
  }
} else {
  cat("Нет статистически значимых различий.\n")
}
#индуктивное обучение: экспериментаторы — теоретики (1:2-1:1)
#комбинированное обучение: экспериментаторы — теоретики (3:2-3:1)
#экспериментаторы: индуктивное — дедуктивное обучение (1:1-2:1)
#экспериментаторы: дедуктивное — комбинированное обучение (3:1-1:1) ?




# Задание №2
tapply(test$score, INDEX = list(test$type, test$method), FUN = mean)  # средние по каждому условию
levels(test$method) <- c("индуктивное", "дедуктивное", "комбинированное") #переименование 
levels(test$type) <- c("экспериментаторы", "теоретики") #переименование
par(mfrow = c(1, 1))   # диаграммы в 2 ряда по 2 столбца
interaction.plot(x.factor = test$method,  # фактор по оси х
                 trace.factor = test$type,  # фактор, отображаемый линиями
                 response = test$score,  # зависимая переменная
                 trace.label = 'тип',  # заголовок легенды
                 fixed = TRUE,  # сохранять в легенде порядок уровней фактора
                 col = c('gray', 'red'),  # 
                 type = 'b',  # тип изображения: линии и точки
                 lwd = 2,  # ширина линий
                 pch = 16,  # тип символа для отображения точек
                 cex = 1.5,  # размер символа
                 xlab = "метод",
                 ylab = "среднее значение балла",
                 main = "Диаграмма взаимодействия факторов"
)
tapply(test$score, INDEX = list(test$type, test$method), FUN = mean)  # средние по каждому условию



# Средний процент усвоенных знаний экспериментаторов при индуктивном обучении составил = 78.71, при дедуктивном = 69.24 и при комбинированном = 77.99, а теоретиков — соответственно 64.09, 80.15 и 76.07.
# 
# 
# Для проверки гипотезы о том, что метод обучения по-разному влияет на усвоение знаний в зависимости от типа обучающегося, а также о взаимодействии этих факторов был проведён двухфакторный дисперсионный анализ.
# 
# 
# Результат для эффекта типа обучающегося и метода обучения оказался статистически значимым: F(2,114) = 20.638, p = 2.24e-08. Результат для основного эффекта метода обучения также оказался значимым: F(2,114) = 1.338, p = 0.0199.
# 
# 
# Попарные пост-хок сравнения при помощи критерия HSD Тьюки показали достоверные различия между экспериментаторами и теоретиками при индуктивном обучении (p < 0.001), но не для комбинированного обучения  (p = 0.9834787), а также между индуктивным и дедуктивным обучением у экспериментаторов (p = 0.0128252), но не между индуктивным и комбинированным обучением (p = 0.9998462).
# 
# 
# Обнаружен умеренный эффект зависимости разницы между методом обучения и типом обучающегося (η2 = 0.25): статистически достоверное различие между теоретиками и экспериментаторами обнаружено лишь при индуктивном обучении, тогда как при комбинированном обучении влияние не достигает статистически значимого уровня. Обнаружена также значимая разница между индуктивным и дедуктивным обучением у экспериментаторов, в то время как аналогичной разницы между индуктивным и комбинированным обучением у экспериментаторов не выявлено.
# 





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


