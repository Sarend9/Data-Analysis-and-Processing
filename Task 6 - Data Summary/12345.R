x1.boxplot <- function(vector){
  v <- boxplot(vector,
               horizontal = TRUE,  # горизонтальная ориентация диаграммы
               col = "orange",
               main = "Сводные характеристики 
распределения веса участников соревнования",  # заголовок в 2 строки
               xlab = "вес, кг",
               ylab = 'соревнование 1'
  )
  abline(v=mean(vector), col = 'red', lwd = 1)
  text  (x = mean(vector), y = 1.3, 
         'среднее
арифметическое', 
         col = 'red')
  return(v)
}

#Вопрос 1: Результаты измерений веса (в кг) участников соревнований сохранены в файле: data_file1.dat
x1 <- read.csv("C:/Users/ak179/OneDrive/Магистратура/2 КУРС/1 СЕМЕСТР/Лекции (2 курс)/Язык R (шпоры и задачи)/Скрипты/Задание 6/data_file1.dat")

print("меры изменчивости (разброса, рассеяния):")
range(x1$weight)
cat(max(range(x1$weight))-min(range(x1$weight)),"        - размах range:")
cat("\n")

var(x1$weight)
variance <- sum((x1$weight - mean(x1$weight)) ^ 2)/length(x1$weight)
cat(variance, "              - дисперсия σ2:") 
cat("\n")

sd(x1$weight)
sqrt(variance)
cat(sqrt(variance), "           - стандартное отклонение σ:") 
cat("\n")

IQR(x1$weight)
cat(IQR(x1$weight), "          - межквартильный размах IQR:") 
cat("\n\n")

sum((x1$weight - mean(x1$weight)) ^ 2)/ length(x1$weight)

#install.packages("moments")
library(moments)  #загрузка пакета moments

print("меры формы:")
cat(skewness(x1$weight, na.rm = TRUE), "- коэффициент асимметрии γ1:"            ) #sum((x1$weight - mean(x1$weight)) ^ 3)/(length(x1$weight)*(sqrt(variance))^3)
cat("\n")
#sum((x1$weight - mean(x1$weight)) ^ 3)/(length(x1$weight)*(sqrt(variance))^3)

cat(kurtosis(x1$weight, na.rm = TRUE)-3, "- коэффициент эксцесса   γ2:"        ) #sum((x1$weight - mean(x1$weight)) ^ 4)/(length(x1$weight)*(sqrt(variance))^4)-3
cat("\n\n")

# Вопрос 2: Постройте диаграмму размаха по данным измерения веса 
# из предыдущего вопроса, ориентировав её горизонтально. 
# Добавьте на диаграмму среднее арифметическое в виде цветной линии.

x1.boxplot(x1$weight)



# Кратко опишите по диаграмме характеристики распределения, 
# указывая, как они на ней представлены.

# По данной диаграмме
