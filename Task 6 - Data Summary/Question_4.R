# Вопрос 4: Результаты ежедневных измерений температуры на метеостанции,
#           в °C, сохранены в файле: data_file.dat


file <- "data_file4.dat"
x4.6 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)

sort(x4.6$temperature)

par(mfrow = c(2,1))

hist(x4.6$temperature, breaks = 15)

x4.6.boxlpot <- boxplot(x4.6$temperature,
                      horizontal = TRUE,  # горизонтальная ориентация диаграммы
                      col = "cornsilk1",
                      main = "Сводные характеристики распределения уровня температуры",
                      xlab = "температура, °C",
                      ylab = 'измерение температуры'
)
print(str(x4.6.boxlpot))

# Определите, целесообразно ли с точки зрения дальнейшего анализа очистить данные от выбросов. 
# Если да, то исключите соответствующие наблюдения.
# Затем выберите адекватные сводные характеристики распределения и найдите их:

print('Изначальные данные')
print(sort(x4.6$temperature)); cat("\n")

print('Очищенные данные от выбросов:')
x4.6.sort <- sort(x4.6$temperature[x4.6$temperature>(-10) & x4.6$temperature<17.4])
print(x4.6.sort); cat("\n")

library(moments)  #загрузка пакета moments
x4.6.result <-list(
  "среднее арифметическое:"     = x4.6.mean   <- mean(x4.6.sort),
  "стандартное отклонение:"     = x4.6.sd     <- sqrt(sum((x4.6.sort - mean(x4.6.sort)) ^ 2)/length(x4.6.sort))  ,
  "коэффициент асимметрии γ1:"  = x4.6.skew   <- skewness(x4.6.sort, na.rm = TRUE)                        ,
  "коэффициент эксцесса   γ2:"  = x4.6.excess <- kurtosis(x4.6.sort, na.rm = TRUE)-3
)

print(x4.6.result)

