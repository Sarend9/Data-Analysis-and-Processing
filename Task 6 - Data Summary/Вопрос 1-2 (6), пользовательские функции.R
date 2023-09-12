#Вопрос 1: Результаты измерений веса (в кг) участников соревнований сохранены в файле: data_file1.dat
file <- "data_file1-2.dat"
x1.6 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)

#install.packages("moments")
library(moments)  #загрузка пакета moments

x1.6.result <-list(
# меры изменчивости                         
    "размах range:"               = x1.6.range  <- range(x1.6$weight)[2]-range(x1.6$weight)[1],
    "дисперсия σ2:"               = x1.6.var    <- sum((x1.6$weight - mean(x1.6$weight)) ^ 2)/length(x1.6$weight),  
    "стандартное отклонение σ:"   = x1.6.sd     <- sqrt(x1.6.var),
    "межквартильный размах IQR:"  = x1.6.IQR    <- IQR(x1.6$weight),                                                      

# меры формы"                            
    "коэффициент асимметрии γ1:"  = x1.6.skew   <- skewness(x1.6$weight, na.rm = TRUE)     , #sum((x1.6$weight - mean(x1.6$weight)) ^ 3)/(length(x1.6$weight)*(sqrt(x1.6.var))^3)
    "коэффициент эксцесса   γ2:"  = x1.6.excess <- kurtosis(x1.6$weight, na.rm = TRUE)-3     #sum((x1.6$weight - mean(x1.6$weight)) ^ 4)/(length(x1.6$weight)*(sqrt(x1.6.var))^4)-3
)
print((x1.6.result))

# Вопрос 2: Постройте диаграмму размаха по данным измерения веса 
# из предыдущего вопроса, ориентировав её горизонтально. 
# Добавьте на диаграмму среднее арифметическое в виде цветной линии.

hist(x1.6$weight, 10)

x1.6.boxplot <- function(vector){
  v <- boxplot(vector,
               horizontal = TRUE,  # горизонтальная ориентация диаграммы
               col = "cornsilk1",
               main = "Сводные характеристики 
               распределения веса участников соревнования               ",
               xlab = "вес, кг",
               ylab = 'соревнование 1'
  )

  abline(v = mean(vector), col = 'firebrick1', lwd = 1)
  text  (x = mean(vector), y = 1.3, 
         'среднее
          арифметическое          ',
         col = 'firebrick1')
  
  return(v)
}


x1.6.boxplot(x1.6$weight)
































hist(x1.6$weight, 
     breaks = 15, 
     ann = FALSE,
     xlim = c(50,90),
     ylim = c(0, 15))

par(new=TRUE)

x1.6.boxplot <- function(vector){
  v <- boxplot(vector,
               horizontal = TRUE,  # горизонтальная ориентация диаграммы
               col = "cornsilk1",
               main = "Сводные характеристики 
               распределения веса участников соревнования               ",
               xlab = "вес, кг",
               ylab = 'соревнование 1',
               ylim = c(50,90),
               xlim = c(-2, 1.8)
  )
  abline(v = mean(vector), col = 'red', lwd = 1)
  text  (x = mean(vector), y = 1.5, 
         'среднее
арифметическое', 
         col = 'red')
  return(v)
}
x1.6.boxplot(x1.6$weight)

(sort(x1.6$weight[x1.6$weight>=64.20 & x1.6$weight<=71.70]))

as.numeric(quantile((x1.6$weight), 0.75)) - median(x1.6$weight)

median(x1.6$weight) - as.numeric(quantile((x1.6$weight), 0.25))






F.x <- 1/(x1.6.sd*sqrt(2*pi))*exp(-((x1.6$weight-mean(x1.6$weight))^2)/(2*x1.6.sd^2))
